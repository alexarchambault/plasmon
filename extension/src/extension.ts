import { env } from 'process'
import * as vscode from 'vscode'
import * as os from 'os'
import * as fs from 'fs'
import * as path from 'path'

import { CloseAction, DocumentSelector, ErrorAction, ErrorHandler, ExecuteCommandParams, ExecuteCommandRequest, ExitNotification, LanguageClient, LanguageClientOptions, Location, ServerOptions, Trace, integer } from 'vscode-languageclient/node'

// Many things here inspired by https://github.com/scalameta/metals-vscode/tree/423de8a8355a37d983b143508b66fa2ee5a10db0
// and earlier versions of it

let documentSelector: DocumentSelector = [
  {
    scheme: "file",
    language: "scala"
  },
  {
    scheme: "file",
    language: "java"
  }
]

function isSupportedLanguage(languageId: vscode.TextDocument["languageId"]): boolean {
  switch (languageId) {
    case "scala":
    case "sc":
    case "mill":
    case "java":
      return true
    default:
      return false
  }
}

interface PlasmonInitializationOptions {
  isLanguageStatusSupported?: boolean
}

function setupLanguageConfig(): void {

  function increaseIndentPattern(): RegExp {
    const old_if = /\b(if|while)\s+\([^\)]*?\)/
    const keywords_not_ending = /((?<!\bend\b\s*?)\b(if|while|for|match|try))/

    const keywords =
      /(\b(then|else|do|catch|finally|yield|return|throw))|=|=>|<-|=>>|:/
    const ending_spaces = /\s*?$/
    const extensionClause = /\s*extension\s*((\(|\[).*(\)|\]))+/

    const regexp = `(${extensionClause.source}|${keywords_not_ending.source}|${old_if.source}|${keywords.source})${ending_spaces.source}`
    return new RegExp(regexp)
  }

  vscode.languages.setLanguageConfiguration("scala", {
    indentationRules: {
      // ^(.*\*/)?\s*\}.*$
      decreaseIndentPattern: /^(.*\*\/)?\s*\}.*$/,
      // ^.*\{[^}"']*$
      increaseIndentPattern: /^.*\{[^}"']*$/,
    },
    wordPattern:
      /(-?\d*\.\d\w*)|([^\`\~\!\@\#\%\^\&\*\(\)\-\=\+\[\{\]\}\\\|\;\:\'\"\,\.\<\>\/\?\s]+)/g,
    onEnterRules: [
      {
        // e.g. /** | */
        beforeText: /^\s*\/\*\*(?!\/)([^\*]|\*(?!\/))*$/,
        afterText: /^\s*\*\/$/,
        action: { indentAction: vscode.IndentAction.IndentOutdent, appendText: " * " },
      },
      {
        // indent in places with optional braces
        beforeText: increaseIndentPattern(),
        action: { indentAction: vscode.IndentAction.Indent },
      },
      {
        // e.g. /** ...|
        beforeText: /^\s*\/\*\*(?!\/)([^\*]|\*(?!\/))*$/,
        action: { indentAction: vscode.IndentAction.None, appendText: " * " },
      },
      {
        // e.g.  * ...| Javadoc style
        beforeText: /^(\t|(\ \ ))*\ \*(\ ([^\*]|\*(?!\/))*)?$/,
        action: { indentAction: vscode.IndentAction.None, appendText: "* " },
      },
      {
        // e.g.  * ...| Scaladoc style
        beforeText: /^(\t|(\ \ ))*\*(\ ([^\*]|\*(?!\/))*)?$/,
        action: { indentAction: vscode.IndentAction.None, appendText: "* " },
      },
      {
        // e.g.  */|
        beforeText: /^(\t|(\ \ ))*\ \*\/\s*$/,
        action: { indentAction: vscode.IndentAction.None, removeText: 1 },
      },
      {
        // e.g.  *-----*/|
        beforeText: /^(\t|(\ \ ))*\ \*[^/]*\*\/\s*$/,
        action: { indentAction: vscode.IndentAction.None, removeText: 1 },
      },
      {
        // stop vscode from indenting automatically to last known indentation
        beforeText: /^\s*$/,
        /* we still want {} to be nicely split with two new lines into
         *{
         *  |
         *}
         */
        afterText: /[^\]\}\)]+/,
        action: { indentAction: vscode.IndentAction.None },
      },
    ],
  })
}

const RO_SCHEME = "plasmon"
class ReadonlyContentProvider implements vscode.TextDocumentContentProvider {
  onDidChange?: vscode.Event<vscode.Uri>

  constructor(private readonly getContent: (uri: vscode.Uri) => string | Promise<string>) {}

  provideTextDocumentContent(uri: vscode.Uri, _token: vscode.CancellationToken) {
    return this.getContent(uri)
  }
}

let client: LanguageClient | null = null
let clientSubscriptions: { dispose(): any }[] = []

let lastFocusedDocument: string | undefined = vscode.window.activeTextEditor?.document.uri.toString(true)
let logOutputChannels: { [key: string]: vscode.OutputChannel } = {}
let statusItems: { [key: string]: vscode.LanguageStatusItem } = {}
let statusBarItem: vscode.StatusBarItem | null = null

let plasmonServerChannel: vscode.OutputChannel | undefined = undefined
let traceChannel: vscode.OutputChannel | undefined = undefined

class Deferred<T> {
  promise: Promise<T>
  resolve!: (value: T | PromiseLike<T>) => void
  reject!: (reason?: any) => void

  constructor() {
    this.promise = new Promise<T>((res, rej) => {
      this.resolve = res
      this.reject = rej
    })
  }
}
let inProgressTasks: { [ids: string]: Deferred<void> } = {}

function plasmonStartingStatus(isRestart: boolean): void {
  if (statusBarItem) {
    statusBarItem.text = isRestart ? "Plasmon restarting $(loading~spin)" : "Plasmon starting $(loading~spin)"
    statusBarItem.backgroundColor = new vscode.ThemeColor('statusBarItem.warningBackground')
    statusBarItem.tooltip = undefined
    statusBarItem.command = "plasmon.show-process-log"
  }
}

function plasmonNoServerStatus(): void {
  if (statusBarItem) {
    statusBarItem.text = "Plasmon stopped"
    statusBarItem.backgroundColor = new vscode.ThemeColor('statusBarItem.errorBackground')
    statusBarItem.tooltip = undefined
  }
}

function plasmonFailedToStartStatus(): void {
  if (statusBarItem) {
    statusBarItem.text = "Plasmon failed to start"
    statusBarItem.backgroundColor = new vscode.ThemeColor('statusBarItem.errorBackground')
    statusBarItem.tooltip = undefined
    statusBarItem.command = "plasmon.show-process-log"
  }
}

function checkConcurrentServer(): boolean {
  let workspacePath = vscode.workspace.workspaceFolders?.[0].uri.fsPath
  if (workspacePath) {
    let lockFilePath = path.join(workspacePath, ".plasmon/lock")
    if (fs.existsSync(lockFilePath)) {
      let content = fs.readFileSync(lockFilePath, "utf-8")
      var pid = -1
      for (const line of content.split("\n"))
        if (line.startsWith("pid="))
          pid = parseInt(line.split("=")[1].trim())
      if (pid > 0) {
        console.log(`Found PID ${pid} in lock file ${lockFilePath}, checking if it's alive`)
        function isRunning(): boolean {
          try {
            process.kill(pid, 0)
            return true
          } catch {
            // ignored
          }
          return false
        }
        if (isRunning()) {
          vscode.window.showInformationMessage(
            `Another Plasmon process is running with PID ${pid}`,
            "Kill it",
            "Dismiss"
          ).then((elem) => {
            if (elem == "Kill it") {
              if (isRunning()) {
                console.log(`Interrupting process ${pid}`)
                process.kill(pid, "SIGINT")
                setTimeout(
                  () => {
                    if (isRunning()) {
                      console.log(`Process ${pid} still running, killing it for good`)
                      process.kill(pid, "SIGKILL")
                    }
                  },
                  2000
                )
              }
            }
          })
          return true
        }
      }
    }
  }

  return false
}

function createClient(
  context: vscode.ExtensionContext,
  serverOptions: ServerOptions,
  clientOptions: LanguageClientOptions,
  trace: boolean
): boolean {

  function clientSubscription<T extends { dispose(): any }>(disposable: T): T {
    clientSubscriptions.push(disposable)
    context.subscriptions.push(disposable)
    return disposable
  }

  if (client == null) {
    console.log("Starting Plasmon LSP server")

    let defaultErrorHandler: ErrorHandler | undefined = undefined
    clientOptions.errorHandler = {
      error(error, message, count) {
        if (defaultErrorHandler)
          return defaultErrorHandler.error(error, message, count)
        else
          return ErrorAction.Continue
      },
      closed() {
        // TODO Clear status stuff
        for (const [id, statusItem] of Object.entries(statusItems)) {
          let index = context.subscriptions.indexOf(statusItem)
          if (index >= 0)
            context.subscriptions.splice(index, 1)
          statusItem.dispose()
        }
        statusItems = {}
        let action = defaultErrorHandler ? defaultErrorHandler.closed() : CloseAction.Restart
        if (action == CloseAction.Restart)
          plasmonStartingStatus(true)
        else if (action == CloseAction.DoNotRestart)
          plasmonFailedToStartStatus()
        return action
      }
    }

    checkConcurrentServer()

    if (!plasmonServerChannel) {
      plasmonServerChannel = vscode.window.createOutputChannel("Plasmon process")
      context.subscriptions.push(plasmonServerChannel)
    }

    let client0 = new LanguageClient(
      "Plasmon process",
      serverOptions,
      {
        ...clientOptions,
        outputChannel: plasmonServerChannel
      }
    )
    defaultErrorHandler = client0.createDefaultErrorHandler(5 /* ??? */)
    client = client0
    clientSubscription(
      client.start()
    )

    plasmonStartingStatus(false)

    client.onReady().then(() => {

      if (client === client0) {

        if (trace)
          client0.trace = Trace.Verbose

        interface LogMessage {
          channelId: string
          channelLabel: string
          lines: string[]
        }
        clientSubscription(
          client0.onNotification(
            "plasmon/log",
            (msg: LogMessage) => {
              let channelId = `plasmon-${msg.channelId}`
              if (!logOutputChannels[channelId])
                logOutputChannels[channelId] = vscode.window.createOutputChannel(`Plasmon ${msg.channelLabel}`)

              let channel = logOutputChannels[channelId]
              for (const line of msg.lines)
                channel.appendLine(line)
            }
          )
        )

        interface StatusEntry {
          text: string
          id: string
          severity: integer
          busy: boolean
          detail: string | undefined
          command: vscode.Command | undefined
        }
        clientSubscription(
          client0.onNotification(
            "plasmon/statusUpdate",
            (uri: string, params: StatusEntry[]) => {
              if (uri === "")
                statusBarItem?.hide()
              else if (lastFocusedDocument) {
                checkStatusBarAndDocument()
                let uri0 = vscode.Uri.parse(uri).toString(true)
                if (lastFocusedDocument === uri0)
                  statusBarItem?.show()
                else {
                  console.log(`Warning: got status update for ${uri0}, while the last focused document is ${lastFocusedDocument}`)
                  client0.sendNotification(
                    "metals/didFocusTextDocument",
                    lastFocusedDocument
                  )
                }
              }
              for (const params0 of params) {
                checkStatusBarAndDocument()
                if (params0.id == 'plasmon.summary' && statusBarItem != null) {
                  let text = params0.text
                  if (params0.busy)
                    text = text + "  $(loading~spin)"
                  if (statusBarItem.text != text)
                    statusBarItem.text = text

                  let backgroundColor: vscode.ThemeColor | undefined = undefined
                  if (params0.severity == 1)
                    backgroundColor = new vscode.ThemeColor('statusBarItem.warningBackground')
                  else if (params0.severity == 2)
                    backgroundColor = new vscode.ThemeColor('statusBarItem.errorBackground')
                  else if (params0.severity != 0)
                    console.log(`Error: unrecognized severity value for status update: ${params0.severity}`)
                  if (statusBarItem.backgroundColor != backgroundColor)
                    statusBarItem.backgroundColor = backgroundColor

                  if (statusBarItem.tooltip != params0.detail)
                    statusBarItem.tooltip = params0.detail
                  if (statusBarItem.command != params0.command)
                    statusBarItem.command = params0.command
                }
                else if (params0.text) {
                  if (!(params0.id in statusItems))
                    statusItems[params0.id] = clientSubscription(
                      vscode.languages.createLanguageStatusItem(params0.id, documentSelector)
                    )
                  let statusItem = statusItems[params0.id]
                  if (statusItem.text != params0.text)
                    statusItem.text = params0.text
                  if (statusItem.severity != params0.severity)
                    statusItem.severity = params0.severity
                    if (statusItem.detail != params0.detail)
                    statusItem.detail = params0.detail
                  if (statusItem.busy != params0.busy)
                    statusItem.busy = params0.busy
                  if (statusItem.command != params0.command)
                    statusItem.command = params0.command
                }
                else if (params0.id in statusItems) {
                  let statusItem = statusItems[params0.id]
                  let index = context.subscriptions.indexOf(statusItem)
                  if (index >= 0)
                    context.subscriptions.splice(index, 1)
                  delete statusItems[params0.id]
                  statusItem.dispose()
                }
              }
              return;
            }
          )
        )

        clientSubscription(
          client0.onNotification(
            "metals/executeClientCommand",
            (params: ExecuteCommandParams) => {
              vscode.commands.executeCommand(params.command, ...(params.arguments ?? [])).then(
                () => {},
                (err) => {
                  console.log(`Error running command from server ${params.command} ${JSON.stringify(params.arguments)}: ${err}`)
                }
              )
            }
          )
        )

        clientSubscription(
          client0.onNotification(
            "plasmon/heartBeat",
            () => {
              vscode.commands.executeCommand("plasmon/clientHeartBeat").then(
                () => {},
                (err) => {
                  console.log(`Error running plasmon/clientHeartBeat command: ${err}`)
                }
              )
            }
          )
        )

        interface ProgressRequest {
          buildToolId: string
          buildToolName: string
          requestId: string
          request: string
          done: boolean
        }

        clientSubscription(
          client0.onNotification(
            "plasmon/progress",
            (details: ProgressRequest) => {
              const id = `${details.buildToolId}: ${details.requestId}`

              if (details.done) {
                if (id in inProgressTasks) {
                  const deferred = inProgressTasks[id]
                  deferred.resolve()
                  delete inProgressTasks[id]
                }
                else
                  console.log(`Warning: ${details.buildToolId} ${details.requestId} not found in in-progress tasks (${JSON.stringify(Object.keys(inProgressTasks))})`)
              }
              else {
                if (id in inProgressTasks)
                  console.log(`Warning: ${details.buildToolId} ${details.requestId} already in in-progress tasks`)
                else {
                  const deferred = new Deferred<void>()
                  inProgressTasks[id] = deferred
                  vscode.window.withProgress(
                    {
                      location: vscode.ProgressLocation.Notification,
                      title: `${details.buildToolName}: ${details.request}`,
                      cancellable: false
                    },
                    (progress, token) => {
                      return deferred.promise
                    }
                  )
                }
              }
            }
          )
        )

        interface BuildChangeDetails {

        }

        var always: "" | "Re-index" | "Dismiss" = ""
        clientSubscription(
          client0.onNotification(
            "plasmon/buildChangeDetected",
            (details: BuildChangeDetails) => {
              if (always == "Re-index")
                reIndex()
              else if (always != "Dismiss") {
                vscode.window.showInformationMessage(
                  "Build change detected",
                  {
                    modal: false
                  },
                  "Always re-index",
                  "Re-index",
                  "Dismiss",
                  "Dismiss all"
                ).then(
                  (elem) => {
                    if (elem == "Always re-index") {
                      always = "Re-index"
                    }
                    if (elem == "Dismiss all") {
                      always = "Dismiss"
                    }

                    if (elem == "Always re-index" || elem == "Re-index") {
                      reIndex()
                    }
                  },
                  (err) => {
                    console.log(`Error asking users to reload or not: ${err}`)
                  }
                )
              }
            }
          )
        )

        let uriStr = vscode.window.activeTextEditor?.document.uri.toString(true)
        if (uriStr) {
          if (!lastFocusedDocument)
            lastFocusedDocument = uriStr
          client.sendNotification("metals/didFocusTextDocument", lastFocusedDocument)
        }
      }
    })

    return true
  }
  else
    return false

}

function reIndex(): void {
  client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/index", arguments: [] }).then(
    () => {},
    (err) => {
      console.log(`Error when running plasmon/index command: ${err}`)
      vscode.window.showErrorMessage(`Error indexing project: ${err}`, { modal: false })
    }
  )
}

async function stopClient(context: vscode.ExtensionContext): Promise<void> {
  if (client != null) {
    console.log("Stopping Plasmon LSP server")
    let client0 = client
    if (client0) {
      try { await client0.stop(); } catch { /* swallow */ }
      clientSubscriptions.forEach(element => {
        element.dispose()
        let index = context.subscriptions.indexOf(element)
        if (index >= 0)
          context.subscriptions.splice(index, 1)
      })
      clientSubscriptions.splice(0, clientSubscriptions.length)
      console.log(`Notifying ${ExitNotification.type.method} to ${client0}`)
      // not sure why this doesn't get sent by stop…
      try {
        client0.sendNotification(ExitNotification.type)
      }
      catch (err) {
        if (!(err instanceof Error) || !err.message.includes("Language client is not ready yet"))
          throw err
      }
      for (const elem in inProgressTasks) {
        inProgressTasks[elem].resolve()
      }
      inProgressTasks = {}
      if (client === client0)
        client = null
    }
  }
}

let documents: { [key: string]: string } = {}

function checkStatusBarAndDocument() {
  if (vscode.window.activeTextEditor) {
    let document = vscode.window.activeTextEditor.document
    if (document?.uri.scheme == 'file') {
      if (isSupportedLanguage(document.languageId)) {
        // console.log(`onDidOpenTextDocument / onDidCloseTextDocument: Showing status for ${document.uri} ${document.languageId}`)
        statusBarItem?.show()
      }
      else {
        // console.log(`onDidOpenTextDocument / onDidCloseTextDocument: Hiding status for ${document.uri} ${document.languageId}`)
        statusBarItem?.hide()
      }
    }
  }
  else {
    console.log(`onDidOpenTextDocument / onDidCloseTextDocument: Hiding status (no open document)`)
    lastFocusedDocument = undefined
    statusBarItem?.hide()
  }
}

function loadBuildTool(discoverId: string, toolId: string, uri: string | undefined) {
  client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/loadBuildTool", arguments: [discoverId, toolId, uri] }).then(
    (resp) => {
      interface Resp {
        success: boolean
        error?: string
      }
      let resp0 = resp as Resp
      if (resp0.success) {
        console.log(`Ran plasmon/loadBuildTool ${discoverId} ${toolId}`)
        if (uri)
          loadBuildToolOrModule(uri, true)
      }
      else
        vscode.window.showErrorMessage(`Error loading build tool: ${resp0.error}`, { modal: false })
    },
    (err) => {
      vscode.window.showErrorMessage(`Error loading build tool: ${err}`, { modal: false })
    }
  )
}

// FIXME Quite some duplication with loadBuildToolOrModule below
function loadModuleOf(uri: string | undefined): void {
  console.log(`Sending listModulesOf ${uri}`)
  client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/listModulesOf", arguments: [uri] }).then(
    (values0) => {
      interface Entry {
        type: string
        label: string
        detail: string
        description: string
        alreadyLoaded?: boolean
        alreadyAdded?: boolean
      }

      let values: Entry[] = values0
      class Item implements vscode.QuickPickItem {
        entry: Entry
        label: string
        detail: string
        description: string
        iconPath?: vscode.ThemeIcon

        constructor(entry: Entry) {
          this.entry = entry
          this.label = entry.label
          this.detail = entry.detail
          this.description = entry.description

          if (entry.type == "BuildTool")
            this.iconPath = vscode.ThemeIcon.Folder
        }
      }
      interface Separator extends vscode.QuickPickItem {

      }
      let items: (Item | Separator)[] = []
      let lastItemType: string | undefined = undefined
      let lastAlreadyAdded: boolean | undefined = undefined
      let lastAlreadyLoaded: boolean | undefined = undefined
      for (let entry of values) {
        if (entry.type == 'Module') {
          let pushedSeparator = false

          if (lastItemType === undefined)
            lastItemType = entry.type
          else if (entry.type != lastItemType) {
            lastItemType = entry.type
            let sep: Separator = {
              label: "",
              kind: vscode.QuickPickItemKind.Separator
            }
            items.push(sep)
            pushedSeparator = true
          }

          if (lastAlreadyLoaded === undefined)
            lastAlreadyLoaded = entry.alreadyLoaded
          else if (entry.alreadyLoaded != lastAlreadyLoaded) {
            lastAlreadyLoaded = entry.alreadyLoaded
            if (!pushedSeparator) {
              let sep: Separator = {
                label: "",
                kind: vscode.QuickPickItemKind.Separator
              }
              items.push(sep)
              pushedSeparator = true
            }
          }

          if (lastAlreadyAdded === undefined)
            lastAlreadyAdded = entry.alreadyAdded
          else if (entry.alreadyAdded != lastAlreadyAdded) {
            lastAlreadyAdded = entry.alreadyAdded
            if (!pushedSeparator) {
              let sep: Separator = {
                label: "",
                kind: vscode.QuickPickItemKind.Separator
              }
              items.push(sep)
              pushedSeparator = true
            }
          }

          items.push(new Item(entry))
        }
      }
      let quickPick = vscode.window.createQuickPick<Item | Separator>()
      quickPick.items = items
      quickPick.onDidAccept(() => {
        for (const item of quickPick.activeItems) {
          if (item instanceof Item) {
            if (item.entry.type == 'Module') {
              interface ModuleEntry extends Entry {
                workspace: string
                server: string
                uri: string
                label: string
                alreadyLoaded: boolean
              }
              let entry = item.entry as ModuleEntry
              console.log(`User picked module ${entry.uri}`)
              if (entry.alreadyLoaded)
                client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/unloadModule", arguments: [entry.workspace, entry.server, entry.uri] }).then(
                  (res) => {

                  },
                  (err) => {
                    let msg = `Error while sending plasmon/unloadModule command: ${err}`
                    console.log(msg)
                    vscode.window.showErrorMessage(msg)
                  }
                )
              else
              client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/loadModule", arguments: [entry.workspace, entry.server, entry.uri] }).then(
                (res) => {

                },
                (err) => {
                  let msg = `Error while sending plasmon/loadModule command: ${err}`
                  console.log(msg)
                  vscode.window.showErrorMessage(msg)
                }
              )
            }
          }
        }
        quickPick.hide()
      })
      quickPick.show()
    },
    (err) => {
      let msg = `Error while sending plasmon/listModulesOf command for ${uri}: ${err}`
      vscode.window.showErrorMessage(msg, { modal: false })
      console.log(msg)
    }
  )
}

function loadBuildToolOrModule(uri: string | undefined, onlyModules: boolean): void {
  console.log(`Sending listBuildToolsOrModules ${uri}`)
  client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/listBuildToolsOrModules", arguments: [uri] }).then(
    (values0) => {
      interface Entry {
        type: string
        label: string
        detail: string
        description: string
        alreadyLoaded?: boolean
        alreadyAdded?: boolean
      }

      let values: Entry[] = values0
      class Item implements vscode.QuickPickItem {
        entry: Entry
        label: string
        detail: string
        description: string
        iconPath?: vscode.ThemeIcon

        constructor(entry: Entry) {
          this.entry = entry
          this.label = entry.label
          this.detail = entry.detail
          this.description = entry.description

          if (entry.type == "BuildTool")
            this.iconPath = vscode.ThemeIcon.Folder
        }
      }
      interface Separator extends vscode.QuickPickItem {

      }
      let items: (Item | Separator)[] = []
      let lastItemType: string | undefined = undefined
      let lastAlreadyAdded: boolean | undefined = undefined
      let lastAlreadyLoaded: boolean | undefined = undefined
      for (let entry of values) {
        if (entry.type != 'BuildTool' || !onlyModules) {
          let pushedSeparator = false

          if (lastItemType === undefined)
            lastItemType = entry.type
          else if (entry.type != lastItemType) {
            lastItemType = entry.type
            let sep: Separator = {
              label: "",
              kind: vscode.QuickPickItemKind.Separator
            }
            items.push(sep)
            pushedSeparator = true
          }

          if (lastAlreadyLoaded === undefined)
            lastAlreadyLoaded = entry.alreadyLoaded
          else if (entry.alreadyLoaded != lastAlreadyLoaded) {
            lastAlreadyLoaded = entry.alreadyLoaded
            if (!pushedSeparator) {
              let sep: Separator = {
                label: "",
                kind: vscode.QuickPickItemKind.Separator
              }
              items.push(sep)
              pushedSeparator = true
            }
          }

          if (lastAlreadyAdded === undefined)
            lastAlreadyAdded = entry.alreadyAdded
          else if (entry.alreadyAdded != lastAlreadyAdded) {
            lastAlreadyAdded = entry.alreadyAdded
            if (!pushedSeparator) {
              let sep: Separator = {
                label: "",
                kind: vscode.QuickPickItemKind.Separator
              }
              items.push(sep)
              pushedSeparator = true
            }
          }

          items.push(new Item(entry))
        }
      }
      let quickPick = vscode.window.createQuickPick<Item | Separator>()
      quickPick.items = items
      quickPick.onDidAccept(() => {
        for (const item of quickPick.activeItems) {
          if (item instanceof Item) {
            if (item.entry.type == 'BuildTool') {
              interface BuildToolEntry extends Entry {
                id: string
                discoverId: string
                alreadyAdded: boolean
              }
              let entry = item.entry as BuildToolEntry
              console.log(entry.id)
              if (entry.alreadyAdded)
                client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/unloadBuildTool", arguments: [entry.discoverId, entry.id, uri] }).then(
                  (resp) => {
                    interface Resp {
                      success: boolean
                      error?: string
                    }
                    let resp0 = resp as Resp
                    if (resp0.success)
                      console.log(`Ran plasmon/unloadBuildTool ${entry.discoverId} ${entry.id}`)
                    else
                      vscode.window.showErrorMessage(`Error unloading build tool: ${resp0.error}`, { modal: false })
                  },
                  (err) => {
                    vscode.window.showErrorMessage(`Error unloading build tool: ${err}`, { modal: false })
                  }
                )
              else
                loadBuildTool(entry.discoverId, entry.id, uri?.toString())
            }
            else if (item.entry.type == 'Module') {
              interface ModuleEntry extends Entry {
                workspace: string
                server: string
                uri: string
                label: string
                alreadyLoaded: boolean
              }
              let entry = item.entry as ModuleEntry
              console.log(`User picked module ${entry.uri}`)
              if (entry.alreadyLoaded)
                client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/unloadModule", arguments: [entry.workspace, entry.server, entry.uri] }).then(
                  (res) => {

                  },
                  (err) => {
                    let msg = `Error while sending plasmon/unloadModule command: ${err}`
                    console.log(msg)
                    vscode.window.showErrorMessage(msg)
                  }
                )
              else
              client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/loadModule", arguments: [entry.workspace, entry.server, entry.uri] }).then(
                (res) => {

                },
                (err) => {
                  let msg = `Error while sending plasmon/loadModule command: ${err}`
                  console.log(msg)
                  vscode.window.showErrorMessage(msg)
                }
              )
            }
            else {
              // ???
            }
          }
        }
        quickPick.hide()
      })
      quickPick.show()
    },
    (err) => {
      let msg = `Error while sending plasmon/listBuildToolsOrModules command for ${uri}: ${err}`
      vscode.window.showErrorMessage(msg, { modal: false })
      console.log(msg)
    }
  )
}

export function activate(context: vscode.ExtensionContext) {

  const provider = new ReadonlyContentProvider((uri) => {
    let content = documents[uri.toString(true)]
    // return `Hello from a read-only doc!\nURI: ${uri.toString(true)}\n\n- Generated at: ${new Date().toISOString()}`
    delete documents[uri.toString(true)]
    return content
  })

  context.subscriptions.push(
    vscode.workspace.registerTextDocumentContentProvider(RO_SCHEME, provider)
  )

  const plasmonConfig = vscode.workspace.getConfiguration("plasmon")

  setupLanguageConfig()

  let serverArgs = ["server"]

  function binaryServerOptions(binary: string, shell: boolean | undefined = undefined): ServerOptions {
    let shell0 = shell
    if (process.platform === "win32" && shell0 === undefined)
      shell0 = true
    return {
      command: binary,
      args: serverArgs,
      options: {
        // cwd: "",
        // env: {},
        // detached: false
        shell: shell0
      }
    }
  }

  function jvmServerOptions(version: string): ServerOptions {
    return {
      command: "cs",
      args: ["launch", `io.github.alexarchambault.plasmon:server_3:${version}`, "--jvm", "24", "--"].concat(serverArgs),
      options: {}
    }
  }

  let serverOptions: ServerOptions

  if (env["PLASMON_FORCED_BINARY"]) {
    console.log(`Using plasmon binary from PLASMON_FORCED_BINARY`)
    serverOptions = binaryServerOptions(env["PLASMON_FORCED_BINARY"])
  }
  else if (env["PLASMON_FORCED_VERSION"]) {
    console.log(`Using plasmon version from PLASMON_FORCED_VERSION`)
    serverOptions = jvmServerOptions(env["PLASMON_FORCED_VERSION"])
  }
  else {
    let binaryFromConfig = plasmonConfig.get<string>("binary")
    if (binaryFromConfig !== undefined) {
      console.log(`Using plasmon binary from config`)
      serverOptions = binaryServerOptions(binaryFromConfig)
    }
    else if (plasmonConfig.get<boolean>("useJvm") ?? false) {
      console.log(`Using plasmon JVM per config`)
      serverOptions = jvmServerOptions("0.1.0-SNAPSHOT")
    }
    else {
      console.log(`Using default plasmon: binary`)
      serverOptions = binaryServerOptions('plasmon')
    }
  }

  let initializationOptions: PlasmonInitializationOptions = {
    isLanguageStatusSupported: true
  }

  let clientOptions: LanguageClientOptions = {
    documentSelector: documentSelector,
    initializationOptions
  }

  createClient(context, serverOptions, clientOptions, false)

  statusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 50)
  statusBarItem.text = 'Plasmon starting $(loading~spin)'
  statusBarItem.tooltip = 'The tooltip'
  statusBarItem.backgroundColor = new vscode.ThemeColor('statusBarItem.warningBackground')
  statusBarItem.command = "plasmon.show-process-log"
  context.subscriptions.push(statusBarItem)
  statusBarItem.show()

  context.subscriptions.push(
    vscode.commands.registerCommand('plasmon.debug-pc-enable', async () => {
      interface Resp {
        enabled: boolean
        changed: boolean
        logPath: string
      }
      client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/debugPresentationCompiler", arguments: [true] }).then(
        async (resp: Resp) => {
          console.log(`Response of debugPresentationCompiler: ${JSON.stringify(resp)}`)
          if (resp.changed)
            vscode.window.showInformationMessage(`Enabled debugging of presentation compiler. See logs in ${resp.logPath}`)
          else
            vscode.window.showWarningMessage(`Debugging of presentation compiler was already enabled. See logs in ${resp.logPath}`)
        },
        (err) => {
          let msg = `Error while sending plasmon/debugPresentationCompiler command: ${err}`
          console.log(msg)
          vscode.window.showErrorMessage(msg)
        }
      )
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand('plasmon.debug-pc-disable', async () => {
      interface Resp {
        enabled: boolean
        changed: boolean
        logPath: string
      }
      client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/debugPresentationCompiler", arguments: [false] }).then(
        async (resp: Resp) => {
          console.log(`Response of debugPresentationCompiler: ${JSON.stringify(resp)}`)
          if (resp.changed)
            vscode.window.showInformationMessage(`Disabled debugging of presentation compiler`)
          else
            vscode.window.showWarningMessage(`Debugging of presentation compiler was already disabled`)
        },
        (err) => {
          let msg = `Error while sending plasmon/debugPresentationCompiler command: ${err}`
          console.log(msg)
          vscode.window.showErrorMessage(msg)
        }
      )
    })
  )

  function buildTooltip(): vscode.MarkdownString {
    const md = new vscode.MarkdownString(undefined, true);
    md.isTrusted = true;                // allow command: links
    md.supportHtml = true;              // optional; basic HTML allowed
    md.supportThemeIcons = true;        // allow $(icon) in markdown

    // Helper to pass args to commands
    const withArgs = (cmd: string, args: unknown) =>
      `command:${cmd}?${encodeURIComponent(JSON.stringify(args))}`;

    md.appendMarkdown([
      '### $(sparkle) MyTool',
      'Status: `indexing` $(sync~spin) 42%',
      '',
      '---',
      '**Show logs**',
      '',
      `- [$(play) Requests](${withArgs('plasmon.show-log', 'request')})`,
      `- [$(cloud-upload) Indexer]( ${withArgs('plasmon.show-log', 'indexer')} )`,
      `- [$(circle-large-outline) LSP requests](${withArgs('plasmon.show-log', 'lsp-requests')})`,
      '',
      '---',
      '**Tips**',
      '',
      '- Click the status bar to open the control panel.',
      '- Links above execute commands directly.',
    ].join('\n'));

    return md;
  }

  context.subscriptions.push(
    vscode.commands.registerCommand('plasmon.testFancyStatusTooltip', () => {
      const item = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 1000);
      item.name = 'MyTool';
      item.text = '$(sparkle) MyTool';
      item.tooltip = buildTooltip();
      item.command = 'mytool.open';
      item.show();
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand('plasmon.addDirAsScalaCliProject', (uri: vscode.Uri) => {
      loadBuildTool("scala-cli", "scala-cli", uri.toString())
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand('plasmon.addFileAsScalaCliProject', (uri: vscode.Uri) => {
      loadBuildTool("scala-cli", "scala-cli", uri.toString())
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand('plasmon.loadMillProjectFromFile', (uri: vscode.Uri) => {
      loadBuildTool("mill", "mill", uri.toString())
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand('plasmon.testOpenReadonly', async () => {
      const uri = vscode.Uri.parse(`${RO_SCHEME}:/welcome.md`)
      const doc = await vscode.workspace.openTextDocument(uri)
      await vscode.window.showTextDocument(doc, { preview: false })
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand("plasmon.show-log", (logName) => {
      let logName0 = `plasmon-${logName}`
      let channel = logOutputChannels[logName0]
      if (channel)
        channel.show(true)
      else
        console.log(`Unknown channel: ${logName0} (available channels: ${Object.keys(logOutputChannels)})`)
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand("plasmon.show-process-log", () => {
      let channel = client?.outputChannel
      if (channel)
        channel.show(true)
      else
        console.log(`Client unavailable, cannot show its output channel`)
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand("plasmon.check-concurrent-server", () => {
      if (!checkConcurrentServer())
        vscode.window.showInformationMessage("No concurrent Plasmon process found")
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand("plasmon.show-doc", async (docName, docContent, docLanguage) => {
      const strUri = `${RO_SCHEME}:/${docName}`
      documents[strUri] = docContent
      const doc = await vscode.workspace.openTextDocument(vscode.Uri.parse(strUri))
      await vscode.window.showTextDocument(doc, { preview: false })
      if (docLanguage)
        await vscode.languages.setTextDocumentLanguage(doc, docLanguage)
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand(`plasmon.dump-symbol-index`, () => {
      interface Resp {
        targetId: string
        content: string
        error: string
      }
      let uri = lastFocusedDocument
      client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/debugSymbolIndex", arguments: [uri] }).then(
        async (resp: Resp) => {
          console.log(`Response of debugSymbolIndex: ${JSON.stringify(resp)}`)
          if (resp.error) {
            vscode.window.showErrorMessage(`Error getting symbol index: ${resp.error}`, { modal: false })
          }
          else {
            const strUri = `${RO_SCHEME}:/symbol-index-${resp.targetId}`
            documents[strUri] = resp.content
            const doc = await vscode.workspace.openTextDocument(vscode.Uri.parse(strUri))
            await vscode.window.showTextDocument(doc, { preview: false })
          }
        },
        (err) => {
          let msg = `Error while sending plasmon/debugSymbolIndex command for ${uri}: ${err}`
          console.log(msg)
          vscode.window.showErrorMessage(msg)
        }
      )
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand(`plasmon.dump-typed-tree`, () => {
      interface Resp {
        targetId: string
        fullTree: string
        diagnostics: string
        error: string
      }
      let uri = lastFocusedDocument
      client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/debugFullTree", arguments: [uri] }).then(
        async (resp: Resp) => {
          console.log(`Response of debugFullTree: ${JSON.stringify(resp)}`)
          if (resp.error) {
            vscode.window.showErrorMessage(`Error getting typed tree: ${resp.error}`, { modal: false })
          }
          else {
            const strUri = `${RO_SCHEME}:${uri}/typed-tree.scala`
            documents[strUri] = resp.fullTree + os.EOL + os.EOL + "/*" + os.EOL + os.EOL + resp.diagnostics + os.EOL + os.EOL + "*/"
            const doc = await vscode.workspace.openTextDocument(vscode.Uri.parse(strUri))
            await vscode.window.showTextDocument(doc, { preview: false })
          }
        },
        (err) => {
          let msg = `Error while sending plasmon/debugFullTree command for ${uri}: ${err}`
          console.log(msg)
          vscode.window.showErrorMessage(msg)
        }
      )
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand(`plasmon.dump-bsp-data`, () => {
      interface Resp {
        targetId: string
        data: string
        error: string
      }
      let uri = lastFocusedDocument
      client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/debugBspData", arguments: [uri] }).then(
        async (resp: Resp) => {
          console.log(`Response of debugBspData: ${JSON.stringify(resp)}`)
          if (resp.error) {
            vscode.window.showErrorMessage(`Error getting BSP data: ${resp.error}`, { modal: false })
          }
          else {
            const strUri = `${RO_SCHEME}:${uri}/bsp-data`
            documents[strUri] = resp.data
            const doc = await vscode.workspace.openTextDocument(vscode.Uri.parse(strUri))
            await vscode.window.showTextDocument(doc, { preview: false })
          }
        },
        (err) => {
          let msg = `Error while sending plasmon/debugBspData command for ${uri}: ${err}`
          console.log(msg)
          vscode.window.showErrorMessage(msg)
        }
      )
    })
  )

  function dumpSemdbDetails(detailed: boolean) {
    interface Resp {
      targetId: string
      text: string
      error: string
    }
    let uri = lastFocusedDocument
    client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/debugSemanticdbLookup", arguments: [uri, detailed] }).then(
      async (resp: Resp) => {
        console.log(`Response of debugSemanticdbLookup: ${JSON.stringify(resp)}`)
        if (resp.error) {
          vscode.window.showErrorMessage(`Error getting semanticdb details: ${resp.error}`, { modal: false })
        }
        else {
          let name = detailed ? "semanticdb-details" : "semanticdb-compact-details"
          const strUri = `${RO_SCHEME}:${uri}/${name}`
          documents[strUri] = resp.text
          const doc = await vscode.workspace.openTextDocument(vscode.Uri.parse(strUri))
          await vscode.window.showTextDocument(doc, { preview: false })
        }
      },
      (err) => {
        let msg = `Error while sending plasmon/debugSemanticdbLookup command for ${uri}: ${err}`
        console.log(msg)
        vscode.window.showErrorMessage(msg)
      }
    )
  }

  function dumpServerState() {
    interface Resp {
      text: string
      id: string
    }
    let uri = lastFocusedDocument
    client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/debugServerState" }).then(
      async (resp: Resp) => {
        console.log(`Response of debugServerState: ${JSON.stringify(resp)}`)
        const strUri = `${RO_SCHEME}:server-state.json?id=${resp.id}`
        documents[strUri] = resp.text
        const doc = await vscode.workspace.openTextDocument(vscode.Uri.parse(strUri))
        await vscode.window.showTextDocument(doc, { preview: false })
      },
      (err) => {
        let msg = `Error while sending plasmon/debugServerState command for ${uri}: ${err}`
        console.log(msg)
        vscode.window.showErrorMessage(msg)
      }
    )
  }

  context.subscriptions.push(
    vscode.commands.registerCommand(`plasmon.dump-compact-semanticdb-details`, () => {
      dumpSemdbDetails(false)
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand(`plasmon.dump-semanticdb-details`, () => {
      dumpSemdbDetails(true)
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand(`plasmon.dump-server-state`, () => {
      dumpServerState()
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand("plasmon.module-actions", () => {
      let uri = lastFocusedDocument
      client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/listModuleActions", arguments: [uri] }).then(
        (values0) => {
          interface Entry {
            label: string
            detail: string
            command: string
            arguments: string[]
          }
          let values: Entry[] = values0
          class Item implements vscode.QuickPickItem {
            entry: Entry
            label: string
            detail: string

            constructor(entry: Entry) {
              this.entry = entry
              this.label = entry.label
              this.detail = entry.detail
            }
          }
          let items: Item[] = []
          for (let entry of values)
            items.push(new Item(entry))
          let quickPick = vscode.window.createQuickPick<Item>()
          quickPick.items = items
          quickPick.onDidAccept(() => {
            for (const item of quickPick.activeItems) {
              let entry = item.entry
              console.log(entry)
              client?.sendRequest(ExecuteCommandRequest.type, { command: entry.command, arguments: entry.arguments }).then(
                () => {
                  console.log(`Ran ${entry.command} ${JSON.stringify(entry.arguments)}`)
                },
                (err) => {
                  let msg = `Error while sending ${entry.command} ${JSON.stringify(entry.arguments)} command: ${err}`
                  console.log(msg)
                  vscode.window.showErrorMessage(msg)
                }
              )
            }
            quickPick.hide()
          })
          quickPick.show()
        },
        (err) => {
          let msg = `Error while sending plasmon/listModuleActions command for ${uri}: ${err}`
          console.log(msg)
          vscode.window.showErrorMessage(msg)
        }
      )
    })
  )

  async function foo(count: number, progress: vscode.Progress<{
    message?: string;
    increment?: number;
  }>, token: vscode.CancellationToken): Promise<void> {
    let i = 0
    while (i < count) {
      let p = new Promise<void>(resolve => setTimeout(resolve, 1000))
      await p
      if (token.isCancellationRequested)
        return
      i += 1
      progress.report({ message: `Hello ${i}`, increment: 100.0 / count })
    }
  }

  context.subscriptions.push(
    vscode.commands.registerCommand('plasmon.test-progress', () => {
      vscode.window.withProgress(
        {
          location: vscode.ProgressLocation.Notification,
          title: "The progress",
          cancellable: true
        },
        (progress, token) => {
          token.isCancellationRequested
          return foo(10, progress, token)
        }
      )
    })
  )

  // context.subscriptions.push(
  //   vscode.commands.registerCommand(`plasmon.test-error`, () => {
  //     vscode.window.showErrorMessage(
  //       "Hello error",
  //       { modal: false, detail: "Detailsss" },
  //       { title: "a" },
  //       { title: "b" },
  //       { title: "c", isCloseAffordance: true }
  //     ).then(
  //       (args) => {
  //         console.log(`Got args: ${JSON.stringify(args)}`)
  //       },
  //       (err) => {
  //         console.log(`Got error: ${JSON.stringify(err)}`)
  //       }
  //     )
  //   })
  // )

  //
  // User commands (commands that can be run manually by users)
  //

  context.subscriptions.push(
    vscode.commands.registerCommand(`plasmon.load-build-tool`, () => {
      let uri = lastFocusedDocument
      client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/listBuildTools", arguments: [uri] }).then(
        (values0) => {
          interface Entry {
            type: string
            label: string
            detail: string
            alreadyLoaded?: boolean
            alreadyAdded?: boolean
          }
          let values: Entry[] = values0
          class Item implements vscode.QuickPickItem {
            entry: Entry
            label: string
            detail: string
            iconPath: vscode.ThemeIcon

            constructor(entry: Entry) {
              this.entry = entry
              this.label = entry.label
              this.detail = entry.detail
              this.iconPath = vscode.ThemeIcon.Folder
            }
          }
          interface Separator extends vscode.QuickPickItem {

          }
          let items: (Item | Separator)[] = []
          let lastAlreadyAdded: boolean | undefined = undefined
          let lastAlreadyLoaded: boolean | undefined = undefined
          for (let entry of values) {
            let pushedSeparator = false

            console.log(`lastAlreadyLoaded = ${lastAlreadyLoaded}`)
            console.log(`entry.alreadyLoaded = ${entry.alreadyLoaded}`)
            if (lastAlreadyLoaded === undefined)
              lastAlreadyLoaded = entry.alreadyLoaded
            else if (entry.alreadyLoaded != lastAlreadyLoaded) {
              lastAlreadyLoaded = entry.alreadyLoaded
              if (!pushedSeparator) {
                let sep: Separator = {
                  label: "",
                  kind: vscode.QuickPickItemKind.Separator
                }
                items.push(sep)
                pushedSeparator = true
              }
            }

            if (lastAlreadyAdded === undefined)
              lastAlreadyAdded = entry.alreadyAdded
            else if (entry.alreadyAdded != lastAlreadyAdded) {
              lastAlreadyAdded = entry.alreadyAdded
              if (!pushedSeparator) {
                let sep: Separator = {
                  label: "",
                  kind: vscode.QuickPickItemKind.Separator
                }
                items.push(sep)
                pushedSeparator = true
              }
            }

            items.push(new Item(entry))
          }
          let quickPick = vscode.window.createQuickPick<Item | Separator>()
          quickPick.items = items
          quickPick.onDidAccept(() => {
            for (const item of quickPick.activeItems) {
              if (item instanceof Item) {
                interface BuildToolEntry extends Entry {
                  id: string
                  discoverId: string
                  alreadyAdded: boolean
                }
                let entry = item.entry as BuildToolEntry
                console.log(entry.id)
                if (entry.alreadyAdded)
                  client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/unloadBuildTool", arguments: [entry.discoverId, entry.id, uri] }).then(
                    (resp) => {
                      interface Resp {
                        success: boolean
                        error?: string
                      }
                      let resp0 = resp as Resp
                      if (resp0.success)
                        console.log(`Ran plasmon/unloadBuildTool ${entry.discoverId} ${entry.id}`)
                      else
                        vscode.window.showErrorMessage(`Error unloading build tool: ${resp0.error}`, { modal: false })
                    },
                    (err) => {
                      vscode.window.showErrorMessage(`Error unloading build tool: ${err}`, { modal: false })
                    }
                  )
                else
                  loadBuildTool(entry.discoverId, entry.id, uri?.toString())
              }
            }
            quickPick.hide()
          })
          quickPick.show()
        },
        (err) => {
          let msg = `Error while sending plasmon/listBuildTools command for ${uri}: ${err}`
          console.log(msg)
          vscode.window.showErrorMessage(msg)
        }
      )
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand(`plasmon.load-build-tool-or-module`, () => {
      loadBuildToolOrModule(lastFocusedDocument, false)
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand(`plasmon.build-tool-restart`, () => {
      let uri = lastFocusedDocument
      interface Resp {
        error?: string
      }
      client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/buildToolRestart", arguments: [uri] }).then(
        (resp: Resp) => {
          if (resp.error)
            vscode.window.showErrorMessage(`Error restarting / reconnecting to build server: ${resp.error}`, { modal: false })
          else
            vscode.window.showInformationMessage(`Restarted / reconnected to build server`, { modal: false })
        },
        (err) => {
          console.log(`Error restarting / reconnecting to build server for ${uri}: ${err}`)
          vscode.window.showErrorMessage(`Error restarting / reconnecting to build server: ${err}`, { modal: false })
        }
      )
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand(`plasmon.build-tool-restart-or-reconnect`, () => {
      let uri = lastFocusedDocument
      interface Resp {
        error?: string
      }
      client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/buildToolRestartOrReconnect", arguments: [uri] }).then(
        (resp: Resp) => {
          if (resp.error)
            vscode.window.showErrorMessage(`Error restarting / reconnecting to build server: ${resp.error}`, { modal: false })
          else
            vscode.window.showInformationMessage(`Restarted / reconnected to build server`, { modal: false })
        },
        (err) => {
          console.log(`Error restarting / reconnecting to build server for ${uri}: ${err}`)
          vscode.window.showErrorMessage(`Error restarting / reconnecting to build server: ${err}`, { modal: false })
        }
      )
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand(`plasmon.unload-all-modules`, () => {
      client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/unloadAllModules" }).then(
        () => {},
        (err) => {
          console.log(`Error unloading all modules: ${err}`)
          vscode.window.showErrorMessage(`Error unloading all modules: ${err}`, { modal: false })
        }
      )
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand(`plasmon.unload-all-build-tools`, () => {
      client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/unloadAllBuildTools" }).then(
        () => {},
        (err) => {
          console.log(`Error unloading all build tools: ${err}`)
          vscode.window.showErrorMessage(`Error unloading all build tools: ${err}`, { modal: false })
        }
      )
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand(`plasmon.re-index`, () => {
      reIndex()
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand(`plasmon.interactive-shutdown`, () => {
      let uri = lastFocusedDocument
      interface Response {
        removed: boolean
      }
      client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/interactiveCompilerRemove", arguments: ["path:" + uri, ""] }).then(
        (resp: Response) => {
          if (resp.removed)
            vscode.window.showInformationMessage(`Interactive compiler was restarted`)
          else
            vscode.window.showInformationMessage(`No interactive compiler to restart`)
        },
        (err) => {
          console.log(`Error shutting down interactive compiler for ${uri}: ${err}`)
          vscode.window.showErrorMessage(`Error shutting down interactive compiler: ${err}`, { modal: false })
        }
      )
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand(`plasmon.completion-interactive-shutdown`, () => {
      let uri = lastFocusedDocument
      interface Response {
        removed: boolean
      }
      client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/interactiveCompilerRemove", arguments: ["path:" + uri, "completion"] }).then(
        (resp: Response) => {
          if (resp.removed)
            vscode.window.showInformationMessage(`Interactive compiler for completion was restarted`)
          else
            vscode.window.showInformationMessage(`No interactive compiler for completion to restart`)
        },
        (err) => {
          console.log(`Error shutting down completion interactive compiler for ${uri}: ${err}`)
          vscode.window.showErrorMessage(`Error shutting down completion interactive compiler: ${err}`, { modal: false })
        }
      )
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand(`plasmon.interactive-interrupt`, () => {
      let uri = lastFocusedDocument
      client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/interactiveCompilerInterrupt", arguments: ['path:' + uri, ""] }).then(
        () => {},
        (err) => {
          console.log(`Error interrupting interactive compiler for ${uri}: ${err}`)
          vscode.window.showErrorMessage(`Error interrupting interactive compiler: ${err}`, { modal: false })
        }
      )
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand(`plasmon.completion-interactive-interrupt`, () => {
      let uri = lastFocusedDocument
      client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/interactiveCompilerInterrupt", arguments: ['path:' + uri, "completion"] }).then(
        () => {},
        (err) => {
          console.log(`Error interrupting completion interactive compiler for ${uri}: ${err}`)
          vscode.window.showErrorMessage(`Error interrupting completion interactive compiler: ${err}`, { modal: false })
        }
      )
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand(`plasmon.interactive-compiler`, () => {
      interface Action {
        command: string
        commandArgs: string[]
        text: string,
        id: string,
        description: string | undefined
      }
      let uri = lastFocusedDocument
      client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/interactiveCompilerActions", arguments: [uri] }).then(
        (actions: Action[]) => {
          class Item implements vscode.QuickPickItem {
            action: Action
            label: string
            detail: string | undefined

            constructor(action: Action) {
              this.action = action
              this.label = action.text
              this.detail = action.description
            }
          }
          let items: Item[] = []
          for (let action of actions)
            items.push(new Item(action))
          let quickPick = vscode.window.createQuickPick<Item>()
          quickPick.items = items
          quickPick.onDidAccept(() => {
            for (const item of quickPick.activeItems) {
              let action = item.action
              client?.sendRequest(ExecuteCommandRequest.type, { command: action.command, arguments: action.commandArgs }).then(
                () => {
                  console.log(`Ran ${action.command} ${action.commandArgs}`)
                },
                (err) => {
                  let msg = `Error while sending ${action.command} command: ${err}`
                  console.log(msg)
                  vscode.window.showErrorMessage(msg)
                }
              )
            }
            quickPick.hide()
          })
          quickPick.show()
        },
        (err) => {
          let msg = `Error while sending plasmon/interactiveCompilerActions command for ${uri}: ${err}`
          console.log(msg)
          vscode.window.showErrorMessage(msg)
        }
      )
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand(`plasmon.enable-pc-diagnostics`, () => {
      let uri = lastFocusedDocument
      client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/enablePcDiagnostics", arguments: [true] }).then(
        () => {},
        (err) => {
          console.log(`Error running command plasmon/enablePcDiagnostics(true): ${err}`)
          vscode.window.showErrorMessage(`Error enabling presentation compiler diagnostics: ${err}`, { modal: false })
        }
      )
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand(`plasmon.disable-pc-diagnostics`, () => {
      let uri = lastFocusedDocument
      client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/enablePcDiagnostics", arguments: [false] }).then(
        () => {},
        (err) => {
          console.log(`Error running command plasmon/enablePcDiagnostics(false): ${err}`)
          vscode.window.showErrorMessage(`Error disabling presentation compiler diagnostics: ${err}`, { modal: false })
        }
      )
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand(`plasmon.toggle-pc-diagnostics`, () => {
      let uri = lastFocusedDocument
      client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/togglePcDiagnostics" }).then(
        () => {},
        (err) => {
          console.log(`Error running command plasmon/togglePcDiagnostics: ${err}`)
          vscode.window.showErrorMessage(`Error toggling presentation compiler diagnostics: ${err}`, { modal: false })
        }
      )
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand("plasmon.compile", () => {
      let uri = lastFocusedDocument
      client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/compile", arguments: [uri] }).then(
        () => {},
        (err) => {
          console.log(`Failed to compile ${uri}: ${JSON.stringify(err)}`)
          vscode.window.showErrorMessage(`Compilation request failed: ${err}`)
        }
      )
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand("plasmon.clean", () => {
      let uri = lastFocusedDocument
      client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/clean", arguments: [uri] }).then(
        () => {
          // nothing to do
        },
        (err) => {
          console.log(`Failed to clean-up module of ${uri}: ${JSON.stringify(err)}`)
          vscode.window.showErrorMessage(`Clean-up request failed: ${err}`)
        }
      )
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand("plasmon.compile-all", () => {
      client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/compileAll" }).then(
        () => {},
        (err) => {
          console.log(`Failed to compile all modules: ${JSON.stringify(err)}`)
          vscode.window.showErrorMessage(`Compile-all request failed: ${err}`)
        }
      )
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand("plasmon.clean-all", () => {
      client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/cleanAll" }).then(
        () => {
          // nothing to do
        },
        (err) => {
          console.log(`Failed to clean-up all modules: ${JSON.stringify(err)}`)
          vscode.window.showErrorMessage(`Clean-up-all request failed: ${err}`)
        }
      )
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand("plasmon.re-import", () => {
      let uri = lastFocusedDocument
      interface Response {
        message?: string
        logId?: string
        error?: string
      }
      client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/reImport", arguments: [uri] }).then(
        (resp: Response) => {
          if (resp.error) {
            vscode.window.showErrorMessage(`Error re-importing build: ${resp.error}`)
          }
          else if (resp.message) {
            if (resp.logId)
              vscode.window.showInformationMessage(resp.message, {}, { title: "Show log", showLog: true }).then(
                (elem) => {
                  if (elem?.showLog && resp.logId) {
                    let logId = `plasmon-${resp.logId}`
                    let channel = logOutputChannels[logId]
                    if (channel)
                      channel.show(true)
                    else
                      console.log(`Unknown channel: ${logId} (available channels: ${Object.keys(logOutputChannels)})`)
                  }
                },
                (err) => {
                  console.log(`Error showing re-import dialog: ${err}`)
                }
              )
            else
              vscode.window.showInformationMessage(resp.message)
          }
          else {
            console.log(`Error re-import build: unexpected response shape (${JSON.stringify(resp)})`)
          }
        },
        (err) => {
          console.log(`Failed to re-import project of ${uri}: ${JSON.stringify(err)}`)
          vscode.window.showErrorMessage(`Re-import request failed: ${err}`)
        }
      )
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand("plasmon.stop-server", () => {
      let maybePromise = stopClient(context)
      if (maybePromise) {
        maybePromise.then(
          () => {
            vscode.window.showInformationMessage(`Plasmon LSP server was stopped`)
            plasmonNoServerStatus()
          },
          (err) => {
            console.log(`Failed to stop Plasmon LSP server: ${err}`)
            vscode.window.showErrorMessage(`Failed to stop Plasmon LSP server: ${err}`)
          }
        )
      }
      else
        vscode.window.showInformationMessage(`Plasmon LSP server isn't running`)
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand("plasmon.start-server", () => {
      let created = createClient(context, serverOptions, clientOptions, false)
      if (created)
        vscode.window.showInformationMessage(`Plasmon LSP server started`)
      else
        vscode.window.showInformationMessage(`Plasmon LSP server is already running`)
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand("plasmon.start-server-with-traces", () => {
      if (!traceChannel) {
        traceChannel = vscode.window.createOutputChannel("Plasmon JSON-RPC trace")
        context.subscriptions.push(traceChannel)
      }
      let created = createClient(
        context,
        serverOptions,
        {
          ...clientOptions,
          traceOutputChannel: traceChannel
        },
        true
      )
      if (created) {
        vscode.window.showInformationMessage(`Plasmon LSP server started with JSON-RPC traces`)
        client?.traceOutputChannel.show(true)
        // traceChannel.show(true)
      }
      else
        vscode.window.showInformationMessage(`Plasmon LSP server is already running`)
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand("plasmon.organize-imports", () => {
      let uri = lastFocusedDocument
      interface Response {
        error?: string
        fileChanged: boolean
      }
      client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/organizeImports", arguments: [uri] }).then(
        (resp: Response) => {
          if (resp.error) {
            console.log(`Error organizing imports: ${resp.error}`)
            vscode.window.showErrorMessage(`Error organizing imports: ${resp.error}`)
          }
          else if (resp.fileChanged) {
            vscode.window.showInformationMessage(`Organized imports`)
          }
          else {
            vscode.window.showInformationMessage(`Imports were already organized`)
          }
        },
        (err) => {
          console.log(`Organize imports request for ${uri} failed: ${JSON.stringify(err)}`)
          vscode.window.showErrorMessage(`Organize imports request failed: ${JSON.stringify(err)}`)
        }
      )
    })
  )

  context.subscriptions.push(
    vscode.commands.registerCommand("plasmon.organize-imports-in-module", () => {
      let uri = lastFocusedDocument
      interface Response {
        error?: string
        fileChanged: integer
      }
      client?.sendRequest(ExecuteCommandRequest.type, { command: "plasmon/organizeImportsInModule", arguments: [uri] }).then(
        (resp: Response) => {
          if (resp.error) {
            console.log(`Error organizing imports for module: ${resp.error}`)
            vscode.window.showErrorMessage(`Error organizing imports for module: ${resp.error}`)
          }
          else if (resp.fileChanged > 0) {
            vscode.window.showInformationMessage(`Organized imports (${resp.fileChanged} file(s) changed)`)
          }
          else {
            vscode.window.showInformationMessage(`Imports were already organized in module`)
          }
        },
        (err) => {
          console.log(`Organize imports in module request for ${uri} failed: ${JSON.stringify(err)}`)
          vscode.window.showErrorMessage(`Organize imports in module request failed: ${JSON.stringify(err)}`)
        }
      )
    })
  )

  //
  // Internal commands (meant to be triggered by UI actions)
  //

  context.subscriptions.push(
    vscode.commands.registerCommand(`plasmon-goto-location`, (location: Location) => {
      console.log(`plasmon-goto-location(${JSON.stringify(location)})`)
      let uri = vscode.Uri.parse(location.uri)
      let range = new vscode.Range(
        location.range.start.line,
        location.range.start.character,
        location.range.end.line,
        location.range.end.character
      )
      vscode.workspace.openTextDocument(uri).then(
        (doc) => {
          vscode.window.showTextDocument(doc, { selection: range })
        },
        (err) => {
          console.log(`Error opening ${location.uri}: ${err}`)
          vscode.window.showErrorMessage(`Error opening ${location.uri}: ${err}`)
        }
      )
    })
  )

  context.subscriptions.push(
    vscode.window.onDidChangeWindowState((windowState) => {
      checkStatusBarAndDocument()
      client?.sendNotification(
        "metals/windowStateDidChange",
        windowState.focused
      )
    })
  )

  context.subscriptions.push(
    vscode.workspace.onDidOpenTextDocument(() => checkStatusBarAndDocument())
  )

  context.subscriptions.push(
    vscode.workspace.onDidCloseTextDocument(() => checkStatusBarAndDocument())
  )

  context.subscriptions.push(
    vscode.workspace.onDidOpenNotebookDocument(() => {
      console.log(`onDidOpenNotebookDocument: Hiding status for notebook`)
      statusBarItem?.hide()
    })
  )

  context.subscriptions.push(
    vscode.window.onDidChangeActiveTextEditor((editor) => {
      if (editor && editor.document.uri.scheme == 'file') {
        if (isSupportedLanguage(editor.document.languageId)) {
          // console.log(`onDidChangeActiveTextEditor: Showing status for ${editor.document.uri} ${editor.document.languageId}`)
          let uriStr = editor.document.uri.toString(true)
          if (lastFocusedDocument != uriStr) {
            lastFocusedDocument = editor.document.uri.toString(true)
            client?.sendNotification(
              "metals/didFocusTextDocument",
              lastFocusedDocument
            )
          }
        }
        else {
          // console.log(`onDidChangeActiveTextEditor: Hiding status for ${editor.document.uri} ${editor.document.languageId}`)
          statusBarItem?.hide()
        }
      }
    })
  )

  context.subscriptions.push(
    vscode.window.onDidChangeActiveNotebookEditor(() => {
      console.log(`onDidChangeActiveNotebookEditor: Hiding status for notebook`)
      statusBarItem?.hide()
    })
  )

  console.log('The extension "plasmon" is now active')
}

export function deactivate(context: vscode.ExtensionContext) {
  stopClient(context)
}


        // [
        //   {
        //     id: "first",
        //     label: "the label",
        //     iconPath: vscode.ThemeIcon.File,
        //     description: "the description",
        //     detail: "the details",
        //     buttons: [
        //       {
        //         iconPath: vscode.ThemeIcon.Folder,
        //         tooltip: "Button tooltip"
        //       }
        //     ]
        //   },
        //   {
        //     id: "_",
        //     label: "foo",
        //     kind: vscode.QuickPickItemKind.Separator
        //   },
        //   {
        //     id: "second",
        //     label: "the other label",
        //     iconPath: vscode.ThemeIcon.Folder,
        //     description: "the other description",
        //     detail: "the other details",
        //     buttons: [
        //       {
        //         iconPath: vscode.ThemeIcon.File,
        //         tooltip: "Other button tooltip"
        //       }
        //     ]
        //   },
        //   {
        //     id: "third",
        //     label: "the last label",
        //     iconPath: vscode.ThemeIcon.Folder,
        //     description: "the last description",
        //     detail: "the last details",
        //     buttons: [
        //       {
        //         iconPath: vscode.ThemeIcon.File,
        //         tooltip: "Last button tooltip"
        //       }
        //     ]
        //   }
        // ]