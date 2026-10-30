// Starts code-server with the local Plasmon extension and server, plus a headless Chromium on the
// workbench that dev/drive.ts connects to, and keeps them running until interrupted.
//
// Usage (via Mill, which builds and passes the extension, launcher, and code-server):
//   ./mill extension.e2e.serve [--state-dir <dir>] [--port <port>] [workspace]
// Without a workspace, a sample Mill project is written to <state-dir>/workspace.

import { chromium } from '@playwright/test'
import fs from 'node:fs'
import path from 'node:path'
import { parseArgs } from 'node:util'
import { freePort, optionsFromEnv, startCodeServer } from '../lib/codeServer.ts'
import { VSCode } from '../lib/vscode.ts'
import { millProject, writeWorkspace } from '../lib/workspace.ts'
import { defaultStateDir, type ServeState } from './state.ts'

const { values, positionals } = parseArgs({
  allowPositionals: true,
  options: {
    'state-dir': { type: 'string', default: defaultStateDir },
    port: { type: 'string' },
  },
})

const stateDir = path.resolve(values['state-dir'])
let workspace: string
if (positionals.length > 0)
  workspace = path.resolve(positionals[0])
else {
  workspace = path.join(stateDir, 'workspace')
  fs.rmSync(workspace, { recursive: true, force: true })
  writeWorkspace(workspace, millProject())
}

fs.rmSync(path.join(stateDir, 'code-server'), { recursive: true, force: true })
const codeServer = await startCodeServer({
  ...optionsFromEnv(),
  workspace,
  stateDir: path.join(stateDir, 'code-server'),
  port: values.port ? Number(values.port) : undefined,
})

const cdpPort = await freePort()
const context = await chromium.launchPersistentContext(path.join(stateDir, 'chromium'), {
  headless: true,
  viewport: { width: 1600, height: 1000 },
  args: [`--remote-debugging-port=${cdpPort}`],
  // Else Playwright exits on these before stop() below gets to stop code-server
  handleSIGINT: false,
  handleSIGTERM: false,
  handleSIGHUP: false,
})
const page = context.pages()[0] ?? (await context.newPage())
await VSCode.open(page, codeServer.url)

const state: ServeState = {
  url: codeServer.url,
  cdpPort,
  workspace,
  stateDir,
  codeServerLog: codeServer.logFile,
  plasmonLog: path.join(workspace, '.plasmon', 'log'),
}
const stateFile = path.join(stateDir, 'state.json')
fs.writeFileSync(stateFile, JSON.stringify(state, null, 2))

console.log(`code-server: ${codeServer.url}`)
console.log(`Chromium DevTools protocol: http://127.0.0.1:${cdpPort}`)
console.log(`Workspace: ${workspace}`)
console.log(`State: ${stateFile}`)
console.log('Ready')

let stopping = false
async function stop() {
  if (stopping)
    return
  stopping = true
  fs.rmSync(stateFile, { force: true })
  await context.close().catch(() => {})
  await codeServer.stop()
  process.exit(0)
}
process.on('SIGINT', stop)
process.on('SIGTERM', stop)
process.on('SIGHUP', stop)
context.on('close', stop)
