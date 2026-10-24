// Starts code-server with the Plasmon extension installed, in an isolated profile, its server
// being a locally built launcher

import { spawn, spawnSync, type ChildProcess } from 'node:child_process'
import fs from 'node:fs'
import net from 'node:net'
import os from 'node:os'
import path from 'node:path'

export type CodeServerOptions = {
  // code-server executable
  codeServer: string
  // Plasmon extension to install
  vsix: string
  // Plasmon launcher, used by the extension via PLASMON_FORCED_BINARY
  launcher: string
  // Directory to open in code-server
  workspace: string
  // Gets the code-server extensions directory and logs
  stateDir: string
  port?: number
  env?: Record<string, string>
}

export type CodeServer = {
  // URL opening the workspace
  url: string
  port: number
  pid: number
  logFile: string
  stop(): Promise<void>
}

// Inputs passed by Mill (see extension.e2e in build.mill)
export function requiredEnv(name: string): string {
  const value = process.env[name]
  if (!value)
    throw new Error(
      `${name} not set. Run these via './mill extension.e2e.test' or './mill extension.e2e.serve', ` +
        'which build what they need and set it.'
    )
  return value
}

export function optionsFromEnv(): Pick<CodeServerOptions, 'codeServer' | 'vsix' | 'launcher'> {
  return {
    codeServer: requiredEnv('PLASMON_E2E_CODE_SERVER'),
    vsix: requiredEnv('PLASMON_E2E_VSIX'),
    launcher: requiredEnv('PLASMON_E2E_LAUNCHER'),
  }
}

export function freePort(): Promise<number> {
  return new Promise((resolve, reject) => {
    const server = net.createServer()
    server.unref()
    server.on('error', reject)
    server.listen(0, '127.0.0.1', () => {
      const port = (server.address() as net.AddressInfo).port
      server.close(() => resolve(port))
    })
  })
}

const userSettings = {
  'security.workspace.trust.enabled': false,
  'workbench.startupEditor': 'none',
  'workbench.tips.enabled': false,
  'workbench.secondarySideBar.defaultVisibility': 'hidden',
  'chat.disableAIFeatures': true,
  'telemetry.telemetryLevel': 'off',
  'extensions.autoUpdate': false,
  'extensions.autoCheckUpdates': false,
  'update.mode': 'none',
  'window.restoreWindows': 'none',
  'files.autoSave': 'off',
}

// Environment variables of ours that shouldn't leak into the build tools the server starts
const droppedEnv = ['MILL_OUTPUT_DIR', 'PLASMON_FORCED_VERSION', 'PLASMON_FORCED_BINARY']

export async function startCodeServer(options: CodeServerOptions): Promise<CodeServer> {
  // code-server creates a Unix socket in its user data directory, whose path can't be longer
  // than ~100 characters, so that directory goes in a short temporary one. Its logs are copied
  // to the state directory when stopping.
  const userDataDir = fs.mkdtempSync(path.join(os.tmpdir(), 'plasmon-cs-'))
  const extensionsDir = path.join(options.stateDir, 'extensions')
  const logFile = path.join(options.stateDir, 'code-server.log')
  fs.mkdirSync(path.join(userDataDir, 'User'), { recursive: true })
  fs.mkdirSync(options.stateDir, { recursive: true })
  fs.mkdirSync(extensionsDir, { recursive: true })
  fs.writeFileSync(
    path.join(userDataDir, 'User', 'settings.json'),
    JSON.stringify(userSettings, null, 2)
  )

  const dirArgs = ['--user-data-dir', userDataDir, '--extensions-dir', extensionsDir]

  const install = spawnSync(
    options.codeServer,
    [...dirArgs, '--install-extension', options.vsix, '--force'],
    { encoding: 'utf8' }
  )
  if (install.status !== 0) {
    fs.rmSync(userDataDir, { recursive: true, force: true })
    throw new Error(`Installing ${options.vsix} failed:\n${install.stdout}\n${install.stderr}`)
  }

  const env: Record<string, string | undefined> = { ...process.env }
  for (const name of droppedEnv)
    delete env[name]
  Object.assign(env, options.env, { PLASMON_FORCED_BINARY: options.launcher })

  const port = options.port ?? (await freePort())
  const log = fs.openSync(logFile, 'a')
  const proc: ChildProcess = spawn(
    options.codeServer,
    [
      ...dirArgs,
      '--bind-addr', `127.0.0.1:${port}`,
      '--auth', 'none',
      '--disable-telemetry',
      '--disable-update-check',
      '--disable-workspace-trust',
      options.workspace,
    ],
    // detached, so that stop() can kill the whole process group (extension host, Plasmon server, …)
    { env, stdio: ['ignore', log, log], detached: true }
  )
  fs.closeSync(log)

  const exited = new Promise<void>(resolve => proc.once('exit', () => resolve()))
  let stopped = false
  const stop = async () => {
    if (stopped)
      return
    stopped = true
    await killProcessGroup()
    const logs = path.join(userDataDir, 'logs')
    if (fs.existsSync(logs))
      fs.cpSync(logs, path.join(options.stateDir, 'logs'), { recursive: true })
    fs.rmSync(userDataDir, { recursive: true, force: true })
  }
  // Even if code-server itself is gone, some of its sub-processes might not be
  const killProcessGroup = async () => {
    const kill = (signal: NodeJS.Signals) => {
      try {
        process.kill(-proc.pid!, signal)
      } catch {
        // already gone
      }
    }
    kill('SIGTERM')
    if (proc.exitCode !== null || proc.signalCode !== null)
      return
    const timeout = new Promise<'timeout'>(resolve => setTimeout(() => resolve('timeout'), 10_000).unref())
    if ((await Promise.race([exited, timeout])) === 'timeout') {
      kill('SIGKILL')
      await exited
    }
  }

  try {
    await waitForHealthz(port, proc, logFile)
  } catch (e) {
    await stop()
    throw e
  }

  return {
    url: `http://127.0.0.1:${port}/?folder=${encodeURIComponent(options.workspace)}`,
    port,
    pid: proc.pid!,
    logFile,
    stop,
  }
}

async function waitForHealthz(port: number, proc: ChildProcess, logFile: string): Promise<void> {
  const deadline = Date.now() + 60_000
  while (Date.now() < deadline) {
    if (proc.exitCode !== null)
      throw new Error(`code-server exited with code ${proc.exitCode}, see ${logFile}`)
    try {
      const resp = await fetch(`http://127.0.0.1:${port}/healthz`)
      if (resp.ok)
        return
    } catch {
      // not listening yet
    }
    await new Promise(resolve => setTimeout(resolve, 250))
  }
  throw new Error(`code-server didn't start listening on port ${port} within a minute, see ${logFile}`)
}
