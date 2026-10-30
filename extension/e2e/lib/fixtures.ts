// Playwright fixtures: one code-server (and Plasmon server) per worker, on a workspace given by
// the `workspace` option, and one workbench page shared by the tests of that worker. Spec files
// are meant to run their tests serially, each test building on the state left by the previous ones.

import { test as base } from '@playwright/test'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { optionsFromEnv, startCodeServer, type CodeServer } from './codeServer.ts'
import { VSCode } from './vscode.ts'
import { writeWorkspace, type WorkspaceFiles } from './workspace.ts'

type WorkerFixtures = {
  workspace: WorkspaceFiles
  workspaceDir: string
  codeServer: CodeServer
  vscode: VSCode
}

type TestFixtures = {
  failureArtifacts: void
}

export const test = base.extend<TestFixtures, WorkerFixtures>({
  workspace: [{}, { scope: 'worker', option: true }],

  workspaceDir: [
    async ({ workspace }, use) => {
      const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'plasmon-e2e-'))
      writeWorkspace(dir, workspace)
      await use(dir)
      fs.rmSync(dir, { recursive: true, force: true })
    },
    { scope: 'worker' },
  ],

  codeServer: [
    async ({ workspaceDir }, use, workerInfo) => {
      // Gets the logs of code-server, and the one of Plasmon once done
      const stateDir = path.join(workerInfo.project.outputDir, `code-server-${workerInfo.workerIndex}`)
      const server = await startCodeServer({ ...optionsFromEnv(), workspace: workspaceDir, stateDir })
      try {
        await use(server)
      } finally {
        await server.stop()
        // Keep the server log around, the workspace itself gets removed
        const log = path.join(workspaceDir, '.plasmon', 'log')
        if (fs.existsSync(log))
          fs.copyFileSync(log, path.join(stateDir, 'plasmon.log'))
      }
    },
    { scope: 'worker', timeout: 120_000 },
  ],

  vscode: [
    async ({ browser, codeServer }, use) => {
      const context = await browser.newContext({ viewport: { width: 1600, height: 1000 } })
      const page = await context.newPage()
      await use(await VSCode.open(page, codeServer.url))
      await context.close()
    },
    { scope: 'worker', timeout: 120_000 },
  ],

  failureArtifacts: [
    async ({ vscode, workspaceDir }, use, testInfo) => {
      await use()
      if (testInfo.status === testInfo.expectedStatus)
        return
      // Written to the test output directory, so that they're there whatever the reporter
      const screenshot = testInfo.outputPath('workbench.png')
      await vscode.page.screenshot({ path: screenshot })
      await testInfo.attach('workbench', { path: screenshot, contentType: 'image/png' })
      const log = path.join(workspaceDir, '.plasmon', 'log')
      if (fs.existsSync(log)) {
        const logTail = testInfo.outputPath('plasmon-log-tail.txt')
        fs.writeFileSync(logTail, tail(fs.readFileSync(log, 'utf8'), 400))
        await testInfo.attach('plasmon-log', { path: logTail, contentType: 'text/plain' })
      }
    },
    { auto: true },
  ],
})

export { expect } from '@playwright/test'

function tail(content: string, lines: number): string {
  return content.split('\n').slice(-lines).join('\n')
}
