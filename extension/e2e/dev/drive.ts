// Runs a snippet against the workbench started by dev/serve.ts. The page and editor state persist
// between runs, like in an interactive session.
//
// Usage:
//   node extension/e2e/dev/drive.ts [--state-dir <dir>] '<async JS function body>'
//   node extension/e2e/dev/drive.ts [--state-dir <dir>] --file <script.js>
//
// In scope: `vscode` (lib/vscode.ts), `page` (Playwright Page), `state` (dev/state.ts), and
// `screenshot(name?)`, which saves a PNG under <state-dir>/screenshots and returns its path.
// A returned value is printed (as JSON unless it's a string). Example:
//   node extension/e2e/dev/drive.ts 'await vscode.openFile("Main.scala"); return vscode.hover(10, 16)'

import { chromium } from '@playwright/test'
import fs from 'node:fs'
import path from 'node:path'
import { parseArgs } from 'node:util'
import { VSCode } from '../lib/vscode.ts'
import { defaultStateDir, readState } from './state.ts'

const { values, positionals } = parseArgs({
  allowPositionals: true,
  options: {
    'state-dir': { type: 'string', default: defaultStateDir },
    file: { type: 'string' },
  },
})

const body = values.file ? fs.readFileSync(values.file, 'utf8') : positionals.join(' ')
if (!body.trim()) {
  console.error('Usage: drive.ts [--state-dir <dir>] (\'<JS>\' | --file <script.js>)')
  process.exit(2)
}

const state = readState(path.resolve(values['state-dir']))
const browser = await chromium.connectOverCDP(`http://127.0.0.1:${state.cdpPort}`)
const page = browser.contexts().flatMap(c => c.pages()).find(p => p.url().startsWith(state.url.split('?')[0]))
if (!page)
  throw new Error(`No page on ${state.url} in the Chromium started by serve.ts`)
const vscode = new VSCode(page)

const screenshotDir = path.join(state.stateDir, 'screenshots')
let screenshotCount = 0
async function screenshot(name?: string): Promise<string> {
  fs.mkdirSync(screenshotDir, { recursive: true })
  const file = path.join(screenshotDir, `${name ?? `${Date.now()}-${screenshotCount++}`}.png`)
  await page!.screenshot({ path: file })
  return file
}

const AsyncFunction = Object.getPrototypeOf(async function () {}).constructor
try {
  const result = await new AsyncFunction('vscode', 'page', 'state', 'screenshot', body)(vscode, page, state, screenshot)
  if (result !== undefined)
    console.log(typeof result === 'string' ? result : JSON.stringify(result, null, 2))
} catch (e) {
  console.error(e)
  process.exitCode = 1
}
// Only disconnect, the browser keeps running for the next snippets
process.exit()
