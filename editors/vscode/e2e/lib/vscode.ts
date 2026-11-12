// Drives the VSCode workbench served by code-server, mostly through the keyboard
//
// Monaco tokens don't map one-to-one to DOM nodes (`g.message` can be a single span), so positions
// are reached with "Go to Line/Column" (Ctrl+G) rather than by clicking on text. Lines and columns
// are 1-based, like in the status bar.

import type { Locator, Page } from '@playwright/test'

export type Position = { line: number; col: number }
export type Location = Position & { file: string }
export type Problems = { errors: number; warnings: number }

export class VSCode {
  readonly page: Page

  constructor(page: Page) {
    this.page = page
    // Rather than no timeout for waitFor and actions, so that a wrong selector fails fast enough
    page.setDefaultTimeout(30_000)
  }

  static async open(page: Page, url: string): Promise<VSCode> {
    await page.goto(url)
    await page.locator('.monaco-workbench').waitFor({ timeout: 60_000 })
    // Let the explorer and extension host settle
    await page.locator('.explorer-folders-view .monaco-list-row').first().waitFor({ timeout: 30_000 })
    return new VSCode(page)
  }

  // Status bar

  statusBarItem(id: string): Locator {
    return this.page.locator(`.statusbar-item[id="${id}"]`)
  }

  // Text of the status bar item of the Plasmon extension ("No build tool", "Compiled", …)
  async plasmonStatus(): Promise<string> {
    // Not the aria-label, that also has the tooltip and the names of the icons
    const item = this.statusBarItem('alexarchambault.plasmon')
    if ((await item.count()) === 0)
      return ''
    const text = (await item.textContent()) ?? ''
    // Icons are characters from the Unicode private use area
    return text.replace(/[-]/g, '').replace(/\s+/g, ' ').trim()
  }

  async waitForPlasmonStatus(expected: RegExp, timeout = 120_000): Promise<string> {
    return this.waitFor(() => this.plasmonStatus(), status => expected.test(status), timeout,
      `Plasmon status bar item to match ${expected}`)
  }

  // Language status items (build tool, module, presentation compiler, …), as a single string
  async languageStatus(): Promise<string> {
    return (await this.statusBarItem('status.languageStatus').getAttribute('aria-label')) ?? ''
  }

  async problems(): Promise<Problems> {
    const label = (await this.statusBarItem('status.problems').getAttribute('aria-label')) ?? ''
    const count = (kind: string) => Number(label.match(new RegExp(`${kind}: (\\d+)`))?.[1] ?? 0)
    return { errors: count('Errors'), warnings: count('Warnings') }
  }

  async waitForProblems(predicate: (p: Problems) => boolean, timeout = 120_000): Promise<Problems> {
    return this.waitFor(() => this.problems(), predicate, timeout, 'problems to match')
  }

  async cursor(): Promise<Position> {
    const label = (await this.statusBarItem('status.editor.selection').getAttribute('aria-label')) ?? ''
    const m = label.match(/Ln (\d+), Col (\d+)/)
    if (!m)
      throw new Error(`Cannot read cursor position from '${label}'`)
    return { line: Number(m[1]), col: Number(m[2]) }
  }

  // File name of the active editor, from the window title ("Main.scala - ws - code-server")
  async activeFile(): Promise<string> {
    const title = await this.page.title()
    return title.split(' - ')[0].replace(/^● /, '')
  }

  // Commands, quick open, quick picks

  // The quick input widget is re-used, and keeps the rows of its former uses once hidden
  quickInput(): Locator {
    return this.page.locator('.quick-input-widget').filter({ visible: true })
  }

  quickInputRows(): Locator {
    return this.quickInput().locator('.monaco-list-row').filter({ visible: true })
  }

  // Runs a command from the command palette, `title` being its exact title
  async runCommand(title: string): Promise<void> {
    await this.page.keyboard.press('Control+Shift+P')
    await this.quickInput().waitFor()
    await this.page.keyboard.type(title)
    await this.waitFor(() => this.quickPickItems(), labels => labels.includes(title), 30_000,
      `command '${title}' in the command palette`)
    await this.pickQuickPickItem(title, 30_000, false)
    // The command may re-use the widget for a quick pick of its own, whose input doesn't start with '>'
    await this.waitFor(
      async () => {
        const input = this.quickInput().locator('input').first()
        return (await input.count()) === 0 || !(await input.inputValue()).startsWith('>')
      },
      closed => closed,
      30_000,
      'the command palette to close'
    )
  }

  // Opens a file via Quick Open (Ctrl+P), `name` being a path relative to the workspace or a file name
  async openFile(name: string): Promise<void> {
    await this.page.keyboard.press('Control+P')
    await this.quickInput().waitFor()
    await this.page.keyboard.type(name)
    // Rows read like "Main.scala foo/src"
    const base = name.split('/').pop()!
    await this.quickInputRows().filter({ hasText: base }).first().waitFor()
    await this.page.keyboard.press('Enter')
    await this.waitFor(() => this.activeFile(), f => f === base, 30_000, `${base} to be the active editor`)
  }

  // Labels of the items of the currently open quick pick, once it has some
  async quickPickItems(timeout = 120_000): Promise<string[]> {
    const rows = this.quickInputRows()
    await rows.first().waitFor({ timeout })
    // The aria-label also has the name of the item icon and its description, keep the label only
    return rows.evaluateAll(es =>
      es.map(e => (e.querySelector('.label-name')?.textContent ?? e.getAttribute('aria-label') ?? '').trim())
    )
  }

  // Picks the first item whose label (as returned by quickPickItems) is / matches `label`
  async pickQuickPickItem(label: RegExp | string, timeout = 120_000, waitForClose = true): Promise<void> {
    const labels = await this.quickPickItems(timeout)
    const idx = labels.findIndex(l => (typeof label === 'string' ? l === label : label.test(l)))
    if (idx < 0)
      throw new Error(`No quick pick item matching ${label} in ${JSON.stringify(labels)}`)
    await this.quickInputRows().nth(idx).click()
    if (waitForClose)
      await this.quickInput().waitFor({ state: 'hidden' })
  }

  // Explorer

  async explorerContextMenu(fileName: string, item: string): Promise<void> {
    const row = this.page.locator('.explorer-folders-view .monaco-list-row', {
      has: this.page.locator('.label-name', { hasText: new RegExp(`^${escapeRegExp(fileName)}$`) }),
    })
    await row.first().click({ button: 'right' })
    // Menus ignore clicks right after opening, select the item via the keyboard instead
    const menu = this.page.getByRole('menu').filter({ visible: true })
    await menu.getByRole('menuitem', { name: item }).first().hover()
    await this.page.keyboard.press('Enter')
    await menu.waitFor({ state: 'hidden' })
  }

  // Editor

  async focusEditor(): Promise<void> {
    await this.runCommand('View: Focus Active Editor Group')
  }

  async goTo(line: number, col = 1): Promise<void> {
    await this.page.keyboard.press('Control+G')
    await this.quickInput().waitFor()
    await this.page.keyboard.type(`${line}:${col}`)
    await this.page.keyboard.press('Enter')
    // Keys sent before the widget is gone get lost
    await this.quickInput().waitFor({ state: 'hidden' })
  }

  // Inserts a new line after `line`, with the given content (no auto-closing brackets or
  // auto-indentation, as it's pasted rather than typed)
  async insertLineAfter(line: number, content: string): Promise<void> {
    await this.goTo(line, 1)
    await this.page.keyboard.press('End')
    await this.page.keyboard.press('Enter')
    await this.page.keyboard.press('Shift+Home')
    await this.page.keyboard.insertText(content)
  }

  async deleteLine(line: number): Promise<void> {
    await this.goTo(line, 1)
    await this.page.keyboard.press('Control+Shift+K')
  }

  async save(): Promise<void> {
    await this.page.keyboard.press('Control+S')
    await this.page.locator('.tab.active.dirty').waitFor({ state: 'detached' })
  }

  // Hover at a position, as shown by "Show or Focus Hover" (Ctrl+K Ctrl+I)
  async hover(line: number, col: number, timeout = 60_000): Promise<string> {
    await this.goTo(line, col)
    await this.page.keyboard.press('Control+K')
    await this.page.keyboard.press('Control+I')
    const content = this.page.locator('.monaco-hover-content').first()
    await content.waitFor({ timeout })
    // The hover shows "Loading..." first
    const text = await this.waitFor(() => content.innerText(), t => !/^Loading/.test(t), timeout, 'hover content')
    await this.page.keyboard.press('Escape')
    return text.trim()
  }

  // Go to Definition (F12) from a position, returns where the cursor lands
  async definition(line: number, col: number, timeout = 60_000): Promise<Location> {
    await this.goTo(line, col)
    const before = { file: await this.activeFile(), ...(await this.cursor()) }
    await this.page.keyboard.press('F12')
    return this.waitFor(
      async () => ({ file: await this.activeFile(), ...(await this.cursor()) }),
      loc => loc.file !== before.file || loc.line !== before.line || loc.col !== before.col,
      timeout,
      'go to definition to move the cursor'
    )
  }

  // Completion labels after typing `text` at the cursor (the suggest widget is closed afterwards,
  // the typed text is left in the editor)
  async completions(text: string, timeout = 60_000): Promise<string[]> {
    await this.page.keyboard.type(text)
    await this.page.keyboard.press('Control+Space')
    const rows = this.page.locator('.suggest-widget.visible .monaco-list-row')
    await rows.first().waitFor({ timeout })
    const labels = await rows.evaluateAll(es =>
      es.map(e => e.querySelector('.label-name')?.textContent ?? e.getAttribute('aria-label') ?? '')
    )
    await this.page.keyboard.press('Escape')
    return labels
  }

  // Polls `get` until `predicate` holds
  async waitFor<T>(get: () => Promise<T>, predicate: (value: T) => boolean, timeout: number, what: string): Promise<T> {
    const deadline = Date.now() + timeout
    let last: T | undefined
    while (true) {
      last = await get()
      if (predicate(last))
        return last
      if (Date.now() > deadline)
        throw new Error(`Timed out after ${timeout} ms waiting for ${what}, last value: ${JSON.stringify(last)}`)
      await this.page.waitForTimeout(250)
    }
  }
}

function escapeRegExp(s: string): string {
  return s.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')
}
