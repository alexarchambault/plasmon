// Test projects, written to a fresh directory before code-server opens them

import fs from 'node:fs'
import path from 'node:path'

export type WorkspaceFiles = Record<string, string>

// Root of the plasmon repository
export const repoRoot = path.resolve(import.meta.dirname, '..', '..', '..')

export function writeWorkspace(dir: string, files: WorkspaceFiles): void {
  for (const [relPath, content] of Object.entries(files)) {
    const dest = path.join(dir, relPath)
    fs.mkdirSync(path.dirname(dest), { recursive: true })
    fs.writeFileSync(dest, content)
  }
  if ('build.mill' in files) {
    fs.copyFileSync(path.join(repoRoot, 'mill'), path.join(dir, 'mill'))
    fs.chmodSync(path.join(dir, 'mill'), 0o755)
  }
}

export function millVersion(): string {
  const version = process.env.PLASMON_E2E_MILL_VERSION
  if (!version)
    throw new Error('PLASMON_E2E_MILL_VERSION not set (run via ./mill extension.e2e.test)')
  return version
}

// A single-module Mill project. Positions in Main.scala are relied on by the tests.
export function millProject(): WorkspaceFiles {
  return {
    'build.mill': `//| mill-version: ${millVersion()}
package build
import mill.*, scalalib.*

object foo extends ScalaModule {
  def scalaVersion = "3.8.4"
}
`,
    'foo/src/Main.scala': `package foo

case class Greeting(name: String) {
  def message: String = s"Hello, $name!"
}

object Main {
  def main(args: Array[String]): Unit = {
    val g = Greeting("plasmon")
    println(g.message)
  }
}
`,
  }
}
