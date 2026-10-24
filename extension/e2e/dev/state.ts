import fs from 'node:fs'
import path from 'node:path'

// Written by dev/serve.ts, read by dev/drive.ts
export type ServeState = {
  url: string
  cdpPort: number
  workspace: string
  stateDir: string
  codeServerLog: string
  plasmonLog: string
}

export const defaultStateDir = path.resolve(import.meta.dirname, '..', '.serve')

export function readState(stateDir: string): ServeState {
  const file = path.join(stateDir, 'state.json')
  if (!fs.existsSync(file))
    throw new Error(`${file} not found, start code-server first with './mill extension.e2e.serve'`)
  return JSON.parse(fs.readFileSync(file, 'utf8'))
}
