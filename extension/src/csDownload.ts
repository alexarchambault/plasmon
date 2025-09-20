import { exec, execFile } from 'child_process'
import * as fs from 'fs'
import { Uri } from 'vscode'

// some things adapted from https://github.com/coursier/cache-action/blob/142d2738bd29f0eb9d44610828acb3a19809feab/src/restore.ts#L13-L49

let _unameValue = ''

function uname(): Promise<string> {
  return new Promise<string>((resolve, reject) => {
    if (_unameValue)
      resolve(_unameValue)
    else
      execFile('uname', ['-s'], { windowsHide: true }, (error, stdout) => {
        if (error)
          reject(error)
        else {
          _unameValue = stdout.trim()
          resolve(_unameValue)
        }
      })
  })
}

function getOs(unameShort: string): string {
  if (unameShort.startsWith('Darwin')) return 'darwin'
  if (unameShort.startsWith('Linux')) return 'linux'
  if (
    unameShort.startsWith('CYGWIN') ||
    unameShort.startsWith('MINGW') ||
    unameShort.startsWith('MSYS')
  )
    return 'win'
  return 'unknown'
}

function getCacheRoot(os: string): string {
  if (os === 'win') return '~\\AppData\\Local\\Coursier\\Cache'
  if (os === 'darwin') return '~/Library/Caches/Coursier'
  return '~/.cache/coursier'
}

async function cacheRoot(): Promise<string> {
  return getCacheRoot(getOs(await uname()))
}

async function cachePath(url: string): Promise<string> {

  let uri = Uri.parse(url)

  if (uri.query)
    throw new Error(`Cached URL cannot have a query (got ${url})`)
  if (uri.fragment)
    throw new Error(`Cached URL cannot have a fragment (got ${url})`)

  if (!uri.scheme)
    throw new Error(`Cached URL must have a scheme (got ${url})`)
  if (!uri.authority)
    throw new Error(`Cached URL must have an authority (got ${url})`)
  if (!uri.path)
    throw new Error(`Cached URL must have a path (got ${url})`)

  let os = getOs(await uname())

  let sep: string = os == 'win' ? `\\` : '/'
  let path: string = `${getCacheRoot(os)}${sep}${uri.scheme}${sep}${uri.authority}${sep}`

  let pathParts = uri.path.split('/')
  for (const part in pathParts)
    path = `${path}${sep}${part}`

  return path
}
