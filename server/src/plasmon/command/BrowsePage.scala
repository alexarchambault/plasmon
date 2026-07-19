package plasmon.command

import scalatags.Text.all.*
import scalatags.Text.tags2

/** The single page of `plasmon browse`
  *
  * It only uses URLs relative to its own for the API (`api/tree`, `api/file`), so that it works
  * behind proxies keeping a path prefix. CodeMirror (syntax coloring) and Devicon (file icons)
  * come from CDNs.
  */
private[command] object BrowsePage {

  private val codeMirror = "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.18"
  private val devicon    = "https://cdn.jsdelivr.net/gh/devicons/devicon@v2.16.0/devicon.min.css"

  def render(rootName: String): String =
    doctype("html")(
      html(
        head(
          meta(charset := "utf-8"),
          meta(name := "viewport", content := "width=device-width, initial-scale=1"),
          tags2.title(rootName),
          link(rel := "stylesheet", href := s"$codeMirror/codemirror.min.css"),
          link(rel := "stylesheet", href := s"$codeMirror/theme/material-darker.min.css"),
          link(rel := "stylesheet", href := devicon),
          tags2.style(raw(css))
        ),
        body(cls := "no-file", attr("data-root") := rootName, attr("data-codemirror") := codeMirror)(
          tags2.aside(id := "sidebar")(
            h1(id := "root-name", rootName),
            ul(id := "tree")
          ),
          tags2.main(id := "viewer")(
            div(id := "header")(
              button(id := "toggle", attr("title") := "Show / hide files", attr("aria-label") := "Show / hide files")(
                raw(sidebarIcon)
              ),
              span(id := "path"),
              span(id := "language"),
              button(id := "close", attr("title") := "Close file", attr("aria-label") := "Close file")("✕")
            ),
            div(id := "editor"),
            div(id := "message")
          ),
          script(src := s"$codeMirror/codemirror.min.js"),
          script(src := s"$codeMirror/mode/meta.min.js"),
          script(src := s"$codeMirror/addon/mode/loadmode.min.js"),
          script(raw(js))
        )
      )
    ).render

  private def sidebarIcon =
    """<svg width="18" height="18" viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.2"><rect x="1.5" y="2.5" width="13" height="11" rx="1.5"/><line x1="6" y1="2.5" x2="6" y2="13.5"/></svg>"""

  private def css =
    """
:root {
  --bg: #1e1e1e; --side: #181818; --border: #2b2b2b; --fg: #cccccc; --muted: #8b8b8b;
  --hover: #2a2d2e; --selected: #37373d; --sidebar-width: 300px;
  --mono: ui-monospace, SFMono-Regular, Menlo, Consolas, "Liberation Mono", monospace;
}
* { box-sizing: border-box }
html, body { height: 100%; margin: 0 }
body { display: flex; overflow: hidden; color: var(--fg); background: var(--bg); font: 13px system-ui, -apple-system, "Segoe UI", sans-serif }
button { display: flex; align-items: center; justify-content: center; width: 30px; height: 30px; padding: 0; border: 0; border-radius: 5px; color: var(--muted); background: none; font: inherit; cursor: pointer }
button:hover { color: var(--fg); background: var(--hover) }

#toggle { flex: none }
body.file-open:not(.folded) #toggle { color: var(--fg); background: var(--selected) }

#sidebar { flex: none; width: var(--sidebar-width); overflow: auto; padding: 8px 4px 24px; background: var(--side); border-right: 1px solid var(--border); transition: width .18s ease, padding .18s ease }
body.file-open.folded #sidebar { width: 0; padding-left: 0; padding-right: 0; border-right: 0; overflow: hidden }
h1 { margin: 2px 10px 8px; color: var(--muted); font-size: 11px; font-weight: 600; letter-spacing: .08em; text-transform: uppercase; white-space: nowrap }
ul { list-style: none; margin: 0; padding-left: 12px }
#tree { padding-left: 0 }
.entry { display: flex; align-items: center; gap: 6px; height: 24px; padding: 0 6px; white-space: nowrap; cursor: pointer; border-radius: 4px; user-select: none }
.entry:hover { background: var(--hover) }
.entry.selected { color: #fff; background: var(--selected) }
.chevron { flex: none; width: 12px; color: var(--muted); font-size: 10px; transition: transform .1s }
.open > .entry .chevron { transform: rotate(90deg) }
.icon { flex: none; width: 16px; font-size: 15px; text-align: center; color: var(--muted) }
.icon.folder { color: #c09553 }
/* Devicon colors too dark for a dark background */
.icon.colored:is(.devicon-markdown-plain, .devicon-json-plain, .devicon-rust-plain, .devicon-bash-plain, .devicon-perl-plain) { color: #c5c5c5 }
.name { overflow: hidden; text-overflow: ellipsis }
.children { display: none } .open > .children { display: block }
.empty { padding: 4px 24px; color: var(--muted); font-style: italic }

#viewer { display: flex; flex: 1; min-width: 0; flex-direction: column; position: relative }
body.no-file :is(#header, #editor, #message) { display: none !important }
#header { display: flex; flex: none; align-items: center; gap: 8px; height: 38px; padding: 0 4px; border-bottom: 1px solid var(--border); white-space: nowrap }
#path { flex: 1; min-width: 0; overflow: hidden; text-overflow: ellipsis; font-family: var(--mono); font-size: 12px }
#language { color: var(--muted); font-size: 12px }
#editor { flex: 1; min-height: 0 }
#message { display: none; position: absolute; inset: 38px 0 0; padding: 24px; color: var(--muted); background: var(--bg) }
#viewer.has-message #message { display: block }
.CodeMirror { height: 100%; font: 13px/1.5 var(--mono) }
.CodeMirror-code { -webkit-user-select: text; user-select: text }
.cm-s-material-darker.CodeMirror, .cm-s-material-darker .CodeMirror-gutters { background: var(--bg) }
.cm-s-material-darker .CodeMirror-gutters { border-right: 1px solid var(--border) }
.cm-s-material-darker .CodeMirror-linenumber { color: #5a5a5a }

/* phones: the unfolded file tree goes over the file */
@media (max-width: 700px) {
  /* no file: the file tree takes the whole page */
  body.no-file #viewer { display: none }
  body.no-file #sidebar { flex: 1; width: auto; border-right: 0; background: var(--bg) }
  body.no-file .entry { height: 30px; font-size: 14px }
  /* below the file header, so that its button can fold the tree back */
  :root { --sidebar-width: 100vw }
  body.file-open #sidebar { position: absolute; z-index: 10; top: 38px; bottom: 0; left: 0; border-top: 1px solid var(--border); box-shadow: 0 6px 16px rgba(0, 0, 0, .5) }
}
"""

  private def js =
    """
(() => {
const $ = id => document.getElementById(id);
const body = document.body, tree = $('tree'), viewer = $('viewer');
const rootName = body.dataset.root;

CodeMirror.modeURL = body.dataset.codemirror + '/mode/%N/%N.min.js';
const editor = CodeMirror($('editor'), {
  // On touch screens, a focusable editor brings up the virtual keyboard, even when read-only:
  // 'nocursor' makes it non-focusable (text can still be selected and copied)
  readOnly: matchMedia('(pointer: coarse)').matches ? 'nocursor' : true,
  lineNumbers: true, theme: 'material-darker', value: '', viewportMargin: 30
});
editor.getInputField().setAttribute('inputmode', 'none');

// Language names, as in .gitattributes linguist-language (lower case), per extension
const extLanguages = {
  java: 'java', c: 'c', h: 'c', cc: 'c++', cpp: 'c++', cxx: 'c++', hh: 'c++', hpp: 'c++', hxx: 'c++',
  cs: 'c#', py: 'python', pyi: 'python', sh: 'shell', bash: 'shell', zsh: 'shell', ksh: 'shell',
  scala: 'scala', sc: 'scala', sbt: 'scala', mill: 'scala', go: 'go', rs: 'rust',
  js: 'javascript', mjs: 'javascript', cjs: 'javascript', jsx: 'javascript', ts: 'typescript', tsx: 'typescript',
  kt: 'kotlin', kts: 'kotlin', rb: 'ruby', php: 'php', hs: 'haskell', lua: 'lua', md: 'markdown',
  html: 'html', htm: 'html', css: 'css', json: 'json', yml: 'yaml', yaml: 'yaml', swift: 'swift',
  ml: 'ocaml', mli: 'ocaml', nix: 'nix', pl: 'perl', pm: 'perl', r: 'r', dart: 'dart',
  ex: 'elixir', exs: 'elixir', erl: 'erlang', hrl: 'erlang', clj: 'clojure', cljs: 'clojure', cljc: 'clojure', edn: 'clojure',
  zig: 'zig', groovy: 'groovy', gradle: 'gradle', xml: 'xml', vim: 'vim script', lisp: 'common lisp', el: 'emacs lisp'
};
// Devicon names per language
const icons = {
  java: 'java', c: 'c', 'c++': 'cplusplus', 'c#': 'csharp', python: 'python', shell: 'bash', bash: 'bash',
  scala: 'scala', go: 'go', rust: 'rust', javascript: 'javascript', typescript: 'typescript', kotlin: 'kotlin',
  ruby: 'ruby', php: 'php', haskell: 'haskell', lua: 'lua', markdown: 'markdown', html: 'html5', css: 'css3',
  json: 'json', yaml: 'yaml', swift: 'swift', ocaml: 'ocaml', nix: 'nixos', perl: 'perl', r: 'r', dart: 'dart',
  elixir: 'elixir', erlang: 'erlang', clojure: 'clojure', zig: 'zig', groovy: 'groovy', gradle: 'gradle',
  xml: 'xml', 'vim script': 'vim', dockerfile: 'docker'
};
// File names CodeMirror's meta.js doesn't know about
const extraModes = { sc: 'Scala', sbt: 'Scala', mill: 'Scala', kts: 'Kotlin', zsh: 'Shell', bash: 'Shell' };

function extension(name) {
  const i = name.lastIndexOf('.');
  return i > 0 ? name.slice(i + 1).toLowerCase() : '';
}
function languageOf(name, language) {
  if (language) return language.toLowerCase();
  if (/^(Dockerfile|Containerfile)(\..*)?$/.test(name)) return 'dockerfile';
  return extLanguages[extension(name)];
}
function iconElement(item) {
  const icon = document.createElement('i');
  const name = icons[languageOf(item.name, item.language)];
  icon.className = 'icon ' + (name ? `devicon-${name}-plain colored` : '');
  if (!name) icon.innerHTML = '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.1"><path d="M4 1.5h5.5L13 5v9.5H4z"/><path d="M9.5 1.5V5H13"/></svg>';
  return icon;
}
// Interpreter of a `#!` line, for files without extensions
const interpreters = { sh: 'Shell', bash: 'Shell', zsh: 'Shell', ksh: 'Shell', dash: 'Shell', python: 'Python', python3: 'Python', node: 'JavaScript', ruby: 'Ruby', perl: 'Perl', lua: 'Lua', scala: 'Scala', 'scala-cli': 'Scala', amm: 'Scala' };
function shebangMode(content) {
  const m = /^#!\s*(?:\S*\/)?([\w.-]+)(?:\s+(?:-\S+\s+)*([\w.-]+))?/.exec(content);
  if (!m) return null;
  const command = m[1] === 'env' ? m[2] : m[1];
  const name = command && interpreters[command.replace(/[\d.]+$/, '')] || interpreters[command];
  return name ? CodeMirror.findModeByName(name) : null;
}
function modeInfo(name, language, content) {
  return (language && (CodeMirror.findModeByName(language) || CodeMirror.findModeByName(language.replace(/-/g, ' ')))) ||
    CodeMirror.findModeByFileName(name) ||
    (extraModes[extension(name)] && CodeMirror.findModeByName(extraModes[extension(name)])) ||
    shebangMode(content);
}

// The file tree
const dirs = new Map(), rows = new Map();
async function fillDir(path, target) {
  const response = await fetch('api/tree?path=' + encodeURIComponent(path));
  if (!response.ok) throw new Error(await response.text());
  const entries = await response.json();
  if (!entries.length) {
    const li = document.createElement('li');
    li.className = 'empty'; li.textContent = 'empty';
    target.append(li);
  }
  for (const item of entries) target.append(entry(item));
}
function entry(item) {
  const li = document.createElement('li'), row = document.createElement('div');
  const chevron = document.createElement('span'), name = document.createElement('span');
  row.className = 'entry'; chevron.className = 'chevron'; name.className = 'name';
  name.textContent = item.name; row.title = item.path;
  rows.set(item.path, row);
  li.append(row);
  if (item.dir) {
    chevron.textContent = '▶';
    const icon = document.createElement('i');
    icon.className = 'icon folder';
    icon.innerHTML = '<svg width="15" height="15" viewBox="0 0 16 16" fill="currentColor"><path d="M1.5 3.5a1 1 0 0 1 1-1h3.6l1.4 1.5h6a1 1 0 0 1 1 1v7.5a1 1 0 0 1-1 1h-11a1 1 0 0 1-1-1z"/></svg>';
    row.append(chevron, icon, name);
    const nested = document.createElement('ul');
    nested.className = 'children';
    li.append(nested);
    let loading = null;
    const dir = {
      async expand() {
        loading ??= fillDir(item.path, nested).catch(e => { nested.textContent = e.message; });
        await loading;
        li.classList.add('open');
      },
      toggle() { li.classList.contains('open') ? li.classList.remove('open') : dir.expand(); }
    };
    dirs.set(item.path, dir);
    row.onclick = dir.toggle;
  } else {
    row.append(chevron, iconElement(item), name);
    row.onclick = () => navigate(item.path);
  }
  return li;
}
const treeReady = fillDir('', tree).catch(e => { tree.textContent = e.message; });

async function reveal(path) {
  await treeReady;
  const parts = path.split('/');
  for (let i = 1; i < parts.length; i++) {
    const dir = dirs.get(parts.slice(0, i).join('/'));
    if (!dir) return;
    await dir.expand();
  }
  const row = rows.get(path);
  if (row) {
    select(row);
    row.scrollIntoView({ block: 'nearest' });
  }
}
function select(row) {
  document.querySelectorAll('.entry.selected').forEach(e => e.classList.remove('selected'));
  if (row) row.classList.add('selected');
}

// The file viewer, the opened file being in the URL fragment
function currentPath() {
  try { return decodeURIComponent(location.hash.slice(1)); } catch { return ''; }
}
function navigate(path) {
  if (path) location.hash = path.split('/').map(encodeURIComponent).join('/');
  else {
    history.pushState(null, '', location.pathname + location.search);
    route();
  }
}
function showMessage(text) {
  viewer.classList.toggle('has-message', !!text);
  $('message').textContent = text || '';
}
function refreshEditor() { requestAnimationFrame(() => editor.refresh()); }
function formatSize(n) {
  return n < 1024 ? n + ' B' : n < 1048576 ? (n / 1024).toFixed(1) + ' KB' : (n / 1048576).toFixed(1) + ' MB';
}

let generation = 0;
async function openFile(path) {
  const current = ++generation;
  $('path').textContent = path;
  $('path').title = path;
  $('language').textContent = '';
  document.title = path.split('/').pop() + ' — ' + rootName;
  showMessage('Loading…');
  let file;
  try {
    const response = await fetch('api/file?path=' + encodeURIComponent(path));
    if (!response.ok) throw new Error(await response.text());
    file = await response.json();
  } catch (e) {
    if (current === generation) showMessage(e.message);
    return;
  }
  if (current !== generation) return;
  if (file.binary) return showMessage(`Binary file (${formatSize(file.size)})`);
  if (file.tooLarge) return showMessage(`File too large to be displayed (${formatSize(file.size)})`);
  showMessage(null);
  const info = modeInfo(path.split('/').pop(), file.language, file.content);
  editor.setOption('mode', info ? (info.mime || info.mode) : null);
  if (info && info.mode && info.mode !== 'null') CodeMirror.autoLoadMode(editor, info.mode);
  $('language').textContent = info ? info.name : '';
  editor.setValue(file.content);
  editor.clearHistory();
  editor.scrollTo(0, 0);
  refreshEditor();
}

const phone = matchMedia('(max-width: 700px)');
function route() {
  const path = currentPath();
  if (!path) {
    generation++;
    body.className = 'no-file';
    document.title = rootName;
    select(null);
    return;
  }
  // The file tree stays where it is when opening a file, except on phones, where it covers the file
  const folded = phone.matches || (body.classList.contains('file-open') && body.classList.contains('folded'));
  body.className = 'file-open' + (folded ? ' folded' : '');
  openFile(path);
  reveal(path);
}
window.addEventListener('hashchange', route);
$('close').onclick = () => navigate('');
$('toggle').onclick = () => { body.classList.toggle('folded'); };
$('sidebar').addEventListener('transitionend', e => { if (e.propertyName === 'width') editor.refresh(); });
document.addEventListener('keydown', e => {
  if (e.key === 'Escape' && body.classList.contains('file-open')) {
    if (!body.classList.contains('folded')) body.classList.add('folded');
    else navigate('');
  }
});
route();
})();
"""
}
