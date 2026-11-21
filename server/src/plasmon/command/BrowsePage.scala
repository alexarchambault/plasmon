package plasmon.command

import scalatags.Text.all.*
import scalatags.Text.tags2

/** The single page of `plasmon browse`
  *
  * It only uses URLs relative to its own for the API (`api/tree`, `api/file`, …), so that it works
  * behind proxies keeping a path prefix. CodeMirror (syntax coloring), Devicon (file icons), marked
  * and DOMPurify (hover contents, in Markdown) come from CDNs.
  */
private[command] object BrowsePage {

  private val codeMirror = "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.18"
  private val devicon    = "https://cdn.jsdelivr.net/gh/devicons/devicon@v2.16.0/devicon.min.css"
  private val marked     = "https://cdnjs.cloudflare.com/ajax/libs/marked/15.0.12/marked.min.js"
  private val domPurify  = "https://cdnjs.cloudflare.com/ajax/libs/dompurify/3.2.6/purify.min.js"

  def render(rootName: String): String =
    doctype("html")(
      html(
        head(
          meta(charset := "utf-8"),
          meta(name    := "viewport", content := "width=device-width, initial-scale=1"),
          tags2.title(rootName),
          link(rel := "stylesheet", href := s"$codeMirror/codemirror.min.css"),
          link(rel := "stylesheet", href := s"$codeMirror/theme/material-darker.min.css"),
          link(rel := "stylesheet", href := devicon),
          tags2.style(raw(css))
        ),
        body(
          cls                     := "no-file",
          attr("data-root")       := rootName,
          attr("data-codemirror") := codeMirror
        )(
          div(id := "app")(
            tags2.aside(id := "sidebar")(
              h1(id := "root-name", rootName),
              ul(id := "tree")
            ),
            tags2.main(id := "viewer")(
              div(id := "header")(
                button(
                  id                 := "toggle",
                  attr("title")      := "Show / hide files",
                  attr("aria-label") := "Show / hide files"
                )(
                  raw(sidebarIcon)
                ),
                span(id := "path"),
                span(id := "language"),
                button(
                  id             := "diag-count",
                  attr("hidden") := "",
                  attr("title")  := "Go to the next problem"
                ),
                button(
                  id                 := "smaller",
                  cls                := "font-size",
                  attr("title")      := "Smaller text",
                  attr("aria-label") := "Smaller text"
                )(
                  "A",
                  sub("−")
                ),
                button(
                  id                 := "larger",
                  cls                := "font-size",
                  attr("title")      := "Larger text",
                  attr("aria-label") := "Larger text"
                )(
                  "A",
                  sub("+")
                ),
                button(
                  id                 := "close",
                  attr("title")      := "Close file",
                  attr("aria-label") := "Close file"
                )("✕")
              ),
              div(id := "editor"),
              div(id := "message"),
              div(id := "hoverbox", attr("hidden") := "")
            ),
            div(id := "log", attr("hidden") := "")(
              div(id := "log-header")(
                tag("select")(
                  id                 := "log-channel",
                  attr("title")      := "Log to show",
                  attr("aria-label") := "Log to show"
                ),
                span(id := "log-info"),
                button(
                  id                 := "log-follow",
                  cls                := "on",
                  attr("title")      := "Follow new lines",
                  attr("aria-label") := "Follow new lines"
                )(raw(followIcon)),
                button(
                  id                 := "log-expand",
                  attr("title")      := "Expand / shrink the log",
                  attr("aria-label") := "Expand / shrink the log"
                )(raw(expandIcon)),
                button(
                  id                 := "log-close",
                  attr("title")      := "Hide the log",
                  attr("aria-label") := "Hide the log"
                )("✕")
              ),
              pre(id := "log-lines")
            )
          ),
          div(id := "statusbar")(
            button(id := "status-main", attr("title") := "Show / hide the log")(
              span(cls := "spinner"),
              span(id  := "status-text", "Ready")
            ),
            div(id := "status-items"),
            button(
              id                 := "log-toggle",
              attr("title")      := "Show / hide the logs",
              attr("aria-label") := "Show / hide the logs"
            )(raw(logIcon))
          ),
          script(src := s"$codeMirror/codemirror.min.js"),
          script(src := s"$codeMirror/mode/meta.min.js"),
          script(src := s"$codeMirror/addon/mode/loadmode.min.js"),
          script(src := s"$codeMirror/addon/runmode/runmode.min.js"),
          script(src := marked),
          script(src := domPurify),
          script(raw(js))
        )
      )
    ).render

  private def sidebarIcon =
    """<svg width="18" height="18" viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.2"><rect x="1.5" y="2.5" width="13" height="11" rx="1.5"/><line x1="6" y1="2.5" x2="6" y2="13.5"/></svg>"""

  private def logIcon =
    """<svg width="14" height="14" viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.2"><rect x="1.5" y="2" width="13" height="12" rx="1.5"/><line x1="4" y1="5.5" x2="12" y2="5.5"/><line x1="4" y1="8" x2="12" y2="8"/><line x1="4" y1="10.5" x2="9" y2="10.5"/></svg>"""
  private def followIcon =
    """<svg width="14" height="14" viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.3"><line x1="8" y1="2" x2="8" y2="11"/><polyline points="4,7.5 8,11.5 12,7.5"/><line x1="3" y1="14" x2="13" y2="14"/></svg>"""
  private def expandIcon =
    """<svg width="14" height="14" viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.3"><polyline points="4,7 8,3 12,7"/><polyline points="4,13 8,9 12,13"/></svg>"""

  private def css =
    """
:root {
  --bg: #1e1e1e; --side: #181818; --border: #2b2b2b; --fg: #cccccc; --muted: #8b8b8b;
  --hover: #2a2d2e; --selected: #37373d; --sidebar-width: 300px;
  /* size of the code, of the file path, and of hover contents, changed with the A-/A+ buttons */
  --font-size: 13px;
  --mono: ui-monospace, SFMono-Regular, Menlo, Consolas, "Liberation Mono", monospace;
}
* { box-sizing: border-box }
html, body { height: 100%; margin: 0 }
body { display: flex; flex-direction: column; overflow: hidden; color: var(--fg); background: var(--bg); font: 13px system-ui, -apple-system, "Segoe UI", sans-serif }
button { display: flex; align-items: center; justify-content: center; width: 30px; height: 30px; padding: 0; border: 0; border-radius: 5px; color: var(--muted); background: none; font: inherit; cursor: pointer }
button:hover { color: var(--fg); background: var(--hover) }

#app { display: flex; flex: 1; min-height: 0; position: relative }
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
.chevron { flex: none; width: 16px; color: var(--muted); font-size: 10px; text-align: center; transition: transform .1s }
.open > .entry .chevron { transform: rotate(90deg) }
.icon { flex: none; width: 16px; font-size: 15px; text-align: center; color: var(--muted) }
/* Devicon colors too dark for a dark background */
.icon.colored:is(.devicon-markdown-plain, .devicon-json-plain, .devicon-rust-plain, .devicon-bash-plain, .devicon-perl-plain) { color: #c5c5c5 }
.name { overflow: hidden; text-overflow: ellipsis }
/* ignored by Git */
.entry.ignored { color: #6b6b6b }
.entry.ignored .icon { opacity: .45 }
.children { display: none } .open > .children { display: block }
.empty { padding: 4px 24px; color: var(--muted); font-style: italic }

#viewer { display: flex; flex: 1; min-width: 0; flex-direction: column; position: relative }
body.no-file :is(#header, #editor, #message) { display: none !important }
#header { display: flex; flex: none; align-items: center; gap: 8px; height: 38px; padding: 0 4px; border-bottom: 1px solid var(--border); white-space: nowrap }
#path { flex: 1; min-width: 0; overflow: hidden; text-overflow: ellipsis; font-family: var(--mono); font-size: var(--font-size) }
button.font-size { flex: none; width: 26px; color: var(--muted); font-size: 13px }
button.font-size sub { font-size: 10px; vertical-align: -1px }
#language { color: var(--muted); font-size: 12px }
#editor { flex: 1; min-height: 0 }
#message { display: none; position: absolute; inset: 38px 0 0; padding: 24px; color: var(--muted); background: var(--bg) }
#viewer.has-message #message { display: block }
.CodeMirror { height: 100%; font: var(--font-size)/1.5 var(--mono) }
.CodeMirror-code { -webkit-user-select: text; user-select: text }
.cm-s-material-darker.CodeMirror, .cm-s-material-darker .CodeMirror-gutters { background: var(--bg) }
.cm-s-material-darker .CodeMirror-gutters { border-right: 1px solid var(--border) }
.cm-s-material-darker .CodeMirror-linenumber { min-width: 0; padding: 0 6px 0 2px; color: #5a5a5a }
.CodeMirror .diag-error { text-decoration: underline wavy #f14c4c; text-decoration-skip-ink: none; text-underline-offset: 3px }
.CodeMirror .diag-warning { text-decoration: underline wavy #cca700; text-decoration-skip-ink: none; text-underline-offset: 3px }
.CodeMirror .diag-information, .CodeMirror .diag-hint { text-decoration: underline dotted #3794ff; text-underline-offset: 3px }
.diagnostics-gutter { width: 8px }
.diag-dot { width: 6px; height: 6px; margin: calc(.75em - 3px) 0 0 1px; border-radius: 50%; background: #3794ff }
.diag-dot.error { background: #f14c4c } .diag-dot.warning { background: #cca700 }
#diag-count { display: flex; gap: 8px; width: auto; padding: 0 8px; font-size: 12px; white-space: nowrap }
#diag-count[hidden] { display: none }
#diag-count .errors { color: #f48771 } #diag-count .warnings { color: #cca700 }
#hoverbox .problems { padding: 8px 10px; border-bottom: 1px solid #454545; font: var(--font-size)/1.45 var(--mono); white-space: pre-wrap; overflow-wrap: anywhere }
#hoverbox .problems > div + div { margin-top: 6px }
#hoverbox .problems .error { color: #f48771 } #hoverbox .problems .warning { color: #cca700 } #hoverbox .problems .information, #hoverbox .problems .hint { color: #75beff }
.hover-target { background: rgba(255, 255, 255, .1); border-radius: 2px }
.flash { background: rgba(255, 213, 0, .25); border-radius: 2px }

#hoverbox { position: absolute; z-index: 20; width: min(560px, calc(100% - 16px)); max-height: min(60%, 420px); display: flex; flex-direction: column; color: var(--fg); background: #252526; border: 1px solid #454545; border-radius: 4px; box-shadow: 0 4px 16px rgba(0, 0, 0, .45) }
#hoverbox[hidden] { display: none }
#hoverbox .contents { overflow: auto; padding: 8px 10px; font-size: var(--font-size); line-height: 1.45; overflow-wrap: anywhere }
#hoverbox .contents > :first-child { margin-top: 0 } #hoverbox .contents > :last-child { margin-bottom: 0 }
#hoverbox .contents p { margin: 6px 0 }
#hoverbox .contents hr { margin: 8px -10px; border: 0; border-top: 1px solid #454545 }
#hoverbox .contents pre { margin: 4px 0; white-space: pre-wrap; font: 1em/1.45 var(--mono) }
#hoverbox .contents code { font: 1em var(--mono) }
#hoverbox .contents :not(pre) > code { padding: 1px 3px; background: rgba(255, 255, 255, .07); border-radius: 3px }
#hoverbox .contents a { color: #3794ff }
#hoverbox .contents ul { padding-left: 20px }
#hoverbox .muted { color: var(--muted) }
#hoverbox .error { color: #f48771 }
#hoverbox .actions { display: flex; gap: 6px; padding: 6px 8px; border-top: 1px solid #454545 }
#hoverbox .actions button, #hoverbox .locations button { width: auto; height: 26px; padding: 0 10px; color: #fff; background: #0e639c; border-radius: 3px; font-size: 12px }
#hoverbox .actions button:hover { background: #1177bb }
#hoverbox .actions button:disabled { opacity: .6; cursor: default }
button.font-size:disabled { opacity: .35; cursor: default; background: none }
#hoverbox .locations { display: flex; flex-direction: column; gap: 2px; padding: 4px 8px 8px }
#hoverbox .locations .title { padding: 2px 2px 4px; color: var(--muted) }
#hoverbox .locations button { justify-content: flex-start; width: 100%; color: var(--fg); background: none; font-family: var(--mono); text-align: left; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; display: block }
#hoverbox .locations button:hover { background: var(--selected) }

/* the status bar: what's going on on the left, the status items of the server on the right */
#statusbar { display: flex; flex: none; align-items: stretch; height: 24px; color: var(--muted); background: var(--side); border-top: 1px solid var(--border); font-size: 12px; white-space: nowrap; user-select: none }
#statusbar button { width: auto; height: auto; gap: 6px; padding: 0 8px; border-radius: 0; font-size: 12px }
#status-main { flex: 1 1 0; min-width: 0; justify-content: flex-start; padding-left: 10px !important }
#status-main.error { color: #f48771 }
#status-text, .item-text { overflow: hidden; text-overflow: ellipsis }
#status-items { display: flex; flex: 0 1 auto; min-width: 0; max-width: 65%; justify-content: flex-end; overflow: hidden }
#status-items button { flex: 0 1 auto; min-width: 0 }
#status-items .label { color: #6f6f6f }
#status-items button:hover .label { color: var(--muted) }
#status-items .warning { color: #cca700 } #status-items .error { color: #f48771 }
#log-toggle { flex: none; padding: 0 10px !important }
body.log-open #log-toggle { color: var(--fg); background: var(--selected) }
.spinner { display: none; flex: none; width: 11px; height: 11px; border: 2px solid rgba(128, 128, 128, .35); border-top-color: currentColor; border-radius: 50%; animation: spin .8s linear infinite }
.busy > .spinner { display: block }
@keyframes spin { to { transform: rotate(360deg) } }
@media (prefers-reduced-motion: reduce) { .spinner { animation-duration: 2.4s } }

/* the logs, one at a time, like the output channels of VS Code */
#log { position: absolute; z-index: 30; left: 0; right: 0; bottom: 0; height: min(45%, 360px); display: flex; flex-direction: column; background: #181818; border-top: 1px solid var(--border); box-shadow: 0 -4px 16px rgba(0, 0, 0, .35) }
#log.expanded { height: 100% }
#log[hidden] { display: none }
#log-header { display: flex; flex: none; align-items: center; gap: 4px; height: 32px; padding: 0 4px 0 8px; border-bottom: 1px solid var(--border) }
#log-header button { flex: none; width: 26px; height: 26px }
#log-follow.on { color: var(--fg); background: var(--selected) }
#log-channel { min-width: 0; max-width: 60%; height: 24px; padding: 0 6px; color: var(--fg); background: #252526; border: 1px solid #3c3c3c; border-radius: 4px; font: 12px system-ui, -apple-system, "Segoe UI", sans-serif }
#log-info { flex: 1; min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; color: var(--muted); font-size: 11px }
#log-lines { flex: 1; min-height: 0; overflow: auto; margin: 0; padding: 6px 10px; color: #bbb; font: 12px/1.45 var(--mono); white-space: pre-wrap; overflow-wrap: anywhere; -webkit-user-select: text; user-select: text }
#log-lines .gap { color: var(--muted); font-style: italic }

/* phones: the unfolded file tree goes over the file */
@media (max-width: 700px) {
  /* no file: the file tree takes the whole page */
  body.no-file #viewer { display: none }
  body.no-file #sidebar { flex: 1; width: auto; border-right: 0; background: var(--bg) }
  body.no-file .entry { height: 30px; font-size: 14px }
  /* below the file header, so that its button can fold the tree back */
  :root { --sidebar-width: 100vw }
  body.file-open #sidebar { position: absolute; z-index: 10; top: 38px; bottom: 0; left: 0; border-top: 1px solid var(--border); box-shadow: 0 6px 16px rgba(0, 0, 0, .5) }
  /* room for what's going on: the status items keep their spinner and their color only */
  #status-items .label { display: none }
  #statusbar { height: 28px }
  #log { height: 60% }
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
  lineNumbers: true, theme: 'material-darker', value: '', viewportMargin: 30,
  gutters: ['diagnostics-gutter', 'CodeMirror-linenumbers']
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
  row.className = item.ignored ? 'entry ignored' : 'entry'; chevron.className = 'chevron'; name.className = 'name';
  name.textContent = item.name; row.title = item.path;
  rows.set(item.path, row);
  li.append(row);
  if (item.dir) {
    chevron.textContent = '▶';
    row.append(chevron, name);
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
    // the icon of files where folders have their chevron, so that names are aligned
    row.append(iconElement(item), name);
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
// `#path`, or `#path:line:col` (one-based) to show a position in it
function currentLocation() {
  let hash;
  try { hash = decodeURIComponent(location.hash.slice(1)); } catch { hash = ''; }
  const m = /^(.*):(\d+):(\d+)$/.exec(hash);
  return m ? { path: m[1], pos: { line: +m[2] - 1, ch: +m[3] - 1 } } : { path: hash, pos: null };
}
function navigate(path, pos) {
  if (path) location.hash = path.split('/').map(encodeURIComponent).join('/') + (pos ? `:${pos.line + 1}:${pos.ch + 1}` : '');
  else {
    history.pushState(null, '', location.pathname + location.search);
    route();
  }
}
function showMessage(text) {
  viewer.classList.toggle('has-message', !!text);
  $('message').textContent = text || '';
}
function sleep(ms) { return new Promise(resolve => setTimeout(resolve, ms)); }
function refreshEditor() { requestAnimationFrame(() => editor.refresh()); }
function formatSize(n) {
  return n < 1024 ? n + ' B' : n < 1048576 ? (n / 1024).toFixed(1) + ' KB' : (n / 1048576).toFixed(1) + ' MB';
}

// Which file the editor shows, once it's loaded
let shownFile = null;
let generation = 0;
async function openFile(path) {
  shownFile = null;
  closeHover();
  clearDiagnostics();
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
    return false;
  }
  if (current !== generation) return false;
  if (file.binary) return showMessage(`Binary file (${formatSize(file.size)})`), false;
  if (file.tooLarge) return showMessage(`File too large to be displayed (${formatSize(file.size)})`), false;
  showMessage(null);
  const info = modeInfo(path.split('/').pop(), file.language, file.content);
  editor.setOption('mode', info ? (info.mime || info.mode) : null);
  if (info && info.mode && info.mode !== 'null') CodeMirror.autoLoadMode(editor, info.mode);
  $('language').textContent = info ? info.name : '';
  editor.setValue(file.content);
  editor.clearHistory();
  editor.scrollTo(0, 0);
  refreshEditor();
  shownFile = path;
  // The status items about it, if the server sent them already
  if (lastStatus) showStatus(lastStatus);
  // Load the build tool and module of the file, like `--auto` does on the command line
  fetch('api/load?path=' + encodeURIComponent(path)).catch(() => {});
  // Then compile it with the presentation compiler (run after the load, on the server side)
  loadDiagnostics(path);
  return true;
}

// Shows a position (zero-based) in the file shown, flashing the word there
function showPosition(pos) {
  editor.refresh();
  const word = editor.findWordAt(pos);
  editor.scrollIntoView({ from: { line: Math.max(0, pos.line - 5), ch: 0 }, to: { line: pos.line + 10, ch: 0 } });
  const top = editor.charCoords({ line: pos.line, ch: 0 }, 'local').top;
  editor.scrollTo(null, Math.max(0, top - editor.getScrollInfo().clientHeight / 3));
  const mark = editor.markText(word.anchor, word.head, { className: 'flash' });
  setTimeout(() => mark.clear(), 1500);
}

// The diagnostics of the presentation compiler for the file shown
let diagnostics = [], diagnosticMarks = [];
const severityOrder = { error: 0, warning: 1, information: 2, hint: 3 };
function clearDiagnostics() {
  diagnosticMarks.forEach(mark => mark.clear());
  diagnosticMarks = [];
  diagnostics = [];
  editor.clearGutter('diagnostics-gutter');
  $('diag-count').hidden = true;
}
async function loadDiagnostics(path) {
  let res;
  try {
    const response = await fetch('api/diagnostics?path=' + encodeURIComponent(path));
    if (!response.ok) throw new Error(await response.text());
    res = await response.json();
  } catch (e) {
    return;
  }
  if (shownFile !== path || res.error) return;
  showDiagnostics(res.diagnostics || []);
}
function showDiagnostics(diags) {
  clearDiagnostics();
  diagnostics = diags.slice().sort((a, b) => a.line - b.line || a.col - b.col);
  const worstPerLine = new Map();
  for (const d of diagnostics) {
    const from = { line: d.line, ch: d.col };
    let to = { line: d.endLine, ch: d.endCol };
    // Empty range: the word there
    if (to.line === from.line && to.ch <= from.ch) to = editor.findWordAt(from).head;
    if (to.line === from.line && to.ch <= from.ch) to = { line: from.line, ch: from.ch + 1 };
    diagnosticMarks.push(editor.markText(from, to, { className: 'diag-' + d.severity }));
    const worst = worstPerLine.get(d.line);
    if (!worst || severityOrder[d.severity] < severityOrder[worst.severity]) worstPerLine.set(d.line, d);
  }
  for (const [line, d] of worstPerLine) {
    const dot = document.createElement('div');
    dot.className = 'diag-dot ' + d.severity;
    dot.title = diagnostics.filter(other => other.line === line).map(other => other.message).join('\n\n');
    editor.setGutterMarker(line, 'diagnostics-gutter', dot);
  }
  const errors = diagnostics.filter(d => d.severity === 'error').length;
  const warnings = diagnostics.filter(d => d.severity === 'warning').length;
  const count = $('diag-count');
  count.replaceChildren();
  if (errors) count.insertAdjacentHTML('beforeend', `<span class="errors">✕ ${errors}</span>`);
  if (warnings) count.insertAdjacentHTML('beforeend', `<span class="warnings">⚠ ${warnings}</span>`);
  count.hidden = !diagnostics.length;
}
let nextDiagnostic = 0;
$('diag-count').onclick = () => {
  if (!diagnostics.length) return;
  const d = diagnostics[nextDiagnostic++ % diagnostics.length];
  showPosition({ line: d.line, ch: d.col });
};
function diagnosticsAt(pos) {
  return diagnostics.filter(d => {
    const afterStart = pos.line > d.line || (pos.line === d.line && pos.ch >= d.col);
    const endLine = Math.max(d.endLine, d.line), endCol = d.endLine > d.line ? d.endCol : Math.max(d.endCol, d.col + 1);
    const beforeEnd = pos.line < endLine || (pos.line === endLine && pos.ch <= endCol);
    return afterStart && beforeEnd;
  });
}
function problemsElement(diags) {
  const div = document.createElement('div');
  div.className = 'problems';
  for (const d of diags) {
    const item = document.createElement('div');
    item.className = d.severity;
    item.textContent = d.message;
    div.append(item);
  }
  return div;
}

// The hover box, shown when clicking in the text
const hoverbox = $('hoverbox');
let hoverGeneration = 0, hoverMark = null, hoverAbort = null;
function closeHover() {
  hoverGeneration++;
  if (hoverAbort) { hoverAbort.abort(); hoverAbort = null; }
  hoverbox.hidden = true;
  if (hoverMark) { hoverMark.clear(); hoverMark = null; }
}
function placeHover(pos) {
  const v = viewer.getBoundingClientRect(), c = editor.charCoords(pos, 'window');
  const width = hoverbox.offsetWidth, height = hoverbox.offsetHeight;
  const left = Math.min(Math.max(8, c.left - v.left - 12), v.width - width - 8);
  const below = c.bottom - v.top + 4, above = c.top - v.top - height - 4;
  hoverbox.style.left = left + 'px';
  hoverbox.style.top = (below + height > v.height - 4 && above > 40 ? above : below) + 'px';
}
function highlightCode(root) {
  for (const code of root.querySelectorAll('pre code')) {
    const lang = (/language-(\S+)/.exec(code.className) || [])[1];
    const info = lang && (CodeMirror.findModeByName(lang) || CodeMirror.findModeByExtension(lang));
    if (!info || !info.mode || info.mode === 'null') continue;
    const text = code.textContent;
    CodeMirror.requireMode(info.mode, () => {
      code.classList.add('cm-s-material-darker');
      CodeMirror.runMode(text, info.mime || info.mode, code);
    });
  }
}
function renderMarkdown(markdown) {
  const div = document.createElement('div');
  div.className = 'contents';
  div.innerHTML = DOMPurify.sanitize(marked.parse(markdown));
  for (const a of div.querySelectorAll('a')) {
    if (/^https?:/i.test(a.getAttribute('href') || '')) {
      a.target = '_blank';
      a.rel = 'noopener';
    } else {
      // Relative links (Javadoc's `{@link Foo}`, say) would point at nothing here
      const code = document.createElement('code');
      code.textContent = a.textContent;
      a.replaceWith(code);
    }
  }
  highlightCode(div);
  return div;
}
// While a hover waits for what the server is doing (loading a module, typically), what that is
function pendingHoverText() {
  return lastSummary.busy ? `Waiting: ${lastSummary.text}` : 'Loading…';
}
function pendingHoverMessage() {
  const div = hoverMessage(pendingHoverText());
  div.classList.add('pending');
  return div;
}
function hoverMessage(text, cls = 'muted') {
  const div = document.createElement('div');
  div.className = 'contents ' + cls;
  div.textContent = text;
  return div;
}
async function showHover(path, pos) {
  const current = ++hoverGeneration;
  if (hoverAbort) hoverAbort.abort();
  const abort = hoverAbort = new AbortController();
  if (hoverMark) hoverMark.clear();
  const word = editor.findWordAt(pos);
  hoverMark = editor.markText(word.anchor, word.head, { className: 'hover-target' });
  hoverbox.replaceChildren(pendingHoverMessage());
  hoverbox.hidden = false;
  placeHover(pos);
  const query = `path=${encodeURIComponent(path)}&line=${pos.line}&col=${pos.ch}`;
  let res;
  try {
    const response = await fetch('api/hover?' + query, { signal: abort.signal });
    if (!response.ok) throw new Error(await response.text());
    res = await response.json();
  } catch (e) {
    res = { error: e.message };
  }
  if (current !== hoverGeneration || res.superseded) return;
  hoverAbort = null;
  const problems = diagnosticsAt(pos);
  const contents = res.error ? hoverMessage(res.error, 'error') :
    res.markdown ? renderMarkdown(res.markdown) :
    problems.length ? null : hoverMessage('No information');
  const actions = document.createElement('div');
  actions.className = 'actions';
  const button = document.createElement('button');
  button.textContent = 'Definition';
  button.onclick = () => goToDefinition(path, pos, query, current, button);
  actions.append(button);
  hoverbox.replaceChildren(...[problems.length ? problemsElement(problems) : null, contents, actions].filter(Boolean));
  placeHover(pos);
}
async function goToDefinition(path, pos, query, current, button) {
  button.disabled = true;
  button.textContent = 'Definition…';
  let res;
  try {
    const response = await fetch('api/definition?' + query);
    if (!response.ok) throw new Error(await response.text());
    res = await response.json();
  } catch (e) {
    res = { error: e.message };
  }
  if (current !== hoverGeneration) return;
  button.disabled = false;
  button.textContent = 'Definition';
  const locations = res.locations || [];
  if (res.error || !locations.length) {
    hoverbox.querySelector('.locations')?.remove();
    hoverbox.querySelector('.contents')?.remove();
    hoverbox.querySelector('.actions').before(hoverMessage(res.error || 'No definition found', res.error ? 'error' : 'muted'));
    placeHover(pos);
    return;
  }
  if (locations.length === 1) return goTo(locations[0]);
  // Several of them: let users pick one
  const list = document.createElement('div');
  list.className = 'locations';
  const title = document.createElement('div');
  title.className = 'title';
  title.textContent = `${locations.length} definitions`;
  list.append(title);
  for (const loc of locations) {
    const item = document.createElement('button');
    item.textContent = `${loc.path || loc.uri}:${loc.line + 1}`;
    item.title = item.textContent;
    item.onclick = () => goTo(loc);
    list.append(item);
  }
  hoverbox.querySelector('.locations')?.remove();
  hoverbox.querySelector('.actions').remove();
  hoverbox.append(list);
  placeHover(pos);
}
function goTo(loc) {
  if (!loc.path) {
    hoverbox.querySelector('.contents')?.remove();
    hoverbox.append(hoverMessage(`Not under ${rootName}: ${loc.uri}`, 'error'));
    return;
  }
  closeHover();
  navigate(loc.path, { line: loc.line, ch: loc.col });
}
// Taps and clicks in the text, from pointer events: on touch screens, CodeMirror cancels the
// touchend of short taps (preventDefault), so that no click follows them
let pointerDown = null;
editor.getWrapperElement().addEventListener('pointerdown', e => {
  pointerDown = e.isPrimary ? { x: e.clientX, y: e.clientY, time: Date.now() } : null;
}, true);
editor.getWrapperElement().addEventListener('pointerup', e => {
  const down = pointerDown;
  pointerDown = null;
  // Not a tap / click: scrolling, selecting text, long presses
  if (!down || !e.isPrimary || Math.hypot(e.clientX - down.x, e.clientY - down.y) > 8 || Date.now() - down.time > 600) return;
  onTap(e);
}, true);
editor.getWrapperElement().addEventListener('pointercancel', () => { pointerDown = null; }, true);
function onTap(e) {
  if (!shownFile || e.target.closest('.CodeMirror-gutters, .CodeMirror-gutter-wrapper')) return;
  // Selecting text with the mouse, not asking for a hover
  if (e.pointerType === 'mouse' && String(window.getSelection()).length) return;
  const pos = editor.coordsChar({ left: e.clientX, top: e.clientY }, 'window');
  const lineEnd = editor.charCoords({ line: pos.line, ch: editor.getLine(pos.line).length }, 'window');
  if (pos.outside || e.clientX > lineEnd.right + 2 || !/\S/.test(editor.getLine(pos.line).charAt(pos.ch) + editor.getLine(pos.line).charAt(pos.ch - 1))) {
    closeHover();
    return;
  }
  showHover(shownFile, pos);
}
editor.on('scroll', () => { if (!hoverbox.hidden) closeHover(); });
document.addEventListener('pointerdown', e => {
  if (!hoverbox.hidden && !hoverbox.contains(e.target) && !editor.getWrapperElement().contains(e.target)) closeHover();
});

// The status bar and the logs, from what the server says it's up to (long-polling)
const log = $('log'), logLines = $('log-lines'), logChannel = $('log-channel');
// The log shown, its next line, and a count of what was asked of the server since it changed
let shownLog = 'browse', nextLine = 0, logGeneration = 0, follow = true, logNodes = [];
try { shownLog = localStorage.getItem('plasmon-browse-log') || 'browse'; } catch {}
// Changing what to show asks for the status right away, without waiting for it to change
let pollAbort = null, pollSeq = -1;
function repoll() {
  pollSeq = -1;
  if (pollAbort) pollAbort.abort();
}
function showLog(id) {
  if (id && id !== shownLog) {
    shownLog = id;
    try { localStorage.setItem('plasmon-browse-log', id); } catch {}
    logChannel.value = id;
    resetLog();
  }
  if (log.hidden) {
    log.hidden = false;
    body.classList.add('log-open');
    resetLog();
  }
}
function hideLog() {
  log.hidden = true;
  body.classList.remove('log-open');
  repoll();
}
function toggleLog() { log.hidden ? showLog() : hideLog(); }
function resetLog() {
  logGeneration++;
  nextLine = 0;
  logNodes = [];
  logLines.replaceChildren();
  repoll();
}
function setFollow(value) {
  follow = value;
  $('log-follow').classList.toggle('on', follow);
  if (follow) logLines.scrollTop = logLines.scrollHeight;
}
function appendLines(fromLine, lines) {
  if (fromLine > nextLine && nextLine > 0) {
    const gap = document.createElement('span');
    gap.className = 'gap';
    gap.textContent = `… ${fromLine - nextLine} lines dropped\n`;
    logLines.append(gap);
    logNodes.push({ node: gap, count: 1 });
  }
  if (!lines.length) return;
  const node = document.createTextNode(lines.join('\n') + '\n');
  logLines.append(node);
  logNodes.push({ node, count: lines.length });
  // Only the last lines, so that the page stays light
  let total = logNodes.reduce((n, e) => n + e.count, 0);
  while (total > 20000 && logNodes.length > 1) {
    const first = logNodes.shift();
    first.node.remove();
    total -= first.count;
  }
  if (follow) logLines.scrollTop = logLines.scrollHeight;
}
logLines.addEventListener('scroll', () => {
  const atBottom = logLines.scrollTop + logLines.clientHeight >= logLines.scrollHeight - 4;
  if (atBottom !== follow) setFollow(atBottom);
});
logChannel.onchange = () => showLog(logChannel.value);
$('log-follow').onclick = () => setFollow(!follow);
$('log-expand').onclick = () => log.classList.toggle('expanded');
$('log-close').onclick = hideLog;
$('log-toggle').onclick = toggleLog;
$('status-main').onclick = toggleLog;

function updateChannels(channels) {
  const known = [...logChannel.options].map(o => o.value + '\u0000' + o.textContent).join('\u0001');
  if (known !== channels.map(c => c.id + '\u0000' + c.label).join('\u0001')) {
    logChannel.replaceChildren(...channels.map(c => {
      const option = document.createElement('option');
      option.value = c.id; option.textContent = c.label;
      return option;
    }));
  }
  if (!channels.some(c => c.id === shownLog)) shownLog = 'browse';
  logChannel.value = shownLog;
}

// What the status items of the server say, for the file shown. The ones about the file only when
// the server's focused file is that one (it's the last file opened, from any page).
const fileItems = new Set(['plasmon.build-tool', 'plasmon.compiler', 'plasmon.interactive']);
// What tells what an item is about, when its text doesn't: the presentation compiler ("Idle", …)
function itemLabel(item) {
  if (item.id !== 'plasmon.interactive') return null;
  return (item.detail || '').replace(/ Interactive$/, '') || 'Interactive';
}
const severities = ['', 'warning', 'error'];
function visibleItems(status) {
  return status.items.filter(item => {
    if (item.id === 'plasmon.summary' || !item.text) return false;
    if (fileItems.has(item.id) && status.file !== shownFile) return false;
    // Modules are read from their sources, nothing gets compiled
    if (item.id === 'plasmon.compiler') return false;
    // Build tools are stopped once they said what the modules are (their data is replayed): the
    // errors that follow aren't worth showing, while loading or after
    if (item.id === 'plasmon.build-tool') return item.busy || !item.severity;
    // Nothing to say before it's needed
    if (item.id === 'plasmon.interactive') return item.text !== 'Not instantiated';
    // Automatic mode isn't what loads things here: only when it's loading something
    if (item.id === 'plasmon.mode') return item.busy;
    return true;
  });
}
function itemButton(item, channels) {
  const button = document.createElement('button');
  button.className = [severities[item.severity] || '', item.busy ? 'busy' : ''].join(' ');
  const spinner = document.createElement('span');
  spinner.className = 'spinner';
  const text = document.createElement('span');
  text.className = 'item-text';
  const label = itemLabel(item);
  if (label) {
    const l = document.createElement('span');
    l.className = 'label';
    l.textContent = label + ' ';
    text.append(l);
  }
  text.append(item.text);
  button.append(spinner, text);
  const channel = item.log && channels.find(c => c.id === item.log);
  button.title = [label ? `${label}: ${item.text}` : item.text, label ? '' : item.detail, channel ? `Click to show the ${channel.label} log` : ''].filter(Boolean).join('\n');
  button.onclick = () => channel ? showLog(channel.id) : toggleLog();
  return button;
}
// What the status bar shows on its left, and hovers waiting for the server
let lastSummary = { busy: false, text: 'Ready', error: false };
function summaryOf(status, items) {
  const serverBusy = items.find(item => item.busy);
  if (status.busy) return { busy: true, text: status.text, error: false };
  if (status.tasks.length) return { busy: true, text: status.tasks[status.tasks.length - 1] + '…', error: false };
  if (serverBusy) return { busy: true, text: serverBusy.text, error: false };
  return { busy: false, text: status.text, error: status.error };
}
let lastStatus = null, shownItems = null;
function showStatus(status) {
  lastStatus = status;
  const items = visibleItems(status);
  const summary = lastSummary = summaryOf(status, items);
  const main = $('status-main');
  main.classList.toggle('busy', summary.busy);
  main.classList.toggle('error', summary.error);
  $('status-text').textContent = summary.text;
  main.title = summary.text + (status.tasks.length > 1 ? '\n' + status.tasks.join('\n') : '') + '\n(click to show / hide the log)';
  // Only when they changed, not to get in the way of clicks and tooltips
  const key = JSON.stringify([items, status.channels.map(c => c.id)]);
  if (key !== shownItems) {
    shownItems = key;
    $('status-items').replaceChildren(...items.map(item => itemButton(item, status.channels)));
  }
  updateChannels(status.channels);
  const pending = hoverbox.querySelector('.pending');
  if (pending) pending.textContent = pendingHoverText();
}
(async () => {
  for (;;) {
    const abort = pollAbort = new AbortController();
    const channel = log.hidden ? '' : shownLog, generation = logGeneration;
    try {
      const query = `since=${pollSeq}&log=${encodeURIComponent(channel)}&fromLine=${nextLine}`;
      const response = await fetch('api/status?' + query, { signal: abort.signal });
      if (!response.ok) throw new Error(await response.text());
      const status = await response.json();
      if (!abort.signal.aborted) pollSeq = status.seq;
      showStatus(status);
      if (channel && status.log === channel && generation === logGeneration) {
        appendLines(status.fromLine, status.lines);
        nextLine = status.fromLine + status.lines.length;
      }
      // A breather, for the logs of a busy server not to have the page poll non-stop
      await sleep(100);
    } catch (e) {
      // Something else to show: right away
      if (!abort.signal.aborted) await sleep(3000);
    }
  }
})();
// Editors tell the server about the file they show: so does this page, when it's back in front
function focusShownFile() {
  if (shownFile && document.visibilityState === 'visible')
    fetch('api/focus?path=' + encodeURIComponent(shownFile)).catch(() => {});
}
document.addEventListener('visibilitychange', focusShownFile);
window.addEventListener('focus', focusShownFile);

const phone = matchMedia('(max-width: 700px)');
function route() {
  const { path, pos } = currentLocation();
  if (!path) {
    generation++;
    shownFile = null;
    if (lastStatus) showStatus(lastStatus);
    closeHover();
    body.className = 'no-file' + (log.hidden ? '' : ' log-open');
    document.title = rootName;
    select(null);
    return;
  }
  // The file tree stays where it is when opening a file, except on phones, where it covers the file
  const folded = phone.matches || (body.classList.contains('file-open') && body.classList.contains('folded'));
  body.className = 'file-open' + (folded ? ' folded' : '') + (log.hidden ? '' : ' log-open');
  if (path === shownFile) {
    closeHover();
    if (pos) showPosition(pos);
  } else
    openFile(path).then(ok => { if (ok && pos) showPosition(pos); });
  reveal(path);
}
window.addEventListener('hashchange', route);
$('close').onclick = () => navigate('');
$('toggle').onclick = () => { body.classList.toggle('folded'); };

// Text size, remembered by the browser
const fontSizes = [9, 10, 11, 12, 13, 14, 15, 16, 18, 20, 22, 24, 28, 32];
let fontSize = 13;
try { fontSize = +localStorage.getItem('plasmon-browse-font-size') || 13; } catch {}
function setFontSize(size) {
  fontSize = Math.min(Math.max(size, fontSizes[0]), fontSizes[fontSizes.length - 1]);
  document.documentElement.style.setProperty('--font-size', fontSize + 'px');
  $('smaller').disabled = fontSize <= fontSizes[0];
  $('larger').disabled = fontSize >= fontSizes[fontSizes.length - 1];
  try { localStorage.setItem('plasmon-browse-font-size', fontSize); } catch {}
  closeHover();
  editor.refresh();
}
$('smaller').onclick = () => setFontSize(fontSizes.filter(s => s < fontSize).pop() ?? fontSize);
$('larger').onclick = () => setFontSize(fontSizes.find(s => s > fontSize) ?? fontSize);
setFontSize(fontSize);
$('sidebar').addEventListener('transitionend', e => { if (e.propertyName === 'width') editor.refresh(); });
document.addEventListener('keydown', e => {
  if (e.key === 'Escape' && !hoverbox.hidden) closeHover();
  else if (e.key === 'Escape' && !log.hidden) hideLog();
  else if (e.key === 'Escape' && body.classList.contains('file-open')) {
    if (!body.classList.contains('folded')) body.classList.add('folded');
    else navigate('');
  }
});
route();
})();
"""
}
