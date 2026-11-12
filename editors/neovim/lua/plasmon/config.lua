-- Plugin options, and the base LSP config handed to vim.lsp.config()

local M = {}

---@class plasmon.Options
---@field binary? string Plasmon launcher, `plasmon` from PATH by default
---@field server_args? string[] Extra arguments passed after `plasmon server`
---@field cmd? string[] Full command starting the server, replacing `binary` / `server_args`
---@field cmd_env? table<string, string> Extra environment variables for the server process
---@field on_build_change? "ask"|"reindex"|"ignore" What to do when the server detects a build change
---@field codelens? boolean Show code lenses (refreshed automatically)
---@field on_type_formatting? boolean Enable on-type formatting (Neovim >= 0.12 only)
---@field notify_progress? boolean Report Plasmon progress via LSP progress (`LspProgress`, `vim.lsp.status()`)
---@field max_log_lines? integer Lines kept per Plasmon log channel
---@field lsp? vim.lsp.Config Extra fields merged into the `plasmon` LSP config

---@type plasmon.Options
M.defaults = {
  binary = nil,
  server_args = {},
  cmd = nil,
  cmd_env = nil,
  on_build_change = "ask",
  codelens = true,
  on_type_formatting = true,
  notify_progress = true,
  max_log_lines = 10000,
  lsp = {},
}

---@type plasmon.Options
M.options = vim.deepcopy(M.defaults)

---@param opts? plasmon.Options
function M.set(opts)
  M.options = vim.tbl_deep_extend("force", vim.deepcopy(M.defaults), opts or {})
end

--- The command starting the server, as a list.
---@return string[]
function M.server_cmd()
  local opts = M.options
  if opts.cmd then
    return opts.cmd
  end
  local cmd = { opts.binary or "plasmon", "server" }
  vim.list_extend(cmd, opts.server_args or {})
  return cmd
end

-- Files whose presence marks the root of a build. When several nested directories have one,
-- the closest one wins, except for sbt, whose sub-projects can have a build.sbt of their own.
local build_markers = { "build.mill", "build.mill.yaml", "build.sc", "build.sbt" }
local scala_cli_markers = { "project.scala", ".scala-build", ".bsp" }

local function exists(path)
  return vim.uv.fs_stat(path) ~= nil
end

local function has_any(dir, markers)
  for _, marker in ipairs(markers) do
    if exists(vim.fs.joinpath(dir, marker)) then
      return true
    end
  end
  return false
end

--- Whether `dir` is the root of a git repository or worktree. Submodules don't count: their
--- files usually belong to the build of the enclosing repository.
local function is_repo_root(dir)
  local git = vim.fs.joinpath(dir, ".git")
  local stat = vim.uv.fs_stat(git)
  if not stat then
    return false
  end
  if stat.type == "directory" then
    return true
  end
  local f = io.open(git, "r")
  local content = f and f:read("*l") or ""
  if f then
    f:close()
  end
  return not content:find("/modules/", 1, true)
end

--- Workspace root for a file.
---
--- In order of preference:
--- - the closest directory with a `.plasmon` directory (a workspace Plasmon already worked on;
---   this also catches the dependency sources Plasmon extracts under `.plasmon/readonly`),
--- - the closest directory with a build definition (Mill, sbt),
--- - the closest directory with Scala CLI files (`project.scala`, `.scala-build`, `.bsp`),
--- - the git root,
--- - the file's directory.
---
--- The search doesn't go above the git repository (or worktree) the file is in, so that a
--- repository cloned inside another one gets its own server.
---@param path string absolute file path
---@return string
function M.find_root(path)
  local home = vim.uv.os_homedir()
  local start = vim.fs.dirname(path)

  -- Directories to look at, closest first, up to the git root (or up to, excluding, $HOME)
  local dirs, git_root = {}, nil
  for dir in vim.fs.parents(path) do
    if dir == home then
      break
    end
    table.insert(dirs, dir)
    if is_repo_root(dir) then
      git_root = dir
      break
    end
  end

  local function closest(pred)
    for i, dir in ipairs(dirs) do
      if pred(dir) then
        return dir, i
      end
    end
  end

  local plasmon_root = closest(function(dir)
    return vim.uv.fs_stat(vim.fs.joinpath(dir, ".plasmon")) ~= nil
  end)
  if plasmon_root then
    return plasmon_root
  end

  local build_root, idx = closest(function(dir)
    return has_any(dir, build_markers)
  end)
  if build_root then
    if exists(vim.fs.joinpath(build_root, "build.sbt")) then
      -- sbt sub-project: climb up to the outermost directory with a build.sbt
      while dirs[idx + 1] and exists(vim.fs.joinpath(dirs[idx + 1], "build.sbt")) do
        idx = idx + 1
      end
      build_root = dirs[idx]
    end
    return build_root
  end

  local scala_cli_root = closest(function(dir)
    return has_any(dir, scala_cli_markers)
  end)
  if scala_cli_root then
    return scala_cli_root
  end

  return git_root or start
end

--- `root_dir` function for vim.lsp.config(): only real files get a server.
---@param bufnr integer
---@param on_dir fun(root?: string)
function M.root_dir(bufnr, on_dir)
  if vim.bo[bufnr].buftype ~= "" then
    return
  end
  local name = vim.api.nvim_buf_get_name(bufnr)
  if name == "" or name:match("^%a[%w+.-]*://") then
    return
  end
  on_dir(M.find_root(vim.fs.normalize(vim.fn.fnamemodify(name, ":p"))))
end

--- Whether a process was started by this Neovim instance, directly or not.
---@param pid integer
---@return boolean
local function is_own_descendant(pid)
  local own = vim.fn.getpid()
  local current = pid
  for _ = 1, 10 do
    local ok, proc = pcall(vim.api.nvim_get_proc, current)
    if not ok or type(proc) ~= "table" or not proc.ppid or proc.ppid <= 1 then
      return false
    end
    if proc.ppid == own then
      return true
    end
    current = proc.ppid
  end
  return false
end

local function is_alive(pid)
  local ok, res = pcall(vim.uv.kill, pid, 0)
  return ok and res == 0
end
M.is_alive = is_alive

--- PID of a live Plasmon server holding `<root>/.plasmon/lock` that this Neovim didn't start.
---@param root string
---@return integer?
function M.lock_holder(root)
  local f = io.open(vim.fs.joinpath(root, ".plasmon", "lock"), "r")
  if not f then
    return nil
  end
  local content = f:read("*a") or ""
  f:close()
  local pid = tonumber(content:match("pid=(%d+)"))
  if pid and pid > 0 and is_alive(pid) and not is_own_descendant(pid) then
    return pid
  end
  return nil
end

--- `cmd` function for vim.lsp.config().
---
--- Starts the server in the workspace root: Plasmon keeps its lock file, log, command socket
--- (used by the `plasmon` CLI) and persisted state under `.plasmon/` in its working directory.
---@param dispatchers vim.lsp.rpc.Dispatchers
---@param config? vim.lsp.ClientConfig
function M.cmd(dispatchers, config)
  local cwd = config and (config.cmd_cwd or config.root_dir) or nil
  if cwd then
    local pid = M.lock_holder(cwd)
    if pid then
      vim.schedule(function()
        vim.notify(
          ("Plasmon: another Plasmon server (PID %d) is running in %s, this one will wait for it to exit. "
            .. "Run :Plasmon check-concurrent-server to stop it."):format(pid, cwd),
          vim.log.levels.WARN
        )
      end)
    end
  end
  local env = M.options.cmd_env
  local cmd = M.server_cmd()
  if vim.fn.executable(cmd[1]) == 0 then
    vim.schedule(function()
      vim.notify(
        ("Plasmon: %s not found or not executable, set the `binary` or `cmd` option of require('plasmon').setup()"):format(
          cmd[1]
        ),
        vim.log.levels.ERROR
      )
    end)
  end
  return vim.lsp.rpc.start(cmd, dispatchers, {
    cwd = cwd,
    env = env,
  })
end

--- Base config, returned by lsp/plasmon.lua.
---@return vim.lsp.Config
function M.default()
  local handlers = require("plasmon.handlers")
  return {
    cmd = M.cmd,
    filetypes = { "scala", "java" },
    root_dir = M.root_dir,
    init_options = {
      -- No VS Code `$(name)` icon markers in code lens titles
      icons = "none",
    },
    handlers = handlers.handlers,
    commands = handlers.client_commands,
    -- Buffers get set up from an LspAttach autocmd (see plugin/plasmon.lua) rather than
    -- on_attach, which users may want to set themselves
    on_exit = function(_, _, client_id)
      handlers.on_exit(client_id)
    end,
  }
end

return M
