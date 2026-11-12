-- Minimal test runner, with no dependency beyond Neovim itself.
--
-- nvim --clean --headless -l editors/neovim/tests/run.lua [spec...] [--filter <pattern>]
--
-- Specs are files under editors/neovim/tests/ named <name>_spec.lua, passed by name ("unit",
-- "integration"). Each spec calls describe() / it() / before_each() / after_each(), all
-- globals set up here. Exits with a non-zero code if any test fails.

local tests_dir = vim.fs.dirname(vim.fs.normalize(vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":p")))
local plugin_dir = vim.fs.dirname(tests_dir)

vim.opt.runtimepath:prepend(plugin_dir)
package.path = tests_dir .. "/?.lua;" .. package.path
vim.o.swapfile = false
vim.o.shadafile = "NONE"
vim.cmd("filetype on")
vim.cmd("runtime plugin/plasmon.lua")

local specs, filter = {}, nil
do
  local args = _G.arg or {}
  local i = 1
  while i <= #args do
    if args[i] == "--filter" then
      filter = args[i + 1]
      i = i + 2
    else
      table.insert(specs, args[i])
      i = i + 1
    end
  end
end
if #specs == 0 then
  specs = { "unit" }
end

local tests = {}
local stack = {}
local hooks = { before = {}, after = {} }

function _G.describe(name, fn)
  table.insert(stack, name)
  local saved_before, saved_after = vim.list_slice(hooks.before), vim.list_slice(hooks.after)
  fn()
  hooks.before, hooks.after = saved_before, saved_after
  table.remove(stack)
end

function _G.it(name, fn)
  table.insert(tests, {
    name = table.concat(vim.list_extend(vim.list_slice(stack), { name }), " / "),
    fn = fn,
    before = vim.list_slice(hooks.before),
    after = vim.list_slice(hooks.after),
  })
end

function _G.before_each(fn)
  table.insert(hooks.before, fn)
end

function _G.after_each(fn)
  table.insert(hooks.after, fn)
end

local function out(s)
  io.stdout:write(s .. "\n")
  io.stdout:flush()
end

for _, spec in ipairs(specs) do
  local path = vim.fs.joinpath(tests_dir, spec .. "_spec.lua")
  local chunk, load_err = loadfile(path)
  if not chunk then
    out("Error loading " .. path .. ": " .. tostring(load_err))
    os.exit(1)
  end
  chunk()
end

local passed, failed, failures = 0, 0, {}
for _, test in ipairs(tests) do
  if not filter or test.name:find(filter, 1, true) then
    local start = vim.uv.hrtime()
    local ok, err = true, nil
    for _, hook in ipairs(test.before) do
      if ok then
        ok, err = xpcall(hook, debug.traceback)
      end
    end
    if ok then
      ok, err = xpcall(test.fn, debug.traceback)
    end
    for _, hook in ipairs(test.after) do
      local hook_ok, hook_err = xpcall(hook, debug.traceback)
      if ok and not hook_ok then
        ok, err = false, hook_err
      end
    end
    local duration = (vim.uv.hrtime() - start) / 1e9
    if ok then
      passed = passed + 1
      out(("  ok    %s (%.1fs)"):format(test.name, duration))
    else
      failed = failed + 1
      table.insert(failures, { name = test.name, err = err })
      out(("  FAIL  %s (%.1fs)"):format(test.name, duration))
      out("        " .. tostring(err):gsub("\n", "\n        "))
    end
  end
end

out(("\n%d passed, %d failed"):format(passed, failed))
for _, failure in ipairs(failures) do
  out("  failed: " .. failure.name)
end
os.exit(failed == 0 and passed > 0 and 0 or 1)
