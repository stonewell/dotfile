-- Emacs-style keybindings and kill helpers.
-- This module is loaded for its side-effects (keymap.add calls).
-- All editing helper functions that need doc/clipboard access live here.

local config  = require "core.config"
local core    = require "core"
local keymap  = require "core.keymap"
local command = require "core.command"
local C       = require "configs.keymap.chains"

-- findfile.lua loads after this config and prepends "core:find-file" onto
-- ctrl+p. Strip it back off via the onload hook.
config.plugins.findfile = {
  onload = function() keymap.unbind("ctrl+p", "core:find-file") end,
}

-- ---------------------------------------------------------------------------
-- View / doc accessors
-- ---------------------------------------------------------------------------

local function dv()
  return core.active_view
end

local function doc()
  return dv().doc
end

local function doc_multiline_selections(sort)
  if dv() == nil or doc() == nil or doc().get_selections == nil then return function() return nil end end
  local iter, state, idx, line1, col1, line2, col2 = doc():get_selections(sort)
  return function()
    idx, line1, col1, line2, col2 = iter(state, idx)
    if idx and line2 > line1 and col2 == 1 then
      line2 = line2 - 1
      col2 = #doc().lines[line2]
    end
    return idx, line1, col1, line2, col2
  end
end

-- ---------------------------------------------------------------------------
-- Core kill/copy helpers
-- ---------------------------------------------------------------------------

local function copy()
  if doc() and doc().get_selection then
    local l1, c1, l2, c2 = doc():get_selection()
    if l1 == l2 and c2 == c1 then
      c2 = c1 + 1
    end
    local text = doc():get_text(l1, c1, l2, c2)
    system.set_clipboard(text)
  end
end

local function delete()
  for idx, line1, col1, line2, col2 in doc_multiline_selections(true) do
    if line1 == line2 and col1 == col2 then
      local text = doc():get_text(line1, 1, line1, math.huge)
      col2 = col2 + 1
      if #text + 1 < col2 then
        col2 = 1
        line2 = line2 + 1
      end
    end
    doc():raw_remove(line1, col1, line2, col2, doc().undo_stack, system.get_time())
    doc():set_selections(idx, line1, col1)
  end
end

-- ---------------------------------------------------------------------------
-- Emacs kill helpers (save to clipboard → recorded by killring hook)
-- ---------------------------------------------------------------------------

-- C-k: kill from cursor to end of line (or kill newline when at EOL).
local function kill_line()
  if not (doc() and doc().get_selection) then return end
  local l1, c1, l2, c2 = doc():get_selection()
  if l1 ~= l2 or c1 ~= c2 then
    -- Has selection: cut it.
    local text = doc():get_text(l1, c1, l2, c2)
    system.set_clipboard(text)
    doc():remove(l1, c1, l2, c2)
    return
  end
  -- No selection: kill from cursor to end of line.
  local line = doc().lines[l1] or ""
  local eol = #line  -- #line includes the trailing \n
  if c1 >= eol then
    -- At or past the newline: kill the newline itself (join with next line).
    l2, c2 = l1 + 1, 1
  else
    -- Mid-line: kill up to (but not including) the \n.
    l2, c2 = l1, eol
  end
  local text = doc():get_text(l1, c1, l2, c2)
  system.set_clipboard(text)
  doc():remove(l1, c1, l2, c2)
end

-- M-d: kill from cursor to next word end.
local function kill_word()
  if not (doc() and doc().get_selection) then return end
  local l1, c1, l2, c2 = doc():get_selection()
  if l1 == l2 and c1 == c2 then
    command.perform("doc:select-to-next-word-end")
    l1, c1, l2, c2 = doc():get_selection()
  end
  if l1 == l2 and c1 == c2 then return end
  system.set_clipboard(doc():get_text(l1, c1, l2, c2))
  doc():remove(l1, c1, l2, c2)
end

-- M-DEL: kill from cursor back to previous word start.
local function backward_kill_word()
  if not (doc() and doc().get_selection) then return end
  local l1, c1, l2, c2 = doc():get_selection()
  if l1 == l2 and c1 == c2 then
    command.perform("doc:select-to-previous-word-start")
    l1, c1, l2, c2 = doc():get_selection()
  end
  if l1 == l2 and c1 == c2 then return end
  system.set_clipboard(doc():get_text(l1, c1, l2, c2))
  doc():remove(l1, c1, l2, c2)
end

-- ---------------------------------------------------------------------------
-- Emacs navigation + cancel (add_direct: full chains, no fallback leakage)
-- ---------------------------------------------------------------------------

keymap.add_direct {
  ["ctrl+p"] = C.NAV_PREV,   ["ctrl+n"] = C.NAV_NEXT,
  ["ctrl+b"] = C.MOVE_LEFT,  ["ctrl+f"] = C.MOVE_RIGHT,
  ["ctrl+a"] = C.MOVE_HOME,  ["ctrl+e"] = C.MOVE_END,
  ["ctrl+g"] = C.CANCEL,
}

-- ---------------------------------------------------------------------------
-- Section B — Global Emacs-style bindings
-- ---------------------------------------------------------------------------

keymap.add {
  -- Emacs editing
  ["ctrl+d"]      = { "listview:delete-forward", function() copy(); delete() end },
  ["ctrl+k"]      = kill_line,
  ["ctrl+r"]      = "isearch:backward",
  ["ctrl+s"]      = "isearch:forward",
  ["ctrl+u"]      = "universal-argument:begin",
  ["ctrl+v"]      = "doc:move-to-next-page",
  ["ctrl+w"]      = { "killring:cut",   "bufferex:cut",   "rg-search:cut",   "fd-files:cut",   "doc:cut" },
  ["ctrl+y"]      = { "killring:paste", "bufferex:paste", "rg-search:paste", "fd-files:paste", "doc:paste" },
  ["ctrl+space"]  = "push-mark:set",

  -- Kill-ring management
  ["ctrl+shift+k"] = "killring:clear",

  -- isearch / avy extras
  ["alt+c"]       = "isearch:toggle-case",
  ["ctrl+;"]      = "avy:goto-char",
  ["ctrl+'"]      = "avy:goto-char-2",

  -- Emacs M-<key> bindings
  ["alt+b"]       = "doc:move-to-previous-word-start",
  ["alt+f"]       = "doc:move-to-next-word-end",
  ["alt+shift+b"] = "doc:select-to-previous-word-start",
  ["alt+shift+f"] = "doc:select-to-next-word-end",
  ["alt+<"]       = "doc:move-to-start-of-doc",
  ["alt+>"]       = "doc:move-to-end-of-doc",
  ["alt+;"]       = "doc:toggle-line-comments",
  ["alt+d"]       = kill_word,
  ["alt+backspace"] = backward_kill_word,
  ["alt+v"]       = "doc:move-to-previous-page",
  ["alt+w"]       = copy,
  ["alt+x"]       = "core:find-command",
  ["alt+y"]       = "killring:open",
  ["alt+g alt+g"] = "doc:go-to-line",

  -- rg-search misc
  ["alt+p"]       = "rg-search:find-at-caret",
  ["f5"]          = "rg-search:refresh",
  ["f6"]          = "fd-files:refresh",
}
