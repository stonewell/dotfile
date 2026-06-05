local core   = require "core"
local keymap = require "core.keymap"
local config = require "core.config"

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

-- =============================================================================
-- Section A — Complete chains (add_direct)
-- Keys whose full fallback chain must be explicitly controlled, including all
-- plugin-scoped commands in the correct priority order.
-- =============================================================================

keymap.add_direct {
  ["ctrl+p"] = {
    "killring:select-previous", "bufferex:select-previous",
    "rg-search:select-previous", "command:select-previous",
    "dialog:previous-entry", "doc:move-to-previous-line",
  },
  ["ctrl+n"] = {
    "killring:select-next", "bufferex:select-next",
    "rg-search:select-next", "command:select-next",
    "dialog:next-entry", "doc:move-to-next-line",
  },
  ["ctrl+g"] = {
    "universal-argument:cancel", "push-mark:cancel",
    "isearch:cancel", "avy:cancel",
    "killring:clear-filter", "bufferex:clear-filter", "rg-search:clear-filter",
    "command:escape", "doc:select-none", "context-menu:hide", "dialog:select-no",
  },
  ["up"] = {
    "killring:select-previous", "bufferex:select-previous",
    "rg-search:select-previous", "command:select-previous",
    "context-menu:focus-previous", "doc:move-to-previous-line",
  },
  ["down"] = {
    "killring:select-next", "bufferex:select-next",
    "rg-search:select-next", "command:select-next",
    "context-menu:focus-next", "doc:move-to-next-line",
  },
  ["return"] = {
    "killring:paste-selected", "bufferex:open-selected", "rg-search:open-selected",
    "command:submit", "context-menu:submit", "doc:newline", "dialog:select",
  },
  ["escape"] = {
    "killring:clear-filter", "bufferex:clear-filter", "rg-search:clear-filter",
    "command:escape", "doc:select-none", "context-menu:hide", "dialog:select-no",
  },
}

-- findfile.lua loads after this config and prepends "core:find-file" onto
-- ctrl+p. Strip it back off via the onload hook.
config.plugins.findfile = {
  onload = function() keymap.unbind("ctrl+p", "core:find-file") end,
}

-- =============================================================================
-- Section B — Global Emacs-style bindings (keymap.add, prepend to defaults)
-- =============================================================================

keymap.add {
  -- Emacs movement / editing
  ["ctrl+a"]      = "doc:move-to-start-of-indentation",
  ["ctrl+b"]      = "doc:move-to-previous-char",
  ["ctrl+d"]      = { "listview:delete-forward", function() copy(); delete() end },
  ["ctrl+e"]      = "doc:move-to-end-of-line",
  ["ctrl+f"]      = "doc:move-to-next-char",
  ["ctrl+r"]      = "isearch:backward",
  ["ctrl+s"]      = "isearch:forward",
  ["ctrl+u"]      = "universal-argument:begin",
  ["ctrl+v"]      = "doc:move-to-next-page",
  ["ctrl+w"]      = "doc:cut",
  ["ctrl+y"]      = "doc:paste",
  ["ctrl+/"]      = "doc:undo",
  ["ctrl+space"]  = "push-mark:set",

  -- isearch / avy extras
  ["alt+c"]       = "isearch:toggle-case",
  ["ctrl+;"]      = "avy:goto-char",
  ["ctrl+'"]      = "avy:goto-char-2",

  -- Emacs Alt- bindings
  ["alt+v"]       = "doc:move-to-previous-page",
  ["alt+w"]       = copy,
  ["alt+x"]       = "core:find-command",
  ["alt+y"]       = "killring:open",
  ["alt+g alt+g"] = "doc:go-to-line",

  -- rg-search misc
  ["f5"]          = "rg-search:refresh",
}

-- =============================================================================
-- Section C — Filter editor bindings (keymap.add, prepended after B so they
-- take priority over global bindings when inside a ListView filter editor).
-- All commands are scoped to their respective view; they fall through when the
-- active view is not the relevant one.
-- =============================================================================

keymap.add {
  -- Deletion
  ["backspace"]        = { "killring:backspace",            "bufferex:backspace",            "rg-search:backspace" },
  ["shift+backspace"]  = { "killring:backspace",            "bufferex:backspace",            "rg-search:backspace" },
  ["ctrl+backspace"]   = { "killring:delete-word-backward", "bufferex:delete-word-backward", "rg-search:delete-word-backward" },
  ["delete"]           = { "killring:delete-forward",       "bufferex:delete-forward",       "rg-search:delete-forward" },
  ["ctrl+delete"]      = { "killring:delete-word-forward",  "bufferex:delete-word-forward",  "rg-search:delete-word-forward" },

  -- Arrow-key movement
  ["left"]             = { "killring:move-left",       "bufferex:move-left",       "rg-search:move-left" },
  ["right"]            = { "killring:move-right",      "bufferex:move-right",      "rg-search:move-right" },
  ["home"]             = { "killring:move-home",       "bufferex:move-home",       "rg-search:move-home" },
  ["end"]              = { "killring:move-end",        "bufferex:move-end",        "rg-search:move-end" },
  ["ctrl+left"]        = { "killring:move-word-left",  "bufferex:move-word-left",  "rg-search:move-word-left" },
  ["ctrl+right"]       = { "killring:move-word-right", "bufferex:move-word-right", "rg-search:move-word-right" },

  -- Emacs-style movement in filter (mirrors arrow keys above)
  ["ctrl+b"]           = { "killring:move-left",  "bufferex:move-left",  "rg-search:move-left" },
  ["ctrl+f"]           = { "killring:move-right", "bufferex:move-right", "rg-search:move-right" },
  ["ctrl+a"]           = { "killring:move-home",  "bufferex:move-home",  "rg-search:move-home" },
  ["ctrl+e"]           = { "killring:move-end",   "bufferex:move-end",   "rg-search:move-end" },

  -- Selection
  ["shift+left"]       = { "killring:select-to-left",       "bufferex:select-to-left",       "rg-search:select-to-left" },
  ["shift+right"]      = { "killring:select-to-right",      "bufferex:select-to-right",      "rg-search:select-to-right" },
  ["shift+home"]       = { "killring:select-to-home",       "bufferex:select-to-home",       "rg-search:select-to-home" },
  ["shift+end"]        = { "killring:select-to-end",        "bufferex:select-to-end",        "rg-search:select-to-end" },
  ["ctrl+shift+left"]  = { "killring:select-to-word-left",  "bufferex:select-to-word-left",  "rg-search:select-to-word-left" },
  ["ctrl+shift+right"] = { "killring:select-to-word-right", "bufferex:select-to-word-right", "rg-search:select-to-word-right" },

  -- Clipboard / undo
  ["ctrl+w"]           = { "killring:cut",   "bufferex:cut",   "rg-search:cut" },
  ["ctrl+y"]           = { "killring:paste", "bufferex:paste", "rg-search:paste" },
  ["ctrl+/"]           = { "killring:undo",  "bufferex:undo",  "rg-search:undo" },
  ["ctrl+z"]           = { "killring:undo",  "bufferex:undo",  "rg-search:undo" },
  ["ctrl+shift+z"]     = { "killring:redo",  "bufferex:redo",  "rg-search:redo" },
}

-- =============================================================================
-- Section D — Multi-stroke sequences
-- =============================================================================

keymap.add {
  -- Search / file find
  ["ctrl+c s f"]       = "core:find-file",
  ["ctrl+c s s"]       = "find-replace:find",
  ["ctrl+c s r"]       = "rg-search:find",
  ["ctrl+c s shift+r"] = "rg-search:find-at-caret",

  -- Avy
  ["ctrl+c j w"]       = "avy:goto-word",
  ["ctrl+c j l"]       = "avy:goto-line",

  -- Buffer / window (C-c x …)
  ["ctrl+c x b"]       = "bufferex:open",
  ["ctrl+c x f"]       = "core:open-file",
  ["ctrl+c x h"]       = "doc:select-all",
  ["ctrl+c x s"]       = "doc:save",
  ["ctrl+c x 0"]       = "buffer:close",
  ["ctrl+c x 1"]       = "root:close-all-others",
  ["ctrl+c x 2"]       = "root:split-down",
  ["ctrl+c x 3"]       = "root:split-right",

  -- Window management (C-x …)
  ["ctrl+x ctrl+c"]    = "core:quit",
  ["ctrl+x ctrl+w"]    = "doc:save-as",
  ["ctrl+x 0"]         = "root:unsplit",
  ["ctrl+x 1"]         = "root:unsplit-others",
  ["ctrl+x 5 0"]       = "root:close-node",
  ["ctrl+x o"]         = "root:cycle-pane",
  ["ctrl+x shift+o"]   = "root:cycle-pane-prev",
}
