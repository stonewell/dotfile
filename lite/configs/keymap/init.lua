local keymap = require "core.keymap"
local config  = require "core.config"

local C = require "configs.keymap.chains"
require "configs.keymap.emacs"   -- side-effect: loads all Section B (Emacs) bindings

-- =============================================================================
-- Section A — Complete chains (add_direct)
-- Keys whose full fallback chain must be explicitly controlled.
-- =============================================================================

keymap.add_direct {
  -- Arrow keys (Emacs ctrl+p/n/b/f/a/e are in emacs.lua)
  ["up"]    = C.NAV_PREV,   ["down"]  = C.NAV_NEXT,
  ["left"]  = C.MOVE_LEFT,  ["right"] = C.MOVE_RIGHT,
  ["home"]  = C.MOVE_HOME,  ["end"]   = C.MOVE_END,

  -- Undo (ctrl+/ and ctrl+z share the same chain)
  ["ctrl+/"] = C.UNDO,  ["ctrl+z"] = C.UNDO,

  ["return"] = {
    "killring:paste-selected", "bufferex:open-selected", "rg-search:open-selected", "fd-files:open-selected",
    "command:submit", "context-menu:submit", "doc:newline", "dialog:select",
  },

  -- escape is identical to ctrl+g (ctrl+g is in emacs.lua)
  ["escape"] = C.CANCEL,
}

-- =============================================================================
-- Section C — Filter editor bindings (keymap.add, prepended after B so they
-- take priority when inside a ListView filter editor).
-- All commands are scoped to their respective view.
-- =============================================================================

keymap.add {
  -- Deletion
  ["backspace"]        = C.FILTER_BACKSPACE,
  ["shift+backspace"]  = C.FILTER_BACKSPACE,
  ["ctrl+backspace"]   = { "killring:delete-word-backward", "bufferex:delete-word-backward", "rg-search:delete-word-backward", "fd-files:delete-word-backward" },
  ["delete"]           = { "killring:delete-forward",       "bufferex:delete-forward",       "rg-search:delete-forward",       "fd-files:delete-forward" },
  ["ctrl+delete"]      = { "killring:delete-word-forward",  "bufferex:delete-word-forward",  "rg-search:delete-word-forward",  "fd-files:delete-word-forward" },

  -- Word movement (no Emacs alias; kept with keymap.add so core defaults remain)
  ["ctrl+left"]        = { "killring:move-word-left",  "bufferex:move-word-left",  "rg-search:move-word-left",  "fd-files:move-word-left" },
  ["ctrl+right"]       = { "killring:move-word-right", "bufferex:move-word-right", "rg-search:move-word-right", "fd-files:move-word-right" },

  -- Selection
  ["shift+left"]       = { "killring:select-to-left",       "bufferex:select-to-left",       "rg-search:select-to-left",       "fd-files:select-to-left" },
  ["shift+right"]      = { "killring:select-to-right",      "bufferex:select-to-right",      "rg-search:select-to-right",      "fd-files:select-to-right" },
  ["shift+home"]       = { "killring:select-to-home",       "bufferex:select-to-home",       "rg-search:select-to-home",       "fd-files:select-to-home" },
  ["shift+end"]        = { "killring:select-to-end",        "bufferex:select-to-end",        "rg-search:select-to-end",        "fd-files:select-to-end" },
  ["ctrl+shift+left"]  = { "killring:select-to-word-left",  "bufferex:select-to-word-left",  "rg-search:select-to-word-left",  "fd-files:select-to-word-left" },
  ["ctrl+shift+right"] = { "killring:select-to-word-right", "bufferex:select-to-word-right", "rg-search:select-to-word-right", "fd-files:select-to-word-right" },

  -- Clipboard (ctrl+w / ctrl+y are in emacs.lua with combined doc fallback)
  ["ctrl+shift+z"]     = { "killring:redo",  "bufferex:redo",  "rg-search:redo",  "fd-files:redo" },
}

-- =============================================================================
-- Section D — Multi-stroke sequences
-- =============================================================================

keymap.add {
  -- Search / file find
  ["ctrl+c s f"]       = { "fd-files:find", "core:find-file" },
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

  -- Comment (Emacs C-c C-c)
  ["ctrl+c ctrl+c"]    = "doc:toggle-line-comments",

  -- Window management (C-x …)
  ["ctrl+x ctrl+c"]    = "core:quit",
  ["ctrl+x ctrl+w"]    = "doc:save-as",
  ["ctrl+x 0"]         = "root:unsplit",
  ["ctrl+x 1"]         = "root:unsplit-others",
  ["ctrl+x 5 0"]       = "root:close-node",
  ["ctrl+x o"]         = "root:cycle-pane",
  ["ctrl+x shift+o"]   = "root:cycle-pane-prev",
}
