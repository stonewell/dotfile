-- Shared command-chain tables.
-- Each table is a fallback sequence passed to keymap.add_direct / keymap.add.
-- Multiple physical keys that should behave identically reference the same
-- table here so there is a single source of truth.

return {
  -- Vertical navigation: used by ctrl+p / up and ctrl+n / down.
  NAV_PREV = {
    "killring:select-previous", "bufferex:select-previous",
    "rg-search:select-previous", "fd-files:select-previous", "command:select-previous",
    "context-menu:focus-previous", "dialog:previous-entry",
    "doc:move-to-previous-line",
  },
  NAV_NEXT = {
    "killring:select-next", "bufferex:select-next",
    "rg-search:select-next", "fd-files:select-next", "command:select-next",
    "context-menu:focus-next", "dialog:next-entry",
    "doc:move-to-next-line",
  },

  -- Horizontal movement: used by left/ctrl+b, right/ctrl+f, home/ctrl+a, end/ctrl+e.
  MOVE_LEFT  = { "killring:move-left",  "bufferex:move-left",  "rg-search:move-left",  "fd-files:move-left",
                 "doc:move-to-previous-char", "dialog:previous-entry" },
  MOVE_RIGHT = { "killring:move-right", "bufferex:move-right", "rg-search:move-right", "fd-files:move-right",
                 "doc:move-to-next-char", "dialog:next-entry" },
  MOVE_HOME  = { "killring:move-home",  "bufferex:move-home",  "rg-search:move-home",  "fd-files:move-home",
                 "doc:move-to-start-of-indentation" },
  MOVE_END   = { "killring:move-end",   "bufferex:move-end",   "rg-search:move-end",   "fd-files:move-end",
                 "doc:move-to-end-of-line" },

  -- Backspace: plain and shift+backspace are identical.
  FILTER_BACKSPACE = { "killring:backspace", "bufferex:backspace", "rg-search:backspace", "fd-files:backspace" },

  -- Undo: ctrl+/ and ctrl+z share this chain.
  UNDO = { "killring:undo", "bufferex:undo", "rg-search:undo", "fd-files:undo", "doc:undo" },

  -- Cancel / escape: ctrl+g and escape share this chain.
  CANCEL = {
    "universal-argument:cancel", "push-mark:cancel",
    "isearch:cancel", "avy:cancel",
    "killring:clear-filter", "bufferex:clear-filter", "rg-search:clear-filter", "fd-files:clear-filter",
    "listview:close",
    "command:escape", "doc:select-none", "context-menu:hide", "dialog:select-no",
  },
}
