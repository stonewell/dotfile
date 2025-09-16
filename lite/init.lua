-- put user settings here
-- this module will be loaded after everything else when the application starts
-- it will be automatically reloaded when saved

local core = require "core"
local keymap = require "core.keymap"
local config = require "core.config"
local style = require "core.style"

local modal = require "plugins.modal"
------------------------------ Themes ----------------------------------------

-- light theme:
-- core.reload_module("colors.summer")

--------------------------- Key bindings -------------------------------------

-- key binding:
keymap.add { ["ctrl+escape"] = "core:quit" }
keymap.add ({ ["ctrl+p"] = "dialog:previous-entry"}, true)
keymap.add { ["ctrl+p"] = "command:select-previous"}
keymap.add ({ ["ctrl+n"] = "dialog:next-entry" }, true)
keymap.add { ["ctrl+n"] = "command:select-next"}
keymap.add ({ ["ctrl+space"] = modal.go_to_mode("SEL")}, true)

config.plugins.miq.repos = {
    'https://github.com/lite-xl/lite-xl-plugins.git:master',
    'https://github.com/Evergreen-lxl/evergreen-languages.git:main',
}

config.plugins.miq.debug = false

config.plugins.miq.plugins = {
  -- this allows Miq to manage itself
  'TorchedSammy/Miq',

  'editorconfig',
  'cleanstart',
  'indentguide',

  {'https://codeberg.org/Mandarancio/lite-modal.git', name='modal'},

  {'https://codeberg.org/Mandarancio/lite-buffers.git', name='buffers'},

  'Evergreen-lxl/Evergreen.lxl',
  'evergreen_c',
  'evergreen_cpp',
  'evergreen_lua',
  'evergreen_python',
  'evergreen_json',
  'evergreen_bash',
  'evergreen_cmake',
  'evergreen_rust',
}

config.plugins.modal.status_bar.strokes = true
modal.set_status_bar()

config.plugins.modal.modes = { "Edit", "CtrlC", "CtrlX", "SEL" }
config.plugins.modal.base_mode = "Edit"

config.plugins.modal.on_key_callbacks.Edit = modal.on_key_passthrough
config.plugins.modal.on_key_callbacks.CtrlC = modal.on_key_command_only
config.plugins.modal.on_key_callbacks.CtrlX = modal.on_key_command_only
config.plugins.modal.on_key_callbacks.SEL = modal.on_key_command_only

config.plugins.modal.keymaps.Edit = {
  ["C-a"] = "doc:move-to-start-of-indentation",
  ["C-b"] = "doc:move-to-previous-char",
  ["C-c"] = modal.go_to_mode("CtrlC"),
  ["C-e"] = "doc:move-to-end-of-line",
  ["C-f"] = "doc:move-to-next-char",
  ["C-n"] = "doc:move-to-next-line",
  ["C-p"] = "doc:move-to-previous-line",
  ["C-v"] = "doc:move-to-next-page",
  ["C-x"] = modal.go_to_mode("CtrlX"),
  ["C-y"] = "doc:paste",
  ["C-/"] = "doc:undo",
  ["C-<space>"] = modal.go_to_mode("SEL"),
  ["A-v"] = "doc:move-to-previous-page",
  ["A-x"] = "core:find-command",
  ["A-C-/"] = "modal:help",
  ["C-A-/"] = "modal:help",
}

config.plugins.modal.keymaps.CtrlC = {
  ["<ESC>"] = modal.go_to_mode("Edit"),
  ["sf"] = "core:find-file",
  ["sr"] = "modal:help",
  ["sR"] = "modal:help",
  ["ss"] = "find-replace:find",
  ["xb"] = "buffer:picker",
  ["xf"] = "core:open-file",
  ["xh"] = {"doc:select-all", modal.go_to_mode("SEL")},
  ["xs"] = {"doc:save", modal.go_to_mode("Edit")},
  ["x0"] = {"buffer:close", modal.go_to_mode("Edit")},
  ["x1"] = {"root:close-all-others", modal.go_to_mode("Edit")},
  ["x2"] = { "root:split-down", "root:switch-to-down" },
  ["x3"] = { "root:split-right", "root:switch-to-right" },
}

config.plugins.modal.keymaps.CtrlX = {
  ["<ESC>"] = modal.go_to_mode("Edit"),
  ["C-c"] = "core:quit",
  ["C-w"] = "doc:save-as",
  ["50"] = "root:close-node",
}

config.plugins.modal.keymaps.SEL = {
  ["<ESC>"] = modal.go_to_mode("Edit"),
  ["C-w"] = {"doc:cut", modal.go_to_mode("Edit")},
  ["C-y"] = {"doc:paste", modal.go_to_mode("Edit")},
  ["A-w"] = {"doc:copy", modal.go_to_mode("Edit")},

  ["C-b"] = { "doc:select-to-previous-char", "dialog:previous-entry" },
  ["C-f"] = { "doc:select-to-next-char", "dialog:next-entry" },
  ["C-p"] = { "command:select-previous", "doc:select-to-previous-line" },
  ["C-n"] = { "command:select-next", "doc:select-to-next-line" },
  ["A-v"] = { "doc:select-to-previous-page" },
  ["C-v"] = { "doc:select-to-next-page" },
}
