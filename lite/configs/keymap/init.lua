local core = require "core"
local keymap = require "core.keymap"
local config = require "core.config"
local search = require "core.doc.search"
local command = require "core.command"

--------------------------- Key bindings -------------------------------------
local modal = require "plugins.modal"

local mode = { Edit = "Edit", CtrlC = "CtrlC", CtrlX = "CtrlX", SEL = "SEL" }

-- key binding:
keymap.add { ["ctrl+escape"] = "core:quit" }
keymap.add ({ ["ctrl+p"] = "dialog:previous-entry"}, true)
keymap.add { ["ctrl+p"] = "command:select-previous"}
keymap.add ({ ["ctrl+n"] = "dialog:next-entry" }, true)
keymap.add { ["ctrl+n"] = "command:select-next"}
keymap.add ({ ["ctrl+space"] = modal.go_to_mode(mode.SEL)}, true)

config.plugins.modal.status_bar.strokes = true
config.plugins.modal.show_helpers = true

modal.set_status_bar()

local function dv()
  return core.active_view
end

local function doc()
  return dv().doc
end

local function sort_positions(line1, col1, line2, col2)
  if line1 > line2 or line1 == line2 and col1 > col2 then
    return line2, col2, line1, col1, true
  end
  return line1, col1, line2, col2, false
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

local function unselect()
  if modal.get_mode() ~= mode.SEL then
    for idx, _, _, line2, col2 in doc_multiline_selections(true) do
      doc():set_selections(idx, line2, col2, line2, col2)
    end
  end
end

local function go_to_line()
  if dv() ~= nil and dv().modal ~= nil and dv().modal.current_command and dv().modal.current_command.n_repeat ~= '' and doc() ~= nil then
    local N = dv().modal.current_command.num
    doc():set_selection(N, 1)
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

local function concat(t1, t2)
  for k, v in pairs(t1) do
    if t2[k] == nil then
      t2[k] = v
    end
  end
  return t2
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

local function on_key_command_only_exit(k, ...)
  local view = dv()
  local last_command = view.modal.last_command
  local is_modifers = (modal.key_modifiers[k] ~= nil)

  local r = modal.on_key_command_only(k, ...)

  if last_command == view.modal.last_command and #view.modal.keystrokes == 0 and not is_modifers then
    view.modal = nil
    modal.set_mode(mode.Edit)
  end

  return r
end

config.plugins.modal.modes = { mode.Edit, mode.CtrlC, mode.CtrlX, mode.SEL }
config.plugins.modal.base_mode = mode.Edit

config.plugins.modal.on_key_callbacks.Edit = modal.on_key_passthrough
config.plugins.modal.on_key_callbacks.CtrlC = on_key_command_only_exit
config.plugins.modal.on_key_callbacks.CtrlX = on_key_command_only_exit
config.plugins.modal.on_key_callbacks.SEL = on_key_command_only_exit

config.plugins.modal.keymaps.Edit = {
  ["C-a"] = "doc:move-to-start-of-indentation",
  ["C-b"] = "doc:move-to-previous-char",
  ["C-c"] = modal.go_to_mode(mode.CtrlC),
  ["C-d"] = {copy, delete},
  ["C-e"] = "doc:move-to-end-of-line",
  ["C-f"] = "doc:move-to-next-char",
  ["C-n"] = "doc:move-to-next-line",
  ["C-p"] = "doc:move-to-previous-line",
  ["C-r"] = "find-replace:previous-find",
  ["C-s"] = "find-replace:repeat-find",
  ["C-v"] = "doc:move-to-next-page",
  ["C-x"] = modal.go_to_mode(mode.CtrlX),
  ["C-y"] = {"doc:paste", modal.go_to_mode(mode.Edit), unselect},
  ["C-/"] = {"doc:undo", modal.go_to_mode(mode.Edit), unselect},
  ["C-<space>"] = modal.go_to_mode(mode.SEL),
  ["A-v"] = "doc:move-to-previous-page",
  ["A-x"] = "core:find-command",
  ["A-C-/"] = "modal:help",
  ["C-A-/"] = "modal:help",
}

config.plugins.modal.keymaps.CtrlC = {
  ["<ESC>"] = modal.go_to_mode(mode.Edit),
  ["sf"] = "core:find-file",
  ["sr"] = "modal:help",
  ["sR"] = "modal:help",
  ["ss"] = "find-replace:find",
  ["xb"] = "buffer:picker",
  ["xf"] = "core:open-file",
  ["xh"] = {"doc:select-all", modal.go_to_mode(mode.SEL)},
  ["xs"] = {"doc:save", modal.go_to_mode(mode.Edit)},
  ["x0"] = {"buffer:close", modal.go_to_mode(mode.Edit)},
  ["x1"] = {"root:close-all-others", modal.go_to_mode(mode.Edit)},
  ["x2"] = { "root:split-down", "root:switch-to-down" },
  ["x3"] = { "root:split-right", "root:switch-to-right" },
}

config.plugins.modal.keymaps.CtrlX = {
  ["<ESC>"] = modal.go_to_mode(mode.Edit),
  ["C-c"] = "core:quit",
  ["C-w"] = "doc:save-as",
  ["50"] = "root:close-node",
}

config.plugins.modal.keymaps.SEL = {
  ["<ESC>"] = modal.go_to_mode(mode.Edit),
  ["C-w"] = {"doc:cut", modal.go_to_mode(mode.Edit)},
  ["C-y"] = {"doc:paste", modal.go_to_mode(mode.Edit), unselect},
  ["A-w"] = {"doc:copy", modal.go_to_mode(mode.Edit), unselect},

  ["C-b"] = { "doc:select-to-previous-char", "dialog:previous-entry" },
  ["C-f"] = { "doc:select-to-next-char", "dialog:next-entry" },
  ["C-p"] = { "command:select-previous", "doc:select-to-previous-line" },
  ["C-n"] = { "command:select-next", "doc:select-to-next-line" },
  ["A-v"] = { "doc:select-to-previous-page" },
  ["C-v"] = { "doc:select-to-next-page" },
}
