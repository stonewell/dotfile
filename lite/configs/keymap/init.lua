local core = require "core"
local keymap = require "core.keymap"
local config = require "core.config"
local search = require "core.doc.search"
local command = require "core.command"

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

--------------------------- Key bindings -------------------------------------
-- key binding:
keymap.add_direct { ["ctrl+p"] = { "killring:select-previous", "bufferex:select-previous", "rg-search:select-previous", "command:select-previous", "dialog:previous-entry", "doc:move-to-previous-line" } }
keymap.add_direct { ["ctrl+n"] = { "killring:select-next",     "bufferex:select-next",     "rg-search:select-next",     "command:select-next",     "dialog:next-entry",     "doc:move-to-next-line"     } }
keymap.add_direct { ["ctrl+g"] = { "command:escape", "doc:select-none", "context-menu:hide", "dialog:select-no" } }

-- findfile.lua loads after this config (plugins run at priority 100, user init at -2)
-- and prepends "core:find-file" onto ctrl+p.  Strip it back off via the onload hook.
config.plugins.findfile = {
  onload = function() keymap.unbind("ctrl+p", "core:find-file") end
}

keymap.add {
  ["ctrl+a"] = "doc:move-to-start-of-indentation",
  ["ctrl+b"] = "doc:move-to-previous-char",
  ["ctrl+d"] = {copy, delete},
  ["ctrl+e"] = "doc:move-to-end-of-line",
  ["ctrl+f"] = "doc:move-to-next-char",
  ["ctrl+r"] = "find-replace:previous-find",
  ["ctrl+s"] = "find-replace:repeat-find",
  ["ctrl+v"] = "doc:move-to-next-page",
  ["ctrl+w"] = "doc:cut",
  ["ctrl+y"] = "doc:paste",
  ["ctrl+/"] = "doc:undo",

  ["alt+v"] = "doc:move-to-previous-page",
  ["alt+w"] = copy,
  ["alt+x"] = "core:find-command",
  ["alt+g alt+g"] = "doc:go-to-line",

  ["ctrl+c s f"] = "core:find-file",
  ["ctrl+c s s"] = "find-replace:find",
  ["ctrl+c s r"]       = "rg-search:find",
  ["ctrl+c s shift+r"] = "rg-search:find-at-caret",
  ["ctrl+c x b"] = "bufferex:open",
  ["ctrl+c x f"] = "core:open-file",
  ["ctrl+c x h"] = "doc:select-all",
  ["ctrl+c x s"] = "doc:save",
  ["ctrl+c x 0"] = "buffer:close",
  ["ctrl+c x 1"] = "root:close-all-others",
  ["ctrl+c x 2"] = "root:split-down",
  ["ctrl+c x 3"] = "root:split-right",

  ["ctrl+x ctrl+c"] = "core:quit",
  ["ctrl+x ctrl+w"] = "doc:save-as",
  ["ctrl+x 5 0"] = "root:close-node",

  -- rg-search (scoped to RgView)
  ["up"]     = "rg-search:select-previous",
  ["down"]   = "rg-search:select-next",
  ["return"] = "rg-search:open-selected",
  ["f5"]     = "rg-search:refresh",
}
