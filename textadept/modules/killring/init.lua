local list = require 'textredux.core.list'

local M = {}

M.max_size = 50

local ring = {}

local function truncate(text, max)
  text = text:gsub('\n', '↵'):gsub('\t', '→')
  if #text > max then return text:sub(1, max) .. '…' end
  return text
end

function M.push(text)
  if not text or text == '' then return end
  for i, v in ipairs(ring) do
    if v == text then table.remove(ring, i); break end
  end
  table.insert(ring, 1, text)
  if #ring > M.max_size then table.remove(ring) end
end

function M.show()
  if #ring == 0 then
    ui.statusbar_text = 'Kill ring is empty'
    return
  end

  local target_buffer = buffer
  local items = {}
  for i, text in ipairs(ring) do
    local entry = {tostring(i), truncate(text, 70)}
    entry.full_text = text  -- non-integer key: hidden from display and search
    items[i] = entry
  end

  local l = list.new('Kill Ring')
  l.headers = {'#', 'Text'}
  l.items = items
  l.on_selection = function(l, item)
    local text = item.full_text
    l:close()
    view:goto_buffer(target_buffer)
    buffer:replace_sel(text)
    M.push(text)  -- move selected entry to top
  end
  l:show()
end

-- Track cut/copy so ring is populated before first paste.
local E = require 'emacs.editing'
local orig_cut, orig_copy = E.cut, E.copy
E.cut  = function(b, e) orig_cut(b, e);  M.push(ui.get_clipboard_text()) end
E.copy = function(b, e) orig_copy(b, e); M.push(ui.get_clipboard_text()) end

-- Track every paste (including external clipboard pastes) and move to top.
local function wrap_paste()
  if buffer._killring_paste_wrapped or buffer._textredux then return end
  buffer._killring_paste_wrapped = true
  local orig_paste = buffer.paste
  buffer.paste = function(buf)
    local text = ui.get_clipboard_text()
    if not text or text == '' then
      orig_paste(buf or buffer)
      return
    end
    M.push(text)
    ;(buf or buffer):replace_sel(ring[1])
  end
end
events.connect(events.BUFFER_NEW, wrap_paste)

return M
