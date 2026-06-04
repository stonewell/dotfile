-- mod-version:4
--
-- bufferex — helm-mini style buffer + recent-file picker.
--
-- Default bindings:
--   ctrl+x b   open / focus the BufferEx panel
--   up/down    navigate list (also ctrl+p / ctrl+n via keymap/init.lua)
--   return     open selected item
--   (typing)   fuzzy filter

local core         = require "core"
local command      = require "core.command"
local keymap       = require "core.keymap"
local DocView      = require "core.docview"
local BufferExView = require "plugins.bufferex.bufferview"

-- ---------------------------------------------------------------------------
-- Open / focus helper
-- ---------------------------------------------------------------------------

local function open_bufferex()
  local av          = core.active_view
  local current_doc = (av and av:is(DocView)) and av.doc or nil

  local node = core.root_view:get_active_node_default()
  for _, view in ipairs(node.views) do
    if view:is(BufferExView) then
      node:set_active_view(view)
      view:populate(current_doc)
      return
    end
  end
  node:add_view(BufferExView(current_doc))
end

-- ---------------------------------------------------------------------------
-- Proxy helper — temporarily set active_view to filter_view so standard
-- doc:* commands operate on filter_doc rather than nothing.
-- ---------------------------------------------------------------------------

local function with_filter(fn)
  return function(v)
    local prev = core.active_view
    core.active_view = v.filter_view
    fn()
    core.active_view = prev
  end
end

-- ---------------------------------------------------------------------------
-- Global command
-- ---------------------------------------------------------------------------

command.add(nil, {
  ["bufferex:open"] = open_bufferex,
})

-- ---------------------------------------------------------------------------
-- Commands scoped to BufferExView
-- ---------------------------------------------------------------------------

command.add(BufferExView, {
  -- Navigation (skips non-selectable header rows)
  ["bufferex:select-previous"] = function(v)
    local idx = v.selected_idx - 1
    while idx >= 1
      and v.filtered_results[idx]
      and v.filtered_results[idx].selectable == false do
      idx = idx - 1
    end
    if idx >= 1 then
      v.selected_idx = idx
      v:scroll_to_make_selected_visible()
    end
  end,

  ["bufferex:select-next"] = function(v)
    local idx = v.selected_idx + 1
    while idx <= #v.filtered_results
      and v.filtered_results[idx]
      and v.filtered_results[idx].selectable == false do
      idx = idx + 1
    end
    if idx <= #v.filtered_results then
      v.selected_idx = idx
      v:scroll_to_make_selected_visible()
    end
  end,

  ["bufferex:open-selected"] = function(v)
    v:open_selected()
  end,

  -- Deletion
  ["bufferex:backspace"]            = with_filter(function() command.perform("doc:backspace") end),
  ["bufferex:delete-word-backward"] = with_filter(function() command.perform("doc:delete-to-previous-word-start") end),
  ["bufferex:delete-forward"]       = with_filter(function() command.perform("doc:delete") end),
  ["bufferex:delete-word-forward"]  = with_filter(function() command.perform("doc:delete-to-next-word-end") end),

  -- Cursor movement
  ["bufferex:move-left"]       = with_filter(function() command.perform("doc:move-to-previous-char") end),
  ["bufferex:move-right"]      = with_filter(function() command.perform("doc:move-to-next-char") end),
  ["bufferex:move-word-left"]  = with_filter(function() command.perform("doc:move-to-previous-word-start") end),
  ["bufferex:move-word-right"] = with_filter(function() command.perform("doc:move-to-next-word-end") end),
  ["bufferex:move-home"]       = with_filter(function() command.perform("doc:move-to-start-of-indentation") end),
  ["bufferex:move-end"]        = with_filter(function() command.perform("doc:move-to-end-of-line") end),

  -- Selection
  ["bufferex:select-to-left"]       = with_filter(function() command.perform("doc:select-to-previous-char") end),
  ["bufferex:select-to-right"]      = with_filter(function() command.perform("doc:select-to-next-char") end),
  ["bufferex:select-to-word-left"]  = with_filter(function() command.perform("doc:select-to-previous-word-start") end),
  ["bufferex:select-to-word-right"] = with_filter(function() command.perform("doc:select-to-next-word-end") end),
  ["bufferex:select-to-home"]       = with_filter(function() command.perform("doc:select-to-start-of-indentation") end),
  ["bufferex:select-to-end"]        = with_filter(function() command.perform("doc:select-to-end-of-line") end),

  -- Clipboard / undo
  ["bufferex:cut"]   = with_filter(function() command.perform("doc:cut") end),
  ["bufferex:paste"] = with_filter(function() command.perform("doc:paste") end),
  ["bufferex:undo"]  = with_filter(function() command.perform("doc:undo") end),
  ["bufferex:redo"]  = with_filter(function() command.perform("doc:redo") end),
})

-- bufferex:clear-filter fires only when there is text to clear.
command.add(function()
  local v = core.active_view
  if not v:is(BufferExView) then return false end
  local text = v.filter_doc:get_text(1, 1, 1, math.huge)
  return text ~= "", v
end, {
  ["bufferex:clear-filter"] = function(v)
    v.filter_doc:remove(1, 1, math.huge, math.huge)
    v:update_filter(true)
  end,
})

-- ---------------------------------------------------------------------------
-- Keybindings (prepend via keymap.add — existing bindings remain as fallback)
-- ---------------------------------------------------------------------------

keymap.add {
  -- Open panel
  ["ctrl+x b"] = "bufferex:open",

  -- Navigation (scoped by predicate to BufferExView)
  ["up"]     = "bufferex:select-previous",
  ["down"]   = "bufferex:select-next",
  ["return"] = "bufferex:open-selected",

  -- Deletion
  ["backspace"]       = "bufferex:backspace",
  ["shift+backspace"] = "bufferex:backspace",
  ["ctrl+backspace"]  = "bufferex:delete-word-backward",
  ["delete"]          = "bufferex:delete-forward",
  ["ctrl+delete"]     = "bufferex:delete-word-forward",

  -- Arrow-key movement
  ["left"]  = "bufferex:move-left",
  ["right"] = "bufferex:move-right",
  ["home"]  = "bufferex:move-home",
  ["end"]   = "bufferex:move-end",

  -- Word movement
  ["ctrl+left"]  = "bufferex:move-word-left",
  ["ctrl+right"] = "bufferex:move-word-right",

  -- Selection
  ["shift+left"]       = "bufferex:select-to-left",
  ["shift+right"]      = "bufferex:select-to-right",
  ["shift+home"]       = "bufferex:select-to-home",
  ["shift+end"]        = "bufferex:select-to-end",
  ["ctrl+shift+left"]  = "bufferex:select-to-word-left",
  ["ctrl+shift+right"] = "bufferex:select-to-word-right",

  -- Emacs-style movement (mirror rgsearch equivalents)
  ["ctrl+b"] = "bufferex:move-left",
  ["ctrl+f"] = "bufferex:move-right",
  ["ctrl+a"] = "bufferex:move-home",
  ["ctrl+e"] = "bufferex:move-end",

  -- Clipboard / undo
  ["ctrl+w"]       = "bufferex:cut",
  ["ctrl+y"]       = "bufferex:paste",
  ["ctrl+/"]       = "bufferex:undo",
  ["ctrl+z"]       = "bufferex:undo",
  ["ctrl+shift+z"] = "bufferex:redo",

  -- Clear filter
  ["escape"] = "bufferex:clear-filter",
  ["ctrl+g"] = "bufferex:clear-filter",
}
