-- Shared list-view base class.
--
-- Provides a scrollable list with:
--   - a sticky header (status text row + inline filter editor row)
--   - fuzzy filter via common.fuzzy_match
--   - keyboard / mouse navigation
--
-- Subclasses must implement:
--   ListView:get_item_text(item)        → string for fuzzy matching
--   ListView:get_status_text()          → string for header row 1
--   ListView:draw_item(i, item, x, y, w, h)
--   ListView:open_selected()            → called on Enter / click

local core    = require "core"
local common  = require "core.common"
local style   = require "core.style"
local Doc     = require "core.doc"
local DocView = require "core.docview"
local View    = require "core.view"

-- ---------------------------------------------------------------------------
-- SingleLineDoc — a Doc that strips newlines so the filter stays on one line.
-- ---------------------------------------------------------------------------

local SingleLineDoc = Doc:extend()
function SingleLineDoc:insert(line, col, text)
  SingleLineDoc.super.insert(self, line, col, text:gsub("\n", ""))
end

-- ---------------------------------------------------------------------------
-- ListView
-- ---------------------------------------------------------------------------

local ListView = View:extend()

ListView.context = "application"

function ListView:new()
  ListView.super.new(self)
  self.scrollable       = true
  self.results          = {}
  self.filtered_results = {}
  self.selected_idx     = 0
  self.max_h_scroll     = 0

  -- Embedded single-line editor for the filter input.
  self.filter_doc  = SingleLineDoc()
  self.filter_view = DocView(self.filter_doc)
  self.filter_view.context                  = "application"
  self.filter_view.scrollable               = false
  self.filter_view.get_gutter_width         = function() return 0 end
  self.filter_view.scroll_to_make_visible   = function() end
  self.filter_view.get_scrollable_size      = function() return 0 end
  self.filter_view.get_h_scrollable_size    = function() return 0 end
  self.filter_view.draw_line_highlight      = function() end
  self.filter_change_id = self.filter_doc:get_change_id()
end

function ListView:supports_text_input()
  return true
end

-- ---------------------------------------------------------------------------
-- Filter helpers
-- ---------------------------------------------------------------------------

local function filter_text(self)
  return self.filter_doc:get_text(1, 1, 1, math.huge)
end

-- reset_selection: true when filter text changed (jump to item 1),
-- false when streaming (preserve current selection).
function ListView:update_filter(reset_selection)
  local ft = filter_text(self)
  if ft == "" then
    self.filtered_results = self.results
  else
    self.filtered_results = {}
    for _, item in ipairs(self.results) do
      local hay = self:get_item_text(item)
      if common.fuzzy_match(hay, ft) then
        table.insert(self.filtered_results, item)
      end
    end
  end
  if reset_selection or self.selected_idx == 0 then
    self.selected_idx = #self.filtered_results > 0 and 1 or 0
    if reset_selection then self.scroll.to.y = 0 end
  else
    self.selected_idx = math.min(self.selected_idx, #self.filtered_results)
  end
  core.redraw = true
end

-- ---------------------------------------------------------------------------
-- Abstract methods (subclass must override)
-- ---------------------------------------------------------------------------

function ListView:get_item_text(item)
  return tostring(item)
end

function ListView:get_status_text()
  return string.format("%d items", #self.filtered_results)
end

function ListView:draw_item(i, item, x, y, w, h) end

function ListView:open_selected() end

-- ---------------------------------------------------------------------------
-- Input forwarding
-- ---------------------------------------------------------------------------

function ListView:on_text_input(text)
  self.filter_view:on_text_input(text)
  core.blink_reset()
end

-- ---------------------------------------------------------------------------
-- Layout helpers
-- ---------------------------------------------------------------------------

function ListView:get_results_yoffset()
  return (style.font:get_height() + style.padding.y) * 2 + style.padding.y * 2
end

function ListView:get_line_height()
  return style.padding.y + style.font:get_height()
end

function ListView:get_scrollable_size()
  return self:get_results_yoffset() + #self.filtered_results * self:get_line_height()
end

function ListView:get_h_scrollable_size()
  return self.max_h_scroll
end

function ListView:get_visible_results_range()
  local lh  = self:get_line_height()
  local oy  = self:get_results_yoffset()
  local min = math.max(1, math.floor((self.scroll.y + oy - style.font:get_height()) / lh))
  return min, min + math.floor(self.size.y / lh) + 1
end

function ListView:each_visible_result()
  return coroutine.wrap(function()
    local lh       = self:get_line_height()
    local x, y     = self:get_content_offset()
    local min, max = self:get_visible_results_range()
    y = y + self:get_results_yoffset() + lh * (min - 1)
    for i = min, max do
      local item = self.filtered_results[i]
      if not item then break end
      local _, _, w = self:get_content_bounds()
      coroutine.yield(i, item, x, y, w, lh)
      y = y + lh
    end
  end)
end

function ListView:scroll_to_make_selected_visible()
  local h  = self:get_line_height()
  local y  = h * (self.selected_idx - 1)
  local oy = self:get_results_yoffset()
  self.scroll.to.y = math.min(self.scroll.to.y, y)
  self.scroll.to.y = math.max(self.scroll.to.y, y + h - self.size.y + oy)
end

-- ---------------------------------------------------------------------------
-- Mouse handling
-- ---------------------------------------------------------------------------

function ListView:on_mouse_moved(mx, my, ...)
  ListView.super.on_mouse_moved(self, mx, my, ...)
  self.selected_idx = 0
  for i, item, x, y, w, h in self:each_visible_result() do
    if mx >= x and my >= y and mx < x + w and my < y + h then
      self.selected_idx = i
      break
    end
  end
end

function ListView:on_mouse_pressed(...)
  local caught = ListView.super.on_mouse_pressed(self, ...)
  if not caught then
    return self:open_selected()
  end
end

-- ---------------------------------------------------------------------------
-- Update
-- ---------------------------------------------------------------------------

function ListView:update()
  ListView.super.update(self)

  -- Position the embedded filter DocView in the header area.
  local lh      = style.font:get_height() + style.padding.y
  local label_w = style.font:get_width("/ ")
  local fx      = self.position.x + style.padding.x + label_w
  local fy      = self.position.y + lh + style.padding.y
  local fw      = self.size.x - style.padding.x - label_w
  self.filter_view.position.x = fx
  self.filter_view.position.y = fy
  self.filter_view.size.x     = fw
  self.filter_view.size.y     = lh

  -- Update filter_view with fake focus so DocView's blink timer advances.
  local prev = core.active_view
  core.active_view = self.filter_view
  self.filter_view:update()
  core.active_view = prev

  -- Detect changes in the filter doc and re-filter.
  local cid = self.filter_doc:get_change_id()
  if cid ~= self.filter_change_id then
    self.filter_change_id = cid
    self:update_filter(true)
  end
end

-- ---------------------------------------------------------------------------
-- Draw
-- ---------------------------------------------------------------------------

function ListView:draw()
  self:draw_background(style.background)

  local ox, oy  = self.position.x, self.position.y
  local yoffset = self:get_results_yoffset()

  -- Sticky header background.
  renderer.draw_rect(ox, oy, self.size.x, yoffset, style.background)
  if self.scroll.y ~= 0 then
    renderer.draw_rect(ox, oy + yoffset, self.size.x, style.divider_size, style.divider)
  end

  -- Status text (row 1).
  renderer.draw_text(style.font, self:get_status_text(),
    ox + style.padding.x, oy + style.padding.y, style.text)

  -- Filter input (row 2): "/ " label + embedded DocView.
  local fy = oy + style.font:get_height() + style.padding.y * 2
  renderer.draw_text(style.font, "/ ", ox + style.padding.x, fy, style.dim)
  self.filter_view:draw()

  -- Result rows.
  local _, _, bw = self:get_content_bounds()
  core.push_clip_rect(ox, oy + yoffset + style.divider_size, bw, self.size.y - yoffset)
  self.max_h_scroll = 0
  for i, item, x, y, w, h in self:each_visible_result() do
    if i == self.selected_idx then
      renderer.draw_rect(x, y, w, h, style.line_highlight)
    end
    self:draw_item(i, item, x, y, w, h)
  end
  core.pop_clip_rect()

  self:draw_scrollbar()
end

-- ---------------------------------------------------------------------------

return ListView
