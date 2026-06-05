local core   = require "core"
local common = require "core.common"
local config = require "core.config"
local style  = require "core.style"

-- Draw the which-key popup panel above the status bar.
-- root_view: the RootView instance (provides size).
-- state: the shared state table from init.lua.
local function draw(root_view, state)
  local sw   = root_view.size.x
  local sv_h = core.status_view and core.status_view.size.y or 0
  local lh   = style.font:get_height() + style.padding.y
  local cw   = config.plugins.whichkey.col_width
  local ncol = math.max(1, math.floor(sw / cw))
  local nrow = math.max(1, math.ceil(#state.entries / ncol))

  -- Header row + entry rows.
  local ph = (nrow + 1) * lh + style.padding.y
  local py = root_view.size.y - ph - sv_h

  -- Background and top border.
  renderer.draw_rect(0, py, sw, ph, style.background2)
  renderer.draw_rect(0, py, sw, style.divider_size, style.divider)

  -- Header: "which-key: <prefix> -"
  local header = "which-key: " .. (state.prefix or "") .. " -"
  renderer.draw_text(style.font, header,
    style.padding.x, py + style.padding.y / 2, style.dim)

  -- Entry rows (fill columns left-to-right, then wrap to next row).
  local entry_y0 = py + lh + style.padding.y / 2

  for i, entry in ipairs(state.entries) do
    local col = (i - 1) % ncol
    local row = math.floor((i - 1) / ncol)
    local x   = col * cw + style.padding.x
    local y   = entry_y0 + row * lh

    -- Key in accent color (left-aligned within a fixed key gutter).
    local key_str = entry.key
    local kw = style.font:get_width(key_str) + style.padding.x
    common.draw_text(style.font, style.accent, key_str, "left", x,      y, cw, lh)

    -- Command / group description.
    local desc_color = entry.is_prefix and style.accent or style.text
    common.draw_text(style.font, desc_color, entry.display, "left", x + kw, y, cw - kw, lh)
  end
end

return { draw = draw }
