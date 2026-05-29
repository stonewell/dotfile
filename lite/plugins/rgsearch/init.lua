-- mod-version:4
local core      = require "core"
local common    = require "core.common"
local command   = require "core.command"
local keymap    = require "core.keymap"
local translate = require "core.doc.translate"
local RgView    = require "plugins.rgsearch.rgview"

-- ---------------------------------------------------------------------------
-- History
-- ---------------------------------------------------------------------------

local history = {}

local function push_history(query)
  for i, v in ipairs(history) do
    if v == query then table.remove(history, i) break end
  end
  table.insert(history, 1, query)
  if #history > 50 then history[51] = nil end
end

local function history_suggestions(text)
  local t = {}
  for _, v in ipairs(history) do
    if text == "" or v:find(text, 1, true) then
      table.insert(t, { text = v })
    end
  end
  return t
end

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

local function get_active_doc()
  local view = core.active_view
  return view and view.doc or nil
end

local VCS_MARKERS = { ".git", ".hg", ".svn" }

local function find_vcs_root(dir)
  local current = dir
  while true do
    for _, marker in ipairs(VCS_MARKERS) do
      if system.get_file_info(current .. PATHSEP .. marker) then
        return current
      end
    end
    local parent = common.dirname(current)
    if not parent or parent == current then break end
    current = parent
  end
  return nil
end

local function get_search_root()
  local doc = get_active_doc()
  local file = doc and doc.filename
  local file_dir = file and common.dirname(file)

  if file_dir then
    local vcs_root = find_vcs_root(file_dir)
    if vcs_root then
      core.log("rg-search: vcs root = %s", vcs_root)
      return vcs_root
    end
  end

  local project = core.root_project()
  local proj_path = project and project.path
  core.log("rg-search: project.path = %s", tostring(proj_path))
  if file and proj_path and common.path_belongs_to(file, proj_path) then
    return proj_path
  end

  if file_dir then return file_dir end
  return proj_path or "."
end

local function get_selection_or_word()
  local doc = get_active_doc()
  if not doc then return "" end
  local l1, c1, l2, c2 = doc:get_selection()
  if l1 == l2 and c1 == c2 then
    local wl1, wc1 = doc:position_offset(l1, c1, translate.start_of_word)
    local wl2, wc2 = doc:position_offset(l1, c1, translate.end_of_word)
    return doc:get_text(wl1, wc1, wl2, wc2)
  end
  return doc:get_text(l1, c1, l2, c2)
end

-- ---------------------------------------------------------------------------
-- Open or reuse existing RgView in the active node
-- ---------------------------------------------------------------------------

local function open_rg_view(query, root)
  local node = core.root_view:get_active_node_default()
  for _, view in ipairs(node.views) do
    if view:is(RgView) then
      node:set_active_view(view)
      view.root = root
      view:begin_search(query)
      return
    end
  end
  node:add_view(RgView(query, root))
end

-- ---------------------------------------------------------------------------
-- Proxy helper: temporarily make filter_view the active view so that
-- standard doc commands operate on filter_doc rather than nothing.
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
-- Commands
-- ---------------------------------------------------------------------------

command.add(nil, {
  ["rg-search:find"] = function()
    local preselect = get_selection_or_word()
    local root = get_search_root()
    core.command_view:enter("Rg Search", {
      text         = preselect,
      select_text  = preselect ~= "",
      suggest      = history_suggestions,
      submit       = function(text)
        if text == "" then return end
        push_history(text)
        open_rg_view(text, root)
      end,
    })
  end,

  ["rg-search:find-at-caret"] = function()
    local word = get_selection_or_word()
    if word == "" then
      core.warn("rg-search: no word under caret")
      return
    end
    push_history(word)
    open_rg_view(word, get_search_root())
  end,
})

command.add(RgView, {
  ["rg-search:select-previous"] = function(v)
    v.selected_idx = math.max(v.selected_idx - 1, 1)
    v:scroll_to_make_selected_visible()
  end,

  ["rg-search:select-next"] = function(v)
    v.selected_idx = math.min(v.selected_idx + 1, #v.filtered_results)
    v:scroll_to_make_selected_visible()
  end,

  ["rg-search:open-selected"] = function(v)
    v:open_selected_result()
  end,

  ["rg-search:refresh"] = function(v)
    v:refresh()
  end,

  -- Deletion
  ["rg-search:backspace"]           = with_filter(function() command.perform("doc:backspace") end),
  ["rg-search:delete-word-backward"]= with_filter(function() command.perform("doc:delete-to-previous-word-start") end),
  ["rg-search:delete-forward"]      = with_filter(function() command.perform("doc:delete") end),
  ["rg-search:delete-word-forward"] = with_filter(function() command.perform("doc:delete-to-next-word-end") end),

  -- Cursor movement
  ["rg-search:move-left"]      = with_filter(function() command.perform("doc:move-to-previous-char") end),
  ["rg-search:move-right"]     = with_filter(function() command.perform("doc:move-to-next-char") end),
  ["rg-search:move-word-left"] = with_filter(function() command.perform("doc:move-to-previous-word-start") end),
  ["rg-search:move-word-right"]= with_filter(function() command.perform("doc:move-to-next-word-end") end),
  ["rg-search:move-home"]      = with_filter(function() command.perform("doc:move-to-start-of-indentation") end),
  ["rg-search:move-end"]       = with_filter(function() command.perform("doc:move-to-end-of-line") end),

  -- Selection
  ["rg-search:select-to-left"]      = with_filter(function() command.perform("doc:select-to-previous-char") end),
  ["rg-search:select-to-right"]     = with_filter(function() command.perform("doc:select-to-next-char") end),
  ["rg-search:select-to-word-left"] = with_filter(function() command.perform("doc:select-to-previous-word-start") end),
  ["rg-search:select-to-word-right"]= with_filter(function() command.perform("doc:select-to-next-word-end") end),
  ["rg-search:select-to-home"]      = with_filter(function() command.perform("doc:select-to-start-of-indentation") end),
  ["rg-search:select-to-end"]       = with_filter(function() command.perform("doc:select-to-end-of-line") end),

  -- Clipboard / undo
  ["rg-search:cut"]   = with_filter(function() command.perform("doc:cut") end),
  ["rg-search:paste"] = with_filter(function() command.perform("doc:paste") end),
  ["rg-search:undo"]  = with_filter(function() command.perform("doc:undo") end),
  ["rg-search:redo"]  = with_filter(function() command.perform("doc:redo") end),
})

-- rg-search:clear-filter has a non-empty predicate so it only fires (and
-- consumes the key) when there is actually text to clear.
command.add(function()
  local v = core.active_view
  if not v:extends(RgView) then return false end
  local text = v.filter_doc:get_text(1, 1, 1, math.huge)
  return text ~= "", v
end, {
  ["rg-search:clear-filter"] = function(v)
    v.filter_doc:remove(1, 1, math.huge, math.huge)
    v:update_filter(true)
  end,
})

-- ---------------------------------------------------------------------------
-- Keymap bindings (prepend with keymap.add so existing doc:* bindings
-- remain active for all other views).
-- ---------------------------------------------------------------------------

keymap.add {
  -- Deletion
  ["backspace"]       = "rg-search:backspace",
  ["shift+backspace"] = "rg-search:backspace",
  ["ctrl+backspace"]  = "rg-search:delete-word-backward",
  ["delete"]          = "rg-search:delete-forward",
  ["ctrl+delete"]     = "rg-search:delete-word-forward",

  -- Arrow-key movement
  ["left"]  = "rg-search:move-left",
  ["right"] = "rg-search:move-right",
  ["home"]  = "rg-search:move-home",
  ["end"]   = "rg-search:move-end",

  -- Word movement (standard)
  ["ctrl+left"]  = "rg-search:move-word-left",
  ["ctrl+right"] = "rg-search:move-word-right",

  -- Selection
  ["shift+left"]        = "rg-search:select-to-left",
  ["shift+right"]       = "rg-search:select-to-right",
  ["shift+home"]        = "rg-search:select-to-home",
  ["shift+end"]         = "rg-search:select-to-end",
  ["ctrl+shift+left"]   = "rg-search:select-to-word-left",
  ["ctrl+shift+right"]  = "rg-search:select-to-word-right",

  -- Emacs-style movement (ctrl+a/b/e/f already mapped to doc commands in user config)
  ["ctrl+b"] = "rg-search:move-left",
  ["ctrl+f"] = "rg-search:move-right",
  ["ctrl+a"] = "rg-search:move-home",
  ["ctrl+e"] = "rg-search:move-end",

  -- Clipboard / undo (Emacs-style and standard)
  ["ctrl+w"] = "rg-search:cut",
  ["ctrl+y"] = "rg-search:paste",
  ["ctrl+/"] = "rg-search:undo",
  ["ctrl+z"] = "rg-search:undo",
  ["ctrl+shift+z"] = "rg-search:redo",

  -- Clear filter (Escape and user's ctrl+g escape chord)
  ["escape"] = "rg-search:clear-filter",
  ["ctrl+g"] = "rg-search:clear-filter",
}
