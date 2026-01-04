view:set_theme(not CURSES and 'base16-dracula' or 'term', {size = 16})

local textredux = require 'textredux'
local hydra = require 'hydra'
local emacs = require 'emacs'
local isearch = require 'isearch'

textredux.hijack()

io.detect_indentation = false
buffer.use_tabs = false
buffer.tab_width = 4
buffer.tab_indents = true
buffer.back_space_un_indents = true
buffer.eol_mode = buffer.EOL_LF

-- disable menubar, tabs and scrollbars
events.connect(events.INITIALIZED, function()
                                       textadept.menu.menubar = nil
                                   end)

ui.tabs = false
view.h_scroll_bar = false
view.v_scroll_bar = false

-- disable folding and fold margin
view.property['fold'] = 0
view.margin_width_n[3] = 0

ui.command_entry.caret_period = 0
ui.command_entry.caret_style = view.CARETSTYLE_BLOCK
ui.command_entry.caret_line_frame = 1
view.caret_period = 0
view.caret_style = view.CARETSTYLE_BLOCK
view.caret_line_frame = 1
-- keep caret 8 lines away from top/bottom
view:set_y_caret_policy(view.CARET_SLOP | view.CARET_STRICT | view.CARET_EVEN, 8)

view.edge_column = 100
view.edge_color = 0xcccccc

view.indentation_guides = buffer.IV_LOOKBOTH
view.whitespace_size = 3

io.ensure_final_newline = true
view.end_at_last_line = false

-- textadept.editing.highlight_words = textadept.editing.HIGHLIGHT_SELECTED
-- center results when searching
events.connect(events.FIND_RESULT_FOUND, function()
                                             view:vertical_center_caret()
                                         end)

-- Reset selection color
events.connect(events.VIEW_NEW,
               function()
                   local sel_text = 0x282a36
                   local sel_back = 0xf1fa8c

                   view.element_color[view.ELEMENT_SELECTION_TEXT] = sel_text
                   view.element_color[view.ELEMENT_SELECTION_BACK] = sel_back
                   view.element_color[view.ELEMENT_SELECTION_ADDITIONAL_TEXT] = sel_text
                   view.element_color[view.ELEMENT_SELECTION_ADDITIONAL_BACK] = sel_back
                   view.element_color[view.ELEMENT_SELECTION_SECONDARY_TEXT] = sel_text
                   view.element_color[view.ELEMENT_SELECTION_SECONDARY_BACK] = sel_back
                   view.element_color[view.ELEMENT_SELECTION_INACTIVE_TEXT ] = sel_text
                   view.element_color[view.ELEMENT_SELECTION_INACTIVE_BACK] = sel_back
               end)

-- unbind some defaults
keys['ctrl+alt+\\'] = nil
keys['ctrl+alt+|'] = nil
keys['ctrl+r'] = nil
keys['ctrl+f'] = nil
keys['ctrl+p'] = nil
keys['ctrl+o'] = nil
keys['ctrl+d'] = nil
keys['ctrl+u'] = nil
keys['ctrl+\t'] = nil
keys['shift+ctrl+\t'] = nil

emacs.keys.enable()

local fn_dispatch = {
    ['open'] = textredux.fs.open_file,
    ['switchbuffer'] = textredux.buffer_list.show,
    ['saveas'] = textredux.fs.save_buffer_as,
    -- ['recent'] = textredux.core.filteredlist.wrap(io.open_recent_file),
    -- ['switchbuffer_project'] = textredux.core.filteredlist.wrap(util.show_project_buffers),

    ['open_recent'] = io.open_recent_file,
}

keys['ctrl+x'] = {
  ['0'] = function() view:unsplit() end,
  ['b'] = fn_dispatch['switchbuffer'],
  ['k'] = fn_dispatch['switchbuffer'],
  ['ctrl+c'] = quit,
  ['ctrl+f'] = fn_dispatch['open'],
  ['ctrl+r'] = fn_dispatch['open_recent'],
  ['ctrl+s'] = buffer.save,
  ['ctrl+w'] = fn_dispatch['saveas'],
}

keys['alt+o'] = function()
    isearch.replace.start_replace()
end
