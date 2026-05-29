local keymap = require "core.keymap"

require "configs.commands.unsplit"
require "configs.commands.cycle_pane"

keymap.add {
  ["ctrl+x 0"] = "root:unsplit",
  ["ctrl+x 1"] = "root:unsplit-others",
  ["ctrl+x o"] = "root:cycle-pane",
  ["ctrl+x O"] = "root:cycle-pane-prev",
}
