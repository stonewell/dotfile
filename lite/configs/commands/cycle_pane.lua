local core    = require "core"
local command = require "core.command"
local helpers = require "configs.commands.helpers"

command.add(nil, {
  ["root:cycle-pane"] = function()
    local root    = core.root_view.root_node
    local current = core.root_view:get_active_node()
    local leaves  = helpers.get_cyclic_leaves(root)
    if #leaves <= 1 then return end
    local idx = 1
    for i, leaf in ipairs(leaves) do
      if leaf == current then idx = i break end
    end
    core.set_active_view(leaves[(idx % #leaves) + 1].active_view)
  end,

  ["root:cycle-pane-prev"] = function()
    local root    = core.root_view.root_node
    local current = core.root_view:get_active_node()
    local leaves  = helpers.get_cyclic_leaves(root)
    if #leaves <= 1 then return end
    local idx = 1
    for i, leaf in ipairs(leaves) do
      if leaf == current then idx = i break end
    end
    core.set_active_view(leaves[((idx - 2) % #leaves) + 1].active_view)
  end,
})
