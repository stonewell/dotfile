local core    = require "core"
local command = require "core.command"
local helpers = require "configs.commands.helpers"

command.add(nil, {
  ["root:unsplit"] = function()
    local node   = core.root_view:get_active_node()
    local root   = core.root_view.root_node
    local parent = node:get_parent_node(root)
    if not parent then return end

    local sibling = parent.a == node and parent.b or parent.a
    local target  = helpers.first_leaf(sibling)
    local active  = node.active_view
    for _, view in ipairs(node.views) do
      target:add_view(view)
    end
    parent:consume(sibling)
    core.set_active_view(active)
  end,

  ["root:unsplit-others"] = function()
    local node = core.root_view:get_active_node()
    local root = core.root_view.root_node
    if node == root then return end
    local active = node.active_view
    root:consume(node)
    root.is_primary_node = true
    core.set_active_view(active)
  end,
})
