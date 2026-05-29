local M = {}

function M.first_leaf(node)
  if node.type == "leaf" then return node end
  return M.first_leaf(node.a)
end

function M.get_cyclic_leaves(node, result)
  result = result or {}
  if node.type == "leaf" then
    local sx, sy = node:get_locked_size()
    if not sx and not sy and not node:is_empty() then
      table.insert(result, node)
    end
  else
    M.get_cyclic_leaves(node.a, result)
    M.get_cyclic_leaves(node.b, result)
  end
  return result
end

return M
