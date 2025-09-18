local M = {}

function M.exec(cmd, opts)
	local proc = process.start(cmd, opts or {})
	if proc then
		while proc:running() do
			coroutine.yield(0.1)
		end
		return (proc:read_stdout() or '<no stdout>') .. (proc:read_stderr() or '<no stderr>'), proc:returncode()
	end

	return nil
end

function M.isURL(url)
	return url:match '^%w+://'
end

function M.fileExists(path)
	local f = io.open(path)
	if f then
		f:close()
		return true
	end

	return false
end

return M
