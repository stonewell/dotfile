local core = require 'core'
local command = require 'core.command'
local common = require 'core.common'
local config = require 'plugins.treesit.config'
local util = require 'plugins.treesit.util'
local ts = require 'libraries.tree_sitter'

local M = {
	defs = {},
	langCache = {},
	queryCache = {
		highlights = {},
	},
}

local soExt = PLATFORM == 'Windows' and '.dll' or '.so'

-- On Windows, Neovim bundled parsers use .dll but nvim-treesitter-installed
-- parsers use .so (their cross-platform convention).  Try both so the same
-- code handles either source without user configuration.
local function findParser(dir, name)
	if PLATFORM == 'Windows' then
		for _, ext in ipairs({ '.dll', '.so' }) do
			local path = dir .. '/' .. name .. ext
			if system.get_file_info(path) then return path end
		end
		return dir .. '/' .. name .. '.dll'  -- fallback; getLang will error clearly
	end
	return dir .. '/' .. name .. '.so'
end

-- Pattern matching Neovim's EXTENDS_FORMAT in runtime/lua/vim/treesitter/query.lua
local EXTENDS_PATTERN = '^%s*;+%s*extends%s*$'

function M.addDef(defOptions)
	local def = {}

	assert(defOptions.name, 'Name is required for language definition')
	assert(not M.defs[defOptions.name], 'Duplicate language name')
	assert(defOptions.path, 'Path is required for language definition')

	def.name = defOptions.name
	def.files = defOptions.files

	local path = common.home_expand(defOptions.path)

	if defOptions.files and #defOptions.files > 0 then
		def.soFile = util.joinPath {
			path,
			defOptions.soFile and
				defOptions.soFile:gsub('{SOEXT}', soExt) or
				'parser' .. soExt
		}
	end

	def.queryFiles = {}

	-- queryFiles.highlights may be a string or an ordered list of strings
	local hl = defOptions.queryFiles and defOptions.queryFiles.highlights
	if type(hl) == 'table' then
		def.queryFiles.highlights = {}
		for _, p in ipairs(hl) do
			def.queryFiles.highlights[#def.queryFiles.highlights + 1] =
				util.joinPath { path, p }
		end
	else
		def.queryFiles.highlights = util.joinPath {
			path,
			hl or 'queries/highlights.scm'
		}
	end

	M.defs[#M.defs + 1] = def
	M.defs[def.name] = def
end

-- Convenience wrapper for nvim-treesitter's directory layout:
--   {root}/parser/{name}.dll   — compiled parser
--   {root}/queries/{name}/highlights.scm  — often has ; extends
--   {runtimeDir}/queries/{name}/highlights.scm — Neovim bundled base
--
-- If runtimeDir is provided, addNvimLang peeks at the nvim-treesitter query
-- file; when it starts with "; extends", the runtime base is prepended so
-- the base captures are not lost.
function M.addNvimLang(opts)
	assert(opts.root, 'root is required for addNvimLang')
	assert(opts.name, 'name is required for addNvimLang')

	local root       = common.home_expand(opts.root)
	local runtimeDir = opts.runtimeDir and common.home_expand(opts.runtimeDir)
	local parserDir  = opts.parserDir and common.home_expand(opts.parserDir) or (root .. '/parser')
	local name       = opts.name

	assert(not M.defs[name], 'Duplicate language name: ' .. name)

	local nvimQueryPath = root .. '/queries/' .. name .. '/highlights.scm'

	-- Peek at the nvim-treesitter query to see if it uses ; extends
	local usesExtends = false
	local f = io.open(nvimQueryPath)
	if f then
		local firstLine = f:read '*l'
		if firstLine and firstLine:match(EXTENDS_PATTERN) then
			usesExtends = true
		end
		f:close()
	end

	local def = {
		name      = name,
		files     = opts.files,
		soFile    = findParser(parserDir, name),
		queryFiles = {},
	}

	if usesExtends and runtimeDir then
		def.queryFiles.highlights = {
			runtimeDir .. '/queries/' .. name .. '/highlights.scm',
			nvimQueryPath,
		}
	else
		def.queryFiles.highlights = nvimQueryPath
	end

	M.defs[#M.defs + 1] = def
	M.defs[def.name] = def
end

function M.findDef(filename)
	local bestScore = 0
	local bestDef

	for i = #M.defs, 1, -1 do
		local def = M.defs[i]
		if not def.files then goto continue end

		for _, pattern in ipairs(def.files) do
			local s, e = filename:find(pattern)
			if not s then goto continue end

			local score = e - s
			if score > bestScore then
				bestScore = score
				bestDef = def
			end

			::continue::
		end

		::continue::
	end

	return bestDef
end

function M.getLang(def)
	local lang = M.langCache[def.name]
	if lang then
		return lang
	end

	local ok, result = pcall(ts.Language.load, def.soFile, def.name)
	if not ok then
		core.error('Error loading language ' .. def.name  .. ':\n' .. result)
		return nil
	end

	M.langCache[def.name] = result
	core.log('Loaded language ' .. def.name)

	return result
end

-- Load a single query file into the builder table.
-- Strips leading ; extends lines (Neovim meta-directive, not valid tree-sitter syntax).
-- Handles ; inherits: lang1, lang2 with spaces around commas.
local function loadQueryFile(path, builder, queryType)
	local f = io.open(path)
	if not f then return false end

	-- Scan header comment block for modelines
	while true do
		local head = f:read '*l'
		if not head or not head:match '%s*;' then
			break
		end

		-- Skip ; extends lines (strip from output)
		if head:match(EXTENDS_PATTERN) then
			goto continue
		end

		-- Handle ; inherits: lang1, lang2  (spaces after commas are ok)
		local rest = head:match '%s*;+%s*inherits%s*:%s*(.*)'
		if rest then
			for name in rest:gmatch '[%l_]+' do
				local inheritDef = M.defs[name]
				if not inheritDef then
					core.warn(
						'Could not find language %s to inherit queries from. \z
						Syntax highlighting may be incomplete.',
						name
					)
					goto continue
				end

				builder[#builder + 1] = '; TREESIT: INHERIT ' .. name .. '\n'
				builder[#builder + 1] = M.getQuery(M.defs[name], queryType)

				::continue::
			end
		end

		::continue::
	end

	f:seek('set', 0)
	local content = f:read '*a'
	f:close()
	-- Strip all ; extends lines (Neovim meta-directive, not valid tree-sitter syntax)
	content = content:gsub('[^\n]*;+%s*extends%s*\n', '')
	content = content:gsub('[^\n]*;+%s*extends%s*$', '')
	builder[#builder + 1] = content
	return true
end

function M.getQuery(def, queryType)
	local query = M.queryCache[queryType][def.name]
	if query then
		return query
	end

	local paths = def.queryFiles[queryType]
	-- Normalise to a list
	if type(paths) == 'string' then
		paths = { paths }
	end

	if not paths or #paths == 0 then
		core.error('No query files configured for ' .. def.name .. ' ' .. queryType)
		return nil
	end

	local builder = { '; TREESIT: BEGIN ' .. def.name .. '\n' }

	for _, path in ipairs(paths) do
		local ok = loadQueryFile(path, builder, queryType)
		if not ok then
			core.error('Error loading ' .. def.name .. ' ' .. queryType .. ' query: ' .. path)
			return nil
		end
	end

	builder[#builder + 1] = '; TREESIT: END ' .. def.name .. '\n'

	query = table.concat(builder)
	M.queryCache[queryType][def.name] = query
	core.log('Loaded ' .. def.name .. ' ' .. queryType .. ' query')

	return query
end

local queryRecents = {}

command.add(nil, {
	['treesit:view-highlights-query'] = function()
		core.command_view:enter('View highlights query for language', {
			submit = function(name)
				local def = M.defs[name]
				if not def then
					core.error('No such language %s', name)
					return
				end

				local doc = core.open_doc('highlights.scm')
				core.root_view:open_doc(doc)
				doc:insert(1, 1, M.getQuery(def, 'highlights'))
				doc.new_file = false
				doc:clean()
			end,

			suggest = function(name)
				local names = {}
				for _, def in ipairs(M.defs) do
					names[#names + 1] = def.name
				end

				return common.fuzzy_match_with_recents(names, queryRecents, name)
			end,
		})
	end,
})

return M
