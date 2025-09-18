local core = require "core"
local config = require "core.config"
local style = require "core.style"

------------------------------ Themes ----------------------------------------

-- light theme:
-- core.reload_module("colors.summer")
if PLATFORM == "Windows" then

  style.font = renderer.font.load(DATADIR .. "/fonts/FiraSans-Regular.ttf",
                                  22 * SCALE,
                                  {antialiasing ="subpixel", hinting="full", smoothing=true}
  )
  style.code_font = renderer.font.load(DATADIR .. "/fonts/JetBrainsMono-Regular.ttf",
                                       22 * SCALE,
                                       {antialiasing ="subpixel", hinting="full", smoothing=true}
  )

end
