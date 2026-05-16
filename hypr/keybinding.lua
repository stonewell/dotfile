local mainMod = "SUPER" -- Sets "Windows" key as main modifier

-- Window
hl.bind(mainMod .. " + V", hl.dsp.window.float({ action = "toggle" }))
hl.bind(mainMod .. " + SHIFT + C", hl.dsp.window.close())

-- Layout
hl.bind(mainMod .. " + SHIFT + return", hl.dsp.layout("swapwithmaster master"))
hl.bind(mainMod .. " + L", hl.dsp.layout("cyclenext"))
hl.bind(mainMod .. " + J", hl.dsp.layout("cycleprev"))

-- Command
hl.bind(mainMod .. " + SHIFT + Q", hl.dsp.exec_cmd("uwsm stop"))
hl.bind(mainMod .. " + return", hl.dsp.exec_cmd("uwsm app -- ~/.run_term_cmd"))
hl.bind("CTRL + ALT + space", hl.dsp.exec_cmd("uwsm app -- ~/.run_launcher"))
hl.bind(mainMod .. " + SHIFT + L", hl.dsp.exec_cmd("hyprlock"))
hl.bind(mainMod .. " + SHIFT + I", hl.dsp.exec_cmd("matcha -t && pkill -RTMIN+8 waybar"))
hl.bind(mainMod .. " + ALT + N", hl.dsp.workspace.move({monitor = "+1"}))

-- Switch workspaces with mainMod + [0-9]
-- Move active window to a workspace with mainMod + SHIFT + [0-9]
for i = 1, 10 do
    local key = i % 10 -- 10 maps to key 0
    hl.bind(mainMod .. " + " .. key,             hl.dsp.focus({ workspace = i}))
    hl.bind(mainMod .. " + SHIFT + " .. key,     hl.dsp.window.move({ workspace = i }))
end

