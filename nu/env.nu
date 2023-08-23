# Nushell Environment Config File
#
# version = 0.83.1

# Specifies how environment variables are:
# - converted from a string to a value on Nushell startup (from_string)
# - converted from a value back to a string when running external commands (to_string)
# Note: The conversions happen *after* config.nu is loaded
$env.ENV_CONVERSIONS = {
    "PATH": {
        from_string: { |s| $s | split row (char esep) | path expand --no-symlink }
        to_string: { |v| $v | path expand --no-symlink | str join (char esep) }
    }
    "Path": {
        from_string: { |s| $s | split row (char esep) | path expand --no-symlink }
        to_string: { |v| $v | path expand --no-symlink | str join (char esep) }
    }
}


mut home = ''
try {
    if $nu.os-info.name == "windows" {
        $home = $env.HOME? | default $env.USERPROFILE
    } else {
        $home = $env.HOME
    }
}

$env.HOME = $home

# Directories to search for scripts when calling source or use
$env.NU_LIB_DIRS = [
    $env.HOME,
    ($env.HOME | path join nu_scripts)
]

# Directories to search for plugin binaries when calling register
$env.NU_PLUGIN_DIRS = [
    ($env.HOME | path join nu_plugins)
]

starship init nu | save -f ($env.HOME | path join starship_init.nu)
