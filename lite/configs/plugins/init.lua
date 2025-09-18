local core = require "core"
local config = require "core.config"

config.plugins.miq.debug = false
config.plugins.miq.repos = {
    'https://github.com/lite-xl/lite-xl-plugins.git:master',
    'https://github.com/Evergreen-lxl/evergreen-languages.git:main',
}

config.plugins.miq.plugins = {
  -- this allows Miq to manage itself
  'TorchedSammy/Miq',

  'editorconfig',
  'cleanstart',
  'indentguide',

  {'https://codeberg.org/Mandarancio/lite-modal.git', name='modal'},

  {'https://codeberg.org/Mandarancio/lite-buffers.git', name='buffers'},

  'Evergreen-lxl/Evergreen.lxl',
  'evergreen_c',
  'evergreen_cpp',
  'evergreen_lua',
  'evergreen_python',
  'evergreen_json',
  'evergreen_bash',
  'evergreen_cmake',
  'evergreen_rust',
}
