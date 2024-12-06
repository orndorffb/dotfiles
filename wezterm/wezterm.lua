local wezterm = require 'wezterm'

local config = wezterm.config_builder()

-- Theme and font
config.color_scheme = 'NvimLight'
config.font_size = 12

config.window_decorations = "RESIZE"

return config
