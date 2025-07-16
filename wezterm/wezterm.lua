local wezterm = require 'wezterm'

local config = wezterm.config_builder()

-- Theme and font

-- config.color_scheme = 'zenbones_dark'
config.color_scheme = 'zenbones'
-- config.color_scheme = "Tokyo Night"
config.font_size = 12

config.font = wezterm.font("Essential PragmataPro")

config.window_decorations = "RESIZE"
config.tab_bar_at_bottom = true
config.inactive_pane_hsb = {
  saturation = 1,
  brightness = 1,
}


return config
