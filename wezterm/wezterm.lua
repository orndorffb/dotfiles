local wezterm = require 'wezterm'

local config = wezterm.config_builder()

-- Theme and font
config.color_scheme = 'NvimLight'
config.font_size = 12

config.window_decorations = "RESIZE"
config.tab_bar_at_bottom = true

-- Tmux like bindings
-- config.leader = { key = 'a', mods = 'CTRL', timeout_milliseconds = 1001 }
config.keys = {
  {
    mods   = "LEADER",
    key    = "-",
    action = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain' }
  },
  {
    mods   = "LEADER",
    key    = "=",
    action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' }
  },
  {
    mods = 'LEADER',
    key = 'z',
    action = wezterm.action.TogglePaneZoomState
  },
  {
    key = '[',
    mods = 'LEADER',
    action = wezterm.action.ActivateCopyMode,
  },
  {
    key = 'h',
    mods = 'LEADER',
    action = wezterm.action{ActivatePaneDirection="Left"}
  },
  {
    key = 'j',
    mods = 'LEADER',
    action = wezterm.action{ActivatePaneDirection="Down"}
  },
  {
    key = 'k',
    mods = 'LEADER',
    action = wezterm.action{ActivatePaneDirection="Up"}
  },
  {
    key = 'l',
    mods = 'LEADER',
    action = wezterm.action{ActivatePaneDirection="Right"}
  }
}


return config
