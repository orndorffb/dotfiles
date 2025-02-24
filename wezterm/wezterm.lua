local wezterm = require 'wezterm'

local config = wezterm.config_builder()

-- Theme and font

-- config.color_scheme = 'zenbones_dark'
 config.color_scheme = 'zenbones'
-- config.color_scheme = 'Everforest Dark Hard (Gogh)'
config.font_size = 12

config.window_decorations = "RESIZE"
config.tab_bar_at_bottom = true
config.inactive_pane_hsb = {
  saturation = 1,
  brightness = 1,
}

config.unix_domains = {
  {
    name = 'unix',
  },
}


-- Tmux like bindings
-- config.leader = { key = 'a', mods = 'CTRL', timeout_milliseconds = 1000 }
config.keys = {
  {
    mods   = "LEADER",
    key    = "-",
    action = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain' }
  },
  {
    mods   = "LEADER",
    key    = "|",
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
