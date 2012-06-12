--------------------------
-- Default luakit theme --
--------------------------

local theme = {}

-- Default settings
theme.font = "terminus normal 9"
theme.fg   = "#d0d0d0"
theme.bg   = "#2c2c2c"

-- Genaral colours
theme.success_fg = "#0f0"
theme.loaded_fg  = "#33AADD"
theme.error_fg = "#FFF"
theme.error_bg = "#F00"

-- Warning colours
theme.warning_fg = "#F00"
theme.warning_bg = "#FFF"

-- Notification colours
theme.notif_fg = "#444"
theme.notif_bg = "#FFF"

-- Menu colours
theme.menu_fg                   = "#888"
theme.menu_bg                   = "#222222"
theme.menu_selected_fg          = "#c37561"
theme.menu_selected_bg          = "#2c2c2c"
theme.menu_title_bg             = "#222222"
theme.menu_primary_title_fg     = "#98b9b1"
theme.menu_secondary_title_fg   = "#666"

-- Proxy manager
theme.proxy_active_menu_fg      = '#000'
theme.proxy_active_menu_bg      = '#FFF'
theme.proxy_inactive_menu_fg    = '#888'
theme.proxy_inactive_menu_bg    = '#FFF'

-- Statusbar specific
theme.sbar_fg         = "#d0d0d0"
theme.sbar_bg         = "#222222"

-- Downloadbar specific
theme.dbar_fg         = "#fff"
theme.dbar_bg         = "#000"
theme.dbar_error_fg   = "#F00"

-- Input bar specific
theme.ibar_fg           = "#d0d0d0"
theme.ibar_bg           = "#222222"

-- Tab label
theme.tab_fg            = "#888"
theme.tab_bg            = "#222222"
theme.tab_ntheme        = "#ddd"
theme.selected_fg       = "#c37561"
theme.selected_bg       = "#2c2c2c"
theme.selected_ntheme   = "#ddd"
theme.loading_fg        = "#33AADD"
theme.loading_bg        = "#000"

-- Trusted/untrusted ssl colours
theme.trust_fg          = "#0F0"
theme.notrust_fg        = "#F00"

-- hints theming
theme.follow = {}
theme.follow = {}
theme.follow.focus_bg = "#00ff00";
theme.follow.normal_bg = "#ffff99";
theme.follow.opacity = 0.3;
theme.follow.border = "1px dotted #000000";
theme.follow.frame_border = "2px solid #880000";
theme.follow.tick_frame_bg = "#880000";
theme.follow.tick_fg = "#ffffff";
theme.follow.tick_bg = "#000000";
theme.follow.tick_border = "2px dashed #000000";
theme.follow.tick_opacity = 0.35;
theme.follow.tick_font = "9px terminus";
theme.follow.vert_offset = 0;
theme.follow.horiz_offset = 0;
return theme
-- vim: et:sw=4:ts=8:sts=4:tw=80
