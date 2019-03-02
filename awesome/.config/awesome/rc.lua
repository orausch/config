-- Standard awesome library
local gears = require("gears")
local lain = require("lain")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local hotkeys_popup = require("awful.hotkeys_popup").widget


-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = tostring(err) })
        in_error = false
    end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
beautiful.init("/home/orausch/.config/awesome/default/theme.lua")
--beautiful.init("/usr/share/awesome/themes/default/theme.lua")
--beautiful.font = "Monospace 12"
beautiful.font = "Terminus Bold 13"
beautiful.useless_gap = 0
gears.wallpaper.set(beautiful.bg_normal)
	
-- This is used later as the default terminal and editor to run.
terminal = "uxterm"
editor = os.getenv("EDITOR") or "editor"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
    awful.layout.suit.tile,
    lain.layout.centerwork,
	awful.layout.suit.fair
}
-- }}}

-- {{{ Helper functions
local function client_menu_toggle_fn()
    local instance = nil

    return function ()
        if instance and instance.wibox.visible then
            instance:hide()
            instance = nil
        else
            instance = awful.menu.clients({ theme = { width = 250 } })
        end
    end
end
-- }}}

-- {{{ Menu
-- Create a launcher widget and a main menu
myawesomemenu = {
   { "hotkeys", function() return false, hotkeys_popup.show2help end},
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end}
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "open terminal", terminal }
                                  }
                        })

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

-- {{{ Wibar

-- Create a wibox for each screen and add it
local taglist_buttons = awful.util.table.join(
                    awful.button({ }, 1, function(t) t:view_only() end),
                    awful.button({ modkey }, 1, function(t)
                                              if client.focus then
                                                  client.focus:move_to_tag(t)
                                              end
                                          end),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, function(t)
                                              if client.focus then
                                                  client.focus:toggle_tag(t)
                                              end
                                          end),
                    awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
                    awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
                )



local function set_wallpaper(s)
    -- Wallpaper
    if beautiful.wallpaper then
        local wallpaper = beautiful.wallpaper
        -- If wallpaper is a function, call it with the screen
        if type(wallpaper) == "function" then
            wallpaper = wallpaper(s)
        end
        gears.wallpaper.set(beautiful.bg_normal)
    end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

-- WIDGETS --

--
local white = beautiful.fg_normal
local back = beautiful.bg_normal
local orange = "#ffa500"
local red = "#ff0000"

local markup = lain.util.markup
local separator = markup.color("#777777", back, " | ")
local mpris = awful.widget.watch(
    { awful.util.shell, "-c", "playerctl status && playerctl metadata" },
    2,
    function(widget, stdout)
        state = string.match(stdout, "Playing") or
                           string.match(stdout, "Paused")  or ""

		if state == "Playing" then
			state = " PLAYING: "
		elseif state == "Paused" then
			state = " PAUSED: "
		end
		state = state
		title = stdout:match("title%s+([^\n]*)") or ""
		artist = stdout:match("artist%s+([^\n]*)") or ""
        -- customize here
        widget:set_markup((markup.color("#7777FF", back, state) .. artist .. " - " .. title) or "")
    end
)
mpris:connect_signal("button::press", function(_,_,_,button)
	if (button == 2)     then awful.spawn("playerctl previous", false)
	elseif (button == 3) then awful.spawn("playerctl next", false)
	elseif (button == 1) then awful.spawn("playerctl play-pause", false)
	end
	end
)

local cpu = lain.widget.cpu {
    settings = function()
		markup_string = "CPU: "
		fg_color = white
		if cpu_now.usage > 90 then
			fg_color = red
		elseif cpu_now.usage > 60 then
			fg_color = orange
		end
        widget:set_markup("CPU: " 
			.. markup.color(fg_color, back, cpu_now.usage .. "%") 
			.. separator )
    end
}

local summary = nil
function show_tooltip()
    local font = 'Terminus 10'
    local text_color = '#FFFFFF'
    local fd = io.popen(os.getenv("HOME") .. "/.config/awesome/mem.sh summary")
    local str = fd:read("*all")
    local content = string.format('<span font="%s" foreground="%s">%s</span>', font, text_color, str)
    summary = naughty.notify({
--        title = "Memory Usage",
        text = content,
        timeout = 0,
        hover_timeout = 0.5,
        width = 60*8
    })
end

function hide_tooltip()
    if summary ~= nil then
        naughty.destroy(summary)
    end
end

local mem = lain.widget.mem {
	settings = function()
		fg_color = white
		if mem_now.used > 50000 then
			fg_color = orange
		elseif mem_now.used > 10000 then
			fg_color = red
		end
        widget:set_markup("MEM: " 
			.. markup.color(fg_color, back, mem_now.used .. "MB")
			.. separator)

		widget:connect_signal("mouse::enter", show_tooltip)
		widget:connect_signal("mouse::leave", hide_tooltip)
	end
}

local volume = lain.widget.pulse {
	settings = function()
		vlevel = volume_now.left .. "%"
        if volume_now.muted == "yes" then
            vlevel = markup.color("#999999", back, "MUTE")
        end
        widget:set_markup("VOL: " .. vlevel  .. separator)
	end

}

textclock = wibox.widget.textclock("%A %d %B %H:%M ")
local cal = lain.widget.cal {
    attach_to = { textclock},
	icons="",
}

local bat = lain.widget.bat {
	battery = "BAT0",
	timeout  = 10,
	settings = function()
		fg_color = white
		if bat_now.perc < 40 then
			fg_color = orange
		elseif bat_now.perc < 15 then
			fg_color = red
		end

     	widget:set_markup("BAT: " 
			.. markup.color(fg_color, back, bat_now.perc .. "% ") 
			.. string.sub(bat_now.status, 1, 1)
			.. "(" .. bat_now.time .. ")"
			.. separator)
 	end
}
-- local net = lain.widget.net {
-- 	iface = "wlp2s0",
-- 	wifi_state = "on",
-- 	settings = function()
-- 		if net_now.state == "up" then
-- 			widget:set_markup("NET: " .. -net_now.devices.wlp2s0.signal .. separator)
-- 		else
-- 			widget:set_markup("NET: " .. net_now.state .. separator)
-- 		end
--  	end

-- }






awful.screen.connect_for_each_screen(function(s)
    -- Wallpaper
    set_wallpaper(s)

    -- Each screen has its own tag table.
    awful.tag({"1", "2", "3", "4", "5", "6", "7", "8"}, s, awful.layout.layouts[1])
    awful.tag.add("9", {
			screen = s,
			layout = awful.layout.layouts[3]
		})

	s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all, taglist_buttons)

    s.mywibox = awful.wibar({ position = "top", screen = s, height=28 })
	s.mylayoutbox = awful.widget.layoutbox(s)

    -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            --mylauncher,
            s.mytaglist,
			mpris
        },
		nil,
        { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
			volume.widget,
			cpu.widget,
			mem.widget,
			bat.widget,
			-- net.widget,
			textclock,
			wibox.widget.systray()
        },
    }
end)
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
    awful.key({ modkey,           }, "s",      hotkeys_popup.show_help,
              {description="show help", group="awesome"}),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore,
              {description = "go back", group = "tag"}),

    awful.key({ modkey,           }, "k", function () awful.client.focus.global_bydirection("up") end,
        {description = "focus up", group = "client"}),
    awful.key({ modkey,           }, "j", function () awful.client.focus.global_bydirection("down") end,
        {description = "focus down", group = "client"}),
    awful.key({ modkey,           }, "h", function () awful.client.focus.global_bydirection("left") end,
        {description = "focus left", group = "client"}),
    awful.key({ modkey,           }, "l", function () awful.client.focus.global_bydirection("right") end,
        {description = "focus right", group = "client"}),
    awful.key({ modkey,           }, "w", function () mymainmenu:show() end,
              {description = "show main menu", group = "awesome"}),

    -- Layout manipulation
	--
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end,
              {description = "focus the next screen", group = "screen"}),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end,
              {description = "focus the previous screen", group = "screen"}),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
              {description = "jump to urgent client", group = "client"}),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "go back", group = "client"}),

	awful.key({ }, "XF86AudioRaiseVolume", function () 
		awful.util.spawn("amixer set Master 2%+", false)
		volume.update()
	end),
	awful.key({ }, "XF86AudioLowerVolume", function () 
		awful.util.spawn("amixer set Master 2%-", false)
		volume.update() 
	end),
	awful.key({ }, "XF86AudioMute", function () 
		awful.util.spawn("pactl set-sink-mute 0 toggle", false) 
		volume.update() 
	end),
	awful.key({ }, "XF86MonBrightnessUp", function () awful.util.spawn("xbacklight -inc 10", false) end),
	awful.key({ }, "XF86MonBrightnessDown", function () awful.util.spawn("xbacklight -dec 10", false) end),
    -- Standard program
    awful.key({ modkey, "Shift"   }, "Return", function () awful.spawn(terminal) end,
              {description = "open a terminal", group = "launcher"}),
    awful.key({ modkey, "Shift"}, "f", function () awful.spawn("caja") end,
              {description = "open file manager", group = "launcher"}),
    awful.key({ modkey}, "v", function () awful.spawn("nvim-qt") end,
              {description = "open gvim", group = "launcher"}),
    awful.key({ modkey, "Control"}, "l", function () awful.spawn("i3lock-fancy -p -t \"\"") end,
              {description = "lock screen", group = "launcher"}),
    awful.key({ modkey, "Control" }, "r", awesome.restart,
              {description = "reload awesome", group = "awesome"}),

    awful.key({ modkey,"Shift"    }, "l",     function () awful.tag.incmwfact( 0.05)          end,
              {description = "increase master width factor", group = "layout"}),
    awful.key({ modkey,"Shift"    }, "h",     function () awful.tag.incmwfact(-0.05)          end,
              {description = "decrease master width factor", group = "layout"}),
    awful.key({ modkey,           }, ",",     function () awful.tag.incnmaster( 1, nil, true) end,
              {description = "increase the number of master clients", group = "layout"}),
    awful.key({ modkey,           }, ".",     function () awful.tag.incnmaster(-1, nil, true) end,
              {description = "decrease the number of master clients", group = "layout"}),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1, nil, true)    end,
              {description = "increase the number of columns", group = "layout"}),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1, nil, true)    end,
              {description = "decrease the number of columns", group = "layout"}),
    awful.key({ modkey,           }, "p", function () awful.layout.inc( 1)                end,
              {description = "select next", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(-1)                end,
              {description = "select previous", group = "layout"}),

    awful.key({ modkey, "Control" }, "n",
              function ()
                  local c = awful.client.restore()
                  -- Focus restored client
                  if c then
                      client.focus = c
                      c:raise()
                  end
              end,

              {description = "restore minimized", group = "client"}),

    -- Prompt
    awful.key({ modkey },            "space",     function () awful.spawn(
			string.format("j4-dmenu-desktop --no-generic --dmenu=\"dmenu -i  -nb '%s' -nf '%s' -sf '%s' -sb '%s'\"", 
				beautiful.bg_normal, beautiful.fg_normal, beautiful.fg_focus, beautiful.bg_focus)
		, false) end,
              {description = "run prompt", group = "launcher"}),

    awful.key({ modkey }, "x",
              function ()
                  awful.prompt.run {
                    prompt       = "Run Lua code: ",
                    textbox      = awful.screen.focused().mypromptbox.widget,
                    exe_callback = awful.util.eval,
                    history_path = awful.util.get_cache_dir() .. "/history_eval"
                  }
              end,
              {description = "lua execute prompt", group = "awesome"})
)

clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",
        function (c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = "toggle fullscreen", group = "client"}),
    awful.key({ modkey}, "q",      function (c) c:kill()                         end,
              {description = "close", group = "client"}),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ,
              {description = "toggle floating", group = "client"}),
    awful.key({ modkey}, "Return", function (c) c:swap(awful.client.getmaster()) end,
              {description = "move to master", group = "client"}),
    awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end,
              {description = "move to screen", group = "client"}),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
              {description = "toggle keep on top", group = "client"}),
    awful.key({ modkey,           }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end ,
        {description = "minimize", group = "client"}),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized = not c.maximized
            c:raise()
        end ,
        {description = "maximize", group = "client"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys = awful.util.table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = awful.screen.focused()
                        local tag = screen.tags[i]
                        if tag then
                           tag:view_only()
                        end
                  end,
                  {description = "view tag #"..i, group = "tag"}),
        -- Toggle tag display.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = awful.screen.focused()
                      local tag = screen.tags[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end,
                  {description = "toggle tag #" .. i, group = "tag"}),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:move_to_tag(tag)
                          end
                     end
                  end,
                  {description = "move focused client to tag #"..i, group = "tag"}),
        -- Toggle tag on focused client.
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:toggle_tag(tag)
                          end
                      end
                  end,
                  {description = "toggle focused client on tag #" .. i, group = "tag"})
    )

end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons,
                     screen = awful.screen.preferred,
                     placement = awful.placement.no_overlap+awful.placement.no_offscreen,
 					 size_hints_honor = false --remove gaps from windows
     }
    },

    -- Floating clients.
    -- { rule_any = {
    --     instance = {
    --       "DTA",  -- Firefox addon DownThemAll.
    --       "copyq",  -- Includes session name in class.
    --     },
    --     class = {
    --       "Arandr",
    --       "Gpick",
    --       "Kruler",
    --       "MessageWin",  -- kalarm.
    --       "Sxiv",
    --       "Wpa_gui",
    --       "pinentry",
    --       "veromix",
    --       "xtightvncviewer"},

    --     name = {
    --       "Event Tester",  -- xev.
    --     },
    --     role = {
    --       "AlarmWindow",  -- Thunderbird's calendar.
    --       "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
    --     }
    --   }, properties = { floating = true }},

    -- { rule_any = {
    --     class = {
    --       "libreoffice",
    --       },
    --   }, properties = { floating = false }},

    -- Add titlebars to normal clients and dialogs
--    { rule_any = {type = { "normal", "dialog" }
--      }, properties = { titlebars_enabled = false }
--    },

    -- Set Firefox to always map on the tag named "2" on screen 1.
	{ rule = { class = "Firefox" },
	properties = { screen = 1, tag = "1" } },

	{ rule = { instance = "claws-mail" },
	properties = { screen = 1, tag = "4" } },

	{ rule = { instance = "geary" },
	properties = { screen = 1, tag = "4" } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
     if not awesome.startup then awful.client.setslave(c) end 
    if awesome.startup and
      not c.size_hints.user_position
      and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
    -- buttons for the titlebar
    local buttons = awful.util.table.join(
        awful.button({ }, 1, function()
            client.focus = c
            c:raise()
            awful.mouse.client.move(c)
        end),
        awful.button({ }, 3, function()
            client.focus = c
            c:raise()
            awful.mouse.client.resize(c)
        end)
    )

    awful.titlebar(c) : setup {
        { -- Left
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        { -- Middle
            { -- Title
                align  = "center",
                widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = wibox.layout.flex.horizontal
        },
        { -- Right
            awful.titlebar.widget.floatingbutton (c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.stickybutton   (c),
            awful.titlebar.widget.ontopbutton    (c),
            awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
    if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
        and awful.client.focus.filter(c) then
        client.focus = c
    end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

-- }}}

-- Autorun programs
autorun = true
autorunApps = 
{ 
	"xrdb ~/.Xresources",
	"xset r rate 200 35",
	"setxkbmap -option caps:escape -option altwin:swap_lalt_lwin -layout 'us(altgr-intl)'",
	"nm-applet",
	-- "xscreensaver",
	--"pnmixer",
	--"/home/oliver/.config/awesome/autorun.sh",
	-- "xss-lock -- xscreensaver-command -lock",
}
if autorun then
	for app = 1, #autorunApps do
		awful.util.spawn(autorunApps[app], false)
	end
end
-- client.connect_signal("manage", function (c)
--     c.shape = function(cr,w,h)
--         gears.shape.rounded_rect(cr,w,h,5)
--     end
-- end)
