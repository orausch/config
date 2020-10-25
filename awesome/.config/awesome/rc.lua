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
--local naughty = require("naughty")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup")
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")

--naughty.config.defaults['icon_size'] = 64
-- Load Debian menu entries


-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
--if awesome.startup_errors then
--    naughty.notify({ preset = naughty.config.presets.critical,
--                     title = "Oops, there were errors during startup!",
--                     text = awesome.startup_errors })
--end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal(
        "debug::error",
        function (err)
            -- Make sure we don't go into an endless error loop
            if in_error then return end
            in_error = true

            --naughty.notify({ preset = naughty.config.presets.critical,
            --                 title = "Oops, an error happened!",
            --                 text = tostring(err) })
            in_error = false
    end)
end

-- Themes define colours, icons, font and wallpapers.
beautiful.init("/home/orausch/.config/awesome/default/theme.lua")
--beautiful.init("/usr/share/awesome/themes/default/theme.lua")
beautiful.font = "Roboto Mono Bold 13"
--beautiful.font = "Terminus (TTF) Bold 12"
beautiful.useless_gap = 0
beautiful.maximized_hide_border = true
gears.wallpaper.set(beautiful.bg_normal)

-- This is used later as the default terminal and editor to run.
terminal = "xfce4-terminal"
editor = os.getenv("EDITOR") or "editor"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- disable snapping in floating mode
awful.mouse.snap.edge_enabled = false
awful.mouse.snap.client_enabled = false
awful.mouse.drag_to_tag.enabled = false

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
    awful.layout.suit.tile.right,
    awful.layout.suit.tile.bottom,
}


-- {{{ Menu
-- Create a launcher widget and a main menu
myawesomemenu = {
    { "hotkeys", function() hotkeys_popup.show_help(nil, awful.screen.focused()) end },
    { "manual", terminal .. " -e man awesome" },
    { "edit config", editor_cmd .. " " .. awesome.conffile },
    { "restart", awesome.restart },
    { "quit", function() awesome.quit() end },
}

local menu_awesome = { "awesome", myawesomemenu, beautiful.awesome_icon }
local menu_terminal = { "open terminal", terminal }


-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- Keyboard map indicator and switcher
mykeyboardlayout = awful.widget.keyboardlayout()

-- {{{ Wibar
-- Create a textclock widget
-- Create a wibox for each screen and add it
local taglist_buttons = gears.table.join(
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

local tasklist_buttons = gears.table.join(
    awful.button({ }, 1, function (c)
            if c == client.focus then
                c.minimized = true
            else
                c:emit_signal(
                    "request::activate",
                    "tasklist",
                    {raise = true}
                )
            end
    end),
    awful.button({ }, 3, function()
            awful.menu.client_list({ theme = { width = 250 } })
    end),
    awful.button({ }, 4, function ()
            awful.client.focus.byidx(1)
    end),
    awful.button({ }, 5, function ()
            awful.client.focus.byidx(-1)
end))

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
--screen.connect_signal("property::geometry", set_wallpaper)

-- WIDGETS --

--
--local white = beautiful.fg_normal
--local back = beautiful.taglist_bg_occupied
--local full_back = beautiful.bg_normal
--local orange = "#ffa500"
--local red = "#ff0000"
--
--local markup = lain.util.markup
--local separator = wibox.widget {
--    markup = markup.color(beautiful.taglist_fg_occupied, full_back, ' | '),
--    align = 'center',
--    valign = 'center',
--    widget = wibox.widget.textbox
--}
--
--function add_background(widget)
--    return wibox.container.margin(
--        wibox.container.background(
--            wibox.container.margin(widget, 4, 4, 4, 4),
--            beautiful.taglist_bg_occupied,
--            beautiful.taglist_shape
--        ),
--        4, 4, 4, 4
--    )
--
--end
--
--local mpris, mpris_timer = awful.widget.watch(
--    { awful.util.shell, "-c", "playerctl status; playerctl metadata" },
--    2,
--    function(widget, stdout)
--        state = string.match(stdout, "Playing") or
--            string.match(stdout, "Paused")  or ""
--
--        if state == "Playing" then
--            state = " PLAYING: "
--        elseif state == "Paused" then
--            state = " PAUSED: "
--        end
--        state = state
--        title = stdout:match("title%s+([^\n]*)") or ""
--        artist = stdout:match("artist%s+([^\n]*)") or ""
--        -- customize here
--        widget:set_markup((markup.color("#7777FF", full_back, state) .. artist .. " - " .. title) or "")
--    end
--)
--
--function trim1(s)
--    return (s:gsub("^%s*(.-)%s*$", "%1"))
--end
----
----local current_task = awful.widget.watch(
----    { awful.util.shell, "-c", "emacsclient -e \" (if (and (boundp 'org-clock-current-task) org-clock-current-task) (substring-no-properties (org-clock-get-clock-string)))\" | tr -d \"\\\"\" | awk '{$1=$1};1'" },
----    10,
----    function(widget, stdout)
----	stdout = trim1(stdout)
----	if stdout == "nil" then
----	    stdout = markup.color("#777777", back, "none")
----	end
----	-- customize here
----	widget:set_markup("TASK: " .. trim1(stdout) .. separator)
----    end
----)
----
----local vpn_name = awful.widget.watch(
----    { awful.util.shell, "-c", "nmcli connection show --active" },
----    10,
----    function(widget, stdout)
----	vpnname = markup.color("#777777", back, "none")
----	if string.match(stdout, "full") then
----	    vpnname = "moat-full"
----	elseif string.match(stdout, "moat") then
----	    vpnname = markup.color(back, "#4D9DE0","moat")
----	elseif string.match(stdout, "oracle") then
----	    vpnname = markup.color( back , "#D1350F","oracle")
----	end
----	-- customize here
----	widget:set_markup("VPN: " .. vpnname .. separator)
----    end
----)
--mpris:connect_signal(
--    "button::press",
--    function(_,_,_,button)
--        if (button == 2) then
--            awful.spawn.with_line_callback(
--                "playerctl previous",
--                { exit = function() mpris_timer:emit_signal("timeout") end}
--            )
--        elseif (button == 3) then
--            awful.spawn.with_line_callback(
--                "playerctl next",
--                { exit = function() mpris_timer:emit_signal("timeout") end}
--            )
--        elseif (button == 1) then
--            awful.spawn.with_line_callback(
--                "playerctl play-pause",
--                { exit = function() mpris_timer:emit_signal("timeout") end}
--            )
--        end
--    end
--)
--
--local cpu_widget = require("awesome-wm-widgets.cpu-widget.cpu-widget")
--
--
----local cpu = wibox.widget {
----        cpu_widget({
----                step_width = 3,
----                width = 50
----        }),
----        wibox.widget.textbox("test"),
----        layout = wibox.layout.align.horizonal
----    }
--
--local cpu = add_background(wibox.widget {
--                               wibox.widget.textbox("CPU: "),
--                               cpu_widget({
--                                       step_width = 3,
--                                       width = 50
--                               }),
--                               layout = wibox.layout.align.horizontal
--                                        }
--)
--
--local summary = nil
--function show_tooltip()
--    local font = 'Terminus (TTF) 12'
--    local text_color = '#FFFFFF'
--    local fd = io.popen(os.getenv("HOME") .. "/.config/awesome/mem.sh summary")
--    local str = fd:read("*all")
--    local content = string.format('<span font="%s" foreground="%s">%s</span>', font, text_color, str)
--    summary = naughty.notify({
--            --        title = "Memory Usage",
--            text = content,
--            timeout = 0,
--            hover_timeout = 0.5,
--            width = 60*8
--    })
--end
--
--function hide_tooltip()
--    if summary ~= nil then
--        naughty.destroy(summary)
--    end
--end
--
--local mem = lain.widget.mem {
--    settings =
--        function()
--            fg_color = white
--            if mem_now.used > 50000 then
--                fg_color = orange
--            elseif mem_now.used > 10000 then
--                fg_color = red
--            end
--            widget:set_markup(
--                "MEM: "
--                    .. markup.color(fg_color, back, mem_now.used .. "MB"))
--
--            widget:connect_signal("mouse::enter", show_tooltip)
--            widget:connect_signal("mouse::leave", hide_tooltip)
--        end
--}
--
--local mem = add_background(mem.widget)
--
--local textclock = wibox.widget.textclock("%A %d %B %H:%M")
--local textclock = add_background(textclock)
--local cal = lain.widget.cal {
--    attach_to = { textclock},
--    icons="",
--}
--
--local bat = lain.widget.bat {
--    battery = "BAT0",
--    timeout  = 10,
--    settings =
--        function()
--            fg_color = white
--            if bat_now.perc < 40 then
--                fg_color = orange
--            elseif bat_now.perc < 15 then
--                fg_color = red
--            end
--
--            widget:set_markup(
--                "BAT: " ..
--                    markup.color(fg_color, back, bat_now.perc .. "% ") ..
--                    string.sub(bat_now.status, 1, 1)..
--                    "(" .. bat_now.time .. ")"
--            )
--        end
--}
--local bat = add_background(bat.widget)

smallest_screen = nil
smallest_screen_size = 1e300

largest_screen = nil
largest_screen_size = 0

awful.screen.connect_for_each_screen(
    function(s)
        -- Wallpaper
        set_wallpaper(s)

        -- figure out the smallest and largest screen
        if s.geometry.width * s.geometry.height < smallest_screen_size then
            smallest_screen = s
            smallest_screen_size = s.geometry.width * s.geometry.height
        end

        if s.geometry.width * s.geometry.height > largest_screen_size then
            largest_screen = s
            largest_screen_size = s.geometry.width * s.geometry.height
        end

        -- Each screen has its own tag table.
        if s.geometry.width >= s.geometry.height then
            awful.tag({"1", "2", "3", "4", "5", "6", "7", "8", "9"}, s, awful.layout.layouts[1])
        else
            awful.tag({"1", "2", "3", "4", "5", "6", "7", "8", "9"}, s, awful.layout.layouts[2])
        end

        s.mytaglist = wibox.container.margin(
            awful.widget.taglist(s, function(t) return t.selected or #t:clients() > 0 end, taglist_buttons), 0, 0, 3, 3)

        s.mylayoutbox = awful.widget.layoutbox(s)
        s.mywibox = awful.wibar({ position = "top", screen = s, height= 32 })
        s.mylayoutbox = awful.widget.layoutbox(s)

        -- Add widgets to the wibox
        s.mywibox:setup (
            {
                layout = wibox.layout.align.horizontal,
                {
                    layout = wibox.layout.fixed.horizontal,
                    s.mytaglist,
                },
                nil,
		nil,
                --{
                --    layout = wibox.layout.fixed.horizontal,
                --    cpu,
                --    bat,
                --    textclock,
                --    s.mylayoutbox,
                --},
            }
        )
    end
)
if smallest_screen.geometry.width >= smallest_screen.geometry.height then
    smallest_screen_layout = awful.layout.suit.tile.right
else
    smallest_screen_layout = awful.layout.suit.tile.bottom
end

zoom_tag = awful.tag.add(
    "Z",
    {
        screen = smallest_screen,
        layout = smallest_screen_layout
    }
)
mail_tag = awful.tag.add(
    "✉",
    {
        screen = smallest_screen,
        layout = smallest_screen_layout
    }
)

music_tag = awful.tag.add(
    "♪",
    {
        screen = smallest_screen,
        layout = smallest_screen_layout
    }
)


largest_screen.mywibox:setup (
    {
        layout = wibox.layout.align.horizontal,
        {
            layout = wibox.layout.fixed.horizontal,
            largest_screen.mytaglist,
        --    mpris,
        },
        nil,
	nil,
        --{
        --    layout = wibox.layout.fixed.horizontal,
        --    --current_task,
        --    --vpn_name,
        --    --cpu,
        --    --separator,
        --    --mem,
        --    separator,
        --    bat,
        --    separator,
        --    textclock,
        --    separator,
        --    wibox.container.margin(wibox.widget.systray(), 0, 0, 4, 4),
        --    separator,
        --    largest_screen.mylayoutbox,
        --},
    }
)





-- {{{ Mouse bindings
root.buttons(gears.table.join(
                 awful.button({ }, 4, awful.tag.viewnext),
                 awful.button({ }, 5, awful.tag.viewprev)
))

-- Key bindings
globalkeys = awful.util.table.join(
    awful.key(
        { modkey,           },"F1", hotkeys_popup.show_help,
        {description="show help", group="awesome"}
    ),
    awful.key(
        { modkey,           }, "Escape", awful.tag.history.restore,
        {description = "go back", group = "tag"}
    ),

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
            awful.util.spawn("amixer set Master toggle", false)
            volume.update()
    end),
    awful.key({ }, "XF86MonBrightnessUp", function () awful.util.spawn("xbacklight -inc 10", false) end),
    awful.key({ }, "XF86MonBrightnessDown", function () awful.util.spawn("xbacklight -dec 10", false) end),
    -- Standard program
    awful.key({ modkey, "Shift"}, "Return", function () awful.spawn(terminal) end,
        {description = "open a terminal", group = "launcher"}),
    awful.key({ modkey, "Shift"}, "f", function () awful.spawn("mate-terminal -e ranger") end,
        {description = "open file manager", group = "launcher"}),
    awful.key({ modkey}, "y", function () awful.spawn("jupyter qtconsole") end,
        {description = "open qtconsole", group = "launcher"}),
    awful.key({ modkey}, "a", function () awful.spawn("xterm -T aerc -e aerc") end,
        {description = "open aerc", group = "launcher"}),
    awful.key({ modkey, "Control"}, "l", function () awful.spawn("i3lock -c 222222") end,
        {description = "lock screen", group = "launcher"}),
    awful.key({ modkey, "Control" }, "r", awesome.restart,
        {description = "reload awesome", group = "awesome"}),
    awful.key({ modkey, "Control" }, "e", awesome.quit,
        {description = "quit awesome", group = "awesome"}),

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

        {description = "restore minimized", group = "client"})
)

clientkeys = gears.table.join(
    awful.key({ modkey,           }, "f",
        function (c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = "toggle fullscreen", group = "client"}),
    awful.key({ modkey}, "q",      function (c) c:kill()                         end,
        {description = "close", group = "client"}),
    awful.key({ modkey, "Control" }, "space",  function(c)
            awful.client.floating.toggle(c)
            c.ontop = c.floating
                                               end,
        {description = "toggle floating", group = "client"}),
    awful.key({ modkey}, "Return", function (c) c:swap(awful.client.getmaster()) end,
        {description = "move to master", group = "client"}),
    awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end,
        {description = "move to screen", group = "client"}),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
        {description = "toggle keep on top", group = "client"}),
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

globalkeys = gears.table.join(
    globalkeys,
    -- View tag only.
    awful.key(
        { modkey, "Control" }, "F10",
        function ()
            if zoom_tag then
                awful.tag.viewtoggle(zoom_tag)
            end
        end,
        {description = "toggle zoom tag", group = "tag"}
    ),
    awful.key(
        { modkey }, "F10",
        function ()
            if zoom_tag then
                zoom_tag:view_only()
            end
            awful.screen.focus(smallest_screen)
        end,
        {description = "view zoom tag", group = "tag"}
    ),
    awful.key(
        { modkey }, "F11",
        function ()
            if mail_tag then
                mail_tag:view_only()
            end
            awful.screen.focus(smallest_screen)
        end,
        {description = "view mail tag", group = "tag"}
    ),
    awful.key(
        { modkey, "Control" }, "F11",
        function ()
            if mail_tag then
                awful.tag.viewtoggle(mail_tag)
            end
        end,
        {description = "toggle mail tag", group = "tag"}
    ),
    awful.key(
        { modkey }, "F12",
        function ()
            if music_tag then
                music_tag:view_only()
            end
            awful.screen.focus(smallest_screen)
        end,
        {description = "view music tag", group = "tag"}
    )
)

for i = 1, 9 do
    globalkeys = gears.table.join(
        globalkeys,
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

clientbuttons = gears.table.join(
    awful.button({ }, 1, function (c)
            c:emit_signal("request::activate", "mouse_click", {raise = true})
    end),
    awful.button({ modkey }, 1, function (c)
            c:emit_signal("request::activate", "mouse_click", {raise = true})
            awful.mouse.client.move(c)
    end),
    awful.button({ modkey }, 3, function (c)
            c:emit_signal("request::activate", "mouse_click", {raise = true})
            awful.mouse.client.resize(c)
    end)
)

root.keys(globalkeys)

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
                     placement = awful.placement.no_overlap+awful.placement.no_offscreen
      }
    },
    { rule = { class = "Wrapper-2.0" },
      properties = { width = 500, height = 500} },
    { rule_any = { class = {
	    "Xfce4-panel",
	    "Wrapper-2.0",
	    "albert"
    }
    },
      properties = { border_width = 0, floating = true  } },

    -- Floating clients.
    { rule_any = {
          instance = {
              "DTA",  -- Firefox addon DownThemAll.
              "copyq",  -- Includes session name in class.
              "pinentry",
          },
          class = {
              "Arandr",
              "Blueman-manager",
              "Gpick",
              "Kruler",
              "MessageWin",  -- kalarm.
              "Sxiv",
              "Tor Browser", -- Needs a fixed window size to avoid fingerprinting by screen size.
              "Wpa_gui",
              "veromix",
              "xtightvncviewer"},

          -- Note that the name property shown in xprop might be set slightly after creation of the client
          -- and the name shown there might not match defined rules here.
          name = {
              "Event Tester",  -- xev.
          },
          role = {
              "AlarmWindow",  -- Thunderbird's calendar.
              "ConfigManager",  -- Thunderbird's about:config.
              "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
          }
    }, properties = { floating = true }},

    -- Add titlebars to normal clients and dialogs
    { rule_any = {type = { "normal", "dialog" }
                 }, properties = { titlebars_enabled = false }
    },

    { rule = { class = "Thunderbird" },
      properties = { tag = mail_tag } },

    { rule = { class = "zoom" },
      properties = { tag = zoom_tag } },

    { rule = { class = "Spotify" },
      properties = { tag = music_tag } },

    { rule = { class = "Mattermost" },
      properties = { tag = mail_tag } },

    { rule = { class = "Ferdi" },
      properties = { tag = mail_tag } },

}

-- Signal function to execute when a new client appears.
client.connect_signal(
    "manage",
    function (c)
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        if not awesome.startup then awful.client.setslave(c) end

        if awesome.startup
            and not c.size_hints.user_position
        and not c.size_hints.program_position then
            -- Prevent clients from being unreachable after screen count changes.
            awful.placement.no_offscreen(c)
        end
    end
)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal(
    "request::titlebars",
    function(c)
        -- buttons for the titlebar
        local buttons = gears.table.join(
            awful.button(
                { },
                1,
                function()
                    c:emit_signal("request::activate", "titlebar", {raise = true})
                    awful.mouse.client.move(c)
                end
            ),
            awful.button(
                { },
                3,
                function()
                    c:emit_signal("request::activate", "titlebar", {raise = true})
                    awful.mouse.client.resize(c)
                end
            )
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
    end
)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal(
    "mouse::enter",
                      function(c)
                          c:emit_signal("request::activate", "mouse_enter", {raise = false})
                                      end
)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)


-- Autorun programs
autorun = true
autorunApps =
    {
        "xset r rate 200 35",
        "setxkbmap -option caps:escape -option altwin:swap_lalt_lwin -layout 'us(altgr-intl)'",
    }
if autorun then
    for app = 1, #autorunApps do
        awful.util.spawn(autorunApps[app], false)
    end
end

client.connect_signal(
    "manage",
    function (c)
        c.shape = function(cr, w, h)
            gears.shape.rounded_rect(cr, w, h, 5)
        end
    end
)

-- move stuff off the mail and music tags when we restart the WM.
awesome.connect_signal(
    'exit',
    function(args)
        for i, unwanted_client in ipairs(zoom_tag:clients()) do
            unwanted_client:move_to_tag(largest_screen.tags[1])
        end
        for i, unwanted_client in ipairs(mail_tag:clients()) do
            unwanted_client:move_to_tag(largest_screen.tags[1])
        end
        for i, unwanted_client in ipairs(music_tag:clients()) do
            unwanted_client:move_to_tag(largest_screen.tags[1])
        end
    end
)
