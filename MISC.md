# make home and end work in terminal

	cp /etc/inputrc ~/.inputrc

(if not present try `.inputrc` in `other/`)

# lock automatic

	cp other/xscreensaver.service /etc/systemd/system/
	systemctl enable xscreensaver.service

# backlight 
try `xbacklight`. If it doesn't work:
  
	git clone https://github.com/Ventto/lux
	sudo make install
	sudo usermod -aG video oliver
	sudo lux

Then relog.

# spotify stuff
Install https://github.com/acrisci/playerctl/releases

# firefox
- set to light theme (not default)
- change fonts to roboto and noto serif
- disable autohide by going to `about:config` and double clicking `browser.fullscreen.autohide`

# awesomewm
clone `lain` into `.config/awesome/`.
Tested version: `33c0e0c2360a04fcc6f51bccb0ad2a7a9e9c07b3`
