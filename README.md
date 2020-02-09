# Linux config
Use stow to install the config files. For example

	stow awesome

## remove grub timeout
edit `/etc/default/grub` and replace `GRUB_TIMEOUT=0`

## make home and end work in terminal

	cp /etc/inputrc ~/.inputrc

(if not present try `.inputrc` in `other/`)

## lock automatic

	cp other/xscreensaver.service /etc/systemd/system/
	systemctl enable xscreensaver.service

## dmenu wifi
install https://github.com/firecat53/networkmanager-dmenu to `/usr/bin/`
install dmenu `.desktop` file

## vimium
https://chrome.google.com/webstore/detail/vimium/dbepggeogbaibhgnhhndojpepiihcmeb?hl=en

## backlight 
try `xbacklight`. If it doesn't work:
  
	git clone https://github.com/Ventto/lux
	sudo make install
	sudo usermod -aG video oliver
	sudo lux

Then relog.

## volume

	cp other/volume-script /usr/bin/

### other approach

	git clone https://github.com/fernandotcl/pa-applet && cd pa-applet
	./autogen.sh
	./configure --prefix=/usr/

then 
`rg Werror` and remove the `-Werror` tag

	sudo make install

## spotify in i3bar
Install https://github.com/acrisci/playerctl/releases

# firefox
- set to light theme (not default)
- change fonts to roboto and noto serif
- disable autohide by going to `about:config` and double clicking `browser.fullscreen.autohide`
## addons
- vim vixen
- ublock origin

# awesomewm
clone `lain` into `.config/awesome/`.
Tested version: `33c0e0c2360a04fcc6f51bccb0ad2a7a9e9c07b3`
