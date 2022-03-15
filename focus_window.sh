#!/bin/sh

PROG="
var windows = global.get_window_actors()
	.map(a=>a.meta_window)
	.filter(w=>w.get_wm_class() === '$1');

if(windows.length === 0) {
	false;
} else {
	windows.map(w=> {w.focus(0); w.raise()});
	true;
}
"

status=`gdbus call \
	--session \
	--dest org.gnome.Shell \
	--object-path /org/gnome/Shell \
	--method org.gnome.Shell.Eval "$PROG"`
echo $status

if [ "$status" != "(true, 'true')" ]
then
	exit 1
fi
