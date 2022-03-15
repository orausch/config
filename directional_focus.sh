#!/bin/sh

PROG="
global.get_window_actors()
      .map(a=>a.meta_window)                                   
      .map(w=>({
        class: w.get_wm_class(),
	title: w.get_title(),
	pos: w.get_frame_rect().x,
	desc: w.get_description(),
      }))"

gdbus call \
	--session \
	--dest org.gnome.Shell \
	--object-path /org/gnome/Shell \
	--method org.gnome.Shell.Eval "$PROG" | sed -E -e "s/^\(\S+, '//" -e "s/'\)$//" | jq .
