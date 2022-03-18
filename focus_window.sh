#!/bin/sh

ID=`xdotool search --name "$1"`
if [[ -z "$ID" ]]; then
	exit 1
else
	xdotool windowactivate $ID
fi
