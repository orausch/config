#!/usr/bin/env bash

function run {
	if ! pgrep $1 ;
	then
		$@&
	fi
}

#run pa-applet
run redshift
run claws-mail
run battray
