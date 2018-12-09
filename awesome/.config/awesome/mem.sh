#!/bin/bash

colorize_title='1s/\(.*\)/<span foreground="#669900"><b>\1<\/b><\/span>/'
case "$1" in
	simple)
		free | grep Mem | awk '{printf("%.1f\n", $3/$2 * 100)}'
	;;
	summary)
		free -h | cut -c -45| sed -e "$colorize_title"
		echo ''
		#ps -eo pid:6,time:10,pmem:6,rss:8,comm --sort -rss | head -n 20 | sed -e "$colorize_title"
		ps -eo pid,pmem,vsz,rss,comm,time --sort -rss | numfmt --header --from-unit=1024 --to=iec --field 3-4  | sed "s/Web Content/WebContent/g"| column -t | head -n 20 | cut -c -60 | sed -e "$colorize_title" 

	;;
esac
