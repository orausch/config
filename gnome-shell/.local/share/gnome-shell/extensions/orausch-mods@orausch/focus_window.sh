#!/bin/sh

STATUS=`gdbus call --session --dest org.gnome.Shell \
  --object-path /org/gnome/Shell/Extensions/OrauschMods \
  --method org.gnome.Shell.Extensions.OrauschMods.Focus "'$1'"`
EXIT_CODE=$?
echo $EXIT_CODE
if [ $EXIT_CODE -ne 0 ]; then
  echo "couldn't find application"
fi
exit $EXIT_CODE
