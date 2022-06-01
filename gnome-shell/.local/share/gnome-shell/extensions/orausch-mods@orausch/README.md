# My Mods

## Development
Looking Glass is useful: start the gnome shell (Alt+F2) and type `lg`

After editing the extension, you need to reload using Alt+F2 and typing `r`.
## Installation
1. Use stow like normal
2. Restart gnome shell
3. Enable extension, either using `gnome-extensions enable orausch-mods@orausch` or the GUI

## Focus
Focusing windows takes the wmclass as an argument
```bash
gdbus call --session --dest org.gnome.Shell \EXI
  --object-path /org/gnome/Shell/Extensions/OrauschMods \
  --method org.gnome.Shell.Extensions.OrauschMods.Focus "'firefox'"
```
If successful, the status code is `0`. Otherwise, an error will be raised an status code will be `1`.
The included `focus_window.sh` script either starts
