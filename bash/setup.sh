function activate_venv {
	if [ -d venv ]; then
		source venv/bin/activate
	elif [ -f pyproject.toml ]; then
		source $(poetry env info --path)/bin/activate
	elif [ -d node_modules ]; then 
		export PATH="$(pwd)/node_modules/.bin:$PATH"
	else
		echo "Couldn't find anything to activate"
	fi
}
alias act="activate_venv"

if command -v tmux &> /dev/null && [ -n "$PS1" ] && [[ "$TERM" = alacritty ]] && [ -z "$TMUX" ]; then
  exec tmux new -As0
fi

export HISTSIZE=10000
export HISTFILESIZE=20000

