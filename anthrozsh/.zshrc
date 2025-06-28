#zmodload zsh/zprof

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

#if command -v tmux &> /dev/null && [ -z "$TMUX" ] && [ "$LC_TERMINAL" = iTerm2 ]; then
#  tmux new-session -A -s main -c ~/code/anthropic
#fi
export ANTHROPIC_MODEL=claude-opus-4-20250514
export DISABLE_SENTRY=1
export I_KNOW_SENTRY_IS_DISABLED=1

alias g1=gcmh1
alias g2=gcmh2
alias g3=gcmh3
alias g4=gcmh4
alias g5=giad5
alias g5e="giad5 ke"
alias g5h="giad5 kh"
alias g2e="gcmh2 ke"
alias g2h="gcmh2 kh"
alias gap="git add -up"
alias gc="git checkout"
alias gaa="git add -u && git commit --amend"
alias gd="git diff"
alias cssh='coo ssh -t'
alias gpr='gh pr view --web'
alias gdump='git checkout HEAD -p'

# Function to push a commit to a personal branch
push_commit() {
  if [ $# -ne 1 ]; then
    echo "ERROR: push_commit requires exactly one argument (commit hash)"
    return 1
  fi
  git push origin "${1}:refs/heads/orausch/tmp-commit/$1" --no-verify
}

dbox() {
  case $1 in
    ''|*[!0-9]*) echo "ERROR: first arg must be a number"; return 1;;
    *);
  esac
  coo fullbox -n$1 or$1
}

gdiff() {
  git diff $1~1 $1
}

diffpr() {
  git diff $(gh pr diff $1 | grep index | cut -d' ' -f2)
}
alias ls="ls --color=auto"

autoload -U colors && colors

precmd() {
  CONTEXT="${AWS_PROFILE:-default}-$(/usr/bin/grep current-context ${KUBECONFIG:-~/.kube/config} | /usr/bin/sed 's/current-context: //g')"
  PS1="$CONTEXT %{$fg[cyan]%}%n@mbp %1~ %#%{$reset_color%} "
}
export PYTHONSTARTUP=/Users/oliver/.startup.py

# print all:
# for i in {0..255}; do  printf "\x1b[48;5;${i}mcolor%-5i\x1b[0m" $i ; if ! (( ($i + 1 ) % 8 )); then echo ; fi ; done
#use-li()  { unset AWS_PROFILE; unset KUBECONFIG ; kubectl config use-context lithium;  }
#use-bo-frontend() { export KUBECONFIG="${BO_FRONTEND}"; tmux select-pane -P 'bg=color224' }
#use-bo-research() { export KUBECONFIG="${BO_RESEARCH}";  tmux select-pane -P 'bg=color195' }

#use-bo

export SAND="$HOME/code/anthropic/sandbox/sandbox/oliver"
export PATH="$SAND/bin:$PATH"
export PATH="$HOME/code/anthropic/config/local/bin:$PATH"
export PATH="$HOME/code/bin:$PATH"


setopt extended_history       # record timestamp of command in HISTFILE
setopt hist_expire_dups_first # delete duplicates first when HISTFILE size exceeds HISTSIZE
setopt hist_ignore_dups       # ignore duplicated commands history list
setopt hist_ignore_space      # ignore commands that start with space
setopt hist_verify            # show command with history expansion to user before running it
# setopt share_history          # share command history data
setopt inc_append_history        # Write to the history file immediately, not when the shell exits.
HISTFILE="$HOME/.custom_history"
HISTSIZE=10000000
SAVEHIST=10000000

alias cdsand="cd $SAND"
source /opt/homebrew/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc

function open_jupyter() {
  export KUBECONFIG="/Users/oliver/personal/cloudlab-kubeconfig.yaml"
  # Assemble the link
  output=$(kubectl exec hs-0 -- jupyter notebook list --json)
  url=$(echo "${output}" | jq -r '"http://localhost:7777" + "/?token=" + .token')

  # Start port forwarding
  kubectl port-forward hs-0 7777:8888 &>/dev/null &
  port_forward_pid=$!

  # Open the link in the browser
	open $url

  # Cleanup the port forwarding after the user exits the script
  echo "Press any key to stop port forwarding and exit..."
  read -r -k1
  kill $port_forward_pid
}

alias reboot-home="ssh-keygen -R '[104.200.27.187]:30194' && ssh-keygen -R '[45.79.109.174]:30194' && ssh-keygen -R '[home-ssh.oraus.ch]:30194'"
alias kp='k -nproduction'
alias ke='k -nemu'
alias kh='k -nhawk'
alias kt='k -ntest'

function tokencount() {
  python -c 'import sys; from belt.encoding import get_memoized_tokenizer as t, J49_TOKENIZER_NAME; print(len((t(J49_TOKENIZER_NAME).tokenize_to_ints(open(sys.argv[1], "r").read()))))' $@
}
alias home-connect='mosh home -p 30281 -- tmux new-session -A -s main'
alias gl="gyro list -nproduction --hide-empty"
alias gf="gyro nodes -oc"
alias gp="kp get pods | grep Pending"

#alias use-v4() { use-tputest }
#alias use-v5() { use-tpuv5test }
export USE_ANTUP=0
source /Users/oliver/code/anthropic/config/local/zsh/zshrc
source $SAND/zshrc
alias ,=coo

function remote-dev() {
  if [ -z "$1" ]; then
    echo "USAGE remote-dev <sts-name>"
    return 1
  fi
  coo vscode --name "$1"
  scp ~/code/anthropic/.log_config.yaml "$1":/root/code/
  ssh -t "$1" 'tmux new-session -A -s main -c /root/code'
}

function remote-mosh() {
  if [ -z "$1" ]; then
    echo "USAGE remote-mosh <sts-name>"
    return 1
  fi
  ssh -t "$1" 'tmux new-session -A -s main -c /root/code'
}

function note() {
  bo coo jupyter ornote
}
alias model='cat ~/runs.txt | fzf'
alias lnote='code "$(cd /Users/oliver/code/local_nb && fzf)"'
alias pre_push='pre-commit run --hook-stage pre-push --from-ref HEAD~1 --to-ref HEAD'
export INFERENCE_TEAM_SAYS_I_CAN_DO_THIS=1
export JAX_PLATFORMS=cpu

alias pyl='python -c "import sys; lines = sys.stdin.read().splitlines(); result = eval(sys.argv[1]); print(\"\\n\".join(result))"'

# export NVM_DIR="$HOME/.nvm"
# [ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"  # This loads nvm
# [ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion
#
alias w=weave
alias bsync="cd ~/Documents/notes && /opt/homebrew/Caskroom/miniforge/base/envs/py311/bin/python bsync.py push"
alias repatch="python -m $SANDPY.patch_pyrightconfig  --ignore_extra claude garcon eval-suite bitbox trax xpipe spectra api rllm feedback interp"
alias repatch="python -m sandbox.oplots.patch_pyrightconfig  --ignore_extra claude garcon eval-suite bitbox trax xpipe spectra api  feedback interp --friends yang juesato saurav"
alias unpatch="python -m $SANDPY.patch_pyrightconfig  --unpatch"
alias o='srun or --'
alias or='srun or --reap --'
alias kpc='k -nprod-canary'
alias zed='/Applications/Zed\ Preview.app/Contents/MacOS/cli'


export WEAVE_REVIEWERS_FILE="/Users/oliver/.employee_github_names"
function checkead() {
  curl --user-agent "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/94.0.4606.81 Safari/537.36" -s 'https://egov.uscis.gov/csol-api/ui-auth' -H 'Content-Type: application/json' | jq ".JwtResponse.accessToken" | xargs -I{} curl -s 'https://egov.uscis.gov/csol-api/case-statuses/SRC2490057904' -H 'Authorization: Bearer {}' -H 'Content-Type: application/json' --user-agent "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/94.0.4606.81 Safari/537.36" | jq ".CaseStatusResponse.detailsEng"
}
export MYVARS="DEV_CACHE_WEIGHTS_LOCALLY=1 MYCRO_LOCAL_CACHE=1 DISABLE_PERPETUO=1 MYCRO_QUIET=1 TPU_PREMAPPED_BUFFER_SIZE=0 MYCRO_SKIP_WARMUP_GRAPHS=1 PYTEST_DISABLE_PLUGIN_AUTOLOAD=1 RUST_BACKTRACE=1"
export PT="${(z)MYVARS} pytest -p no:logging --despite-markers"

test -e /Users/oliver/.iterm2_shell_integration.zsh && source /Users/oliver/.iterm2_shell_integration.zsh || true
alias ipy="python ~/code/console/main.py"

# git config rebase.updateRefs false
alias hr="python -m sandbox.oliver.hotrun"
# export HOTRUN_WATCH_ROOT="~/code/oplots"
export HOTRUN_ROOT=/Users/oliver/code/oplots
alias gkd='gcmh1 k -ndove'
alias gk='gcmh1 k'
function le() {
  coo splunk_ui $1 --log-explorer

}
alias nkd='nan k -ndove'
alias nk='nan k'
alias kci='k -nci'
alias kd='k -ndove'
alias gettoken='bo k exec -it shared-notebox-0 -- jupyter server list'
export PATH="/Users/oliver/.local/bin:$PATH"
export SKIP=ant-rebase,manage-py-deps,reqs-txt-freshness
alias mm='mycroman'

alias startlab="PYTHONPATH='/Users/oliver/code/oplots' bo jupyter lab"
eval "$(atuin init zsh --disable-up-arrow)"
cd ~/code/anthropic
alias spanspw="oxy k get secret -ojson trace-spans-clickhouse-password | jq -r .data.TRACE_SPANS_CLICKHOUSE_PASSWORD | base64 -d | pbcopy"
function prdiff {
  git fetch && git diff $(gh pr view $1 --json baseRefName,headRefName | jq -r '"origin/\(.baseRefName)...origin/\(.headRefName)"')
}



export WT_ALIAS_NAME=jj_wt  # or any name you prefer
source $MONOREPO_ROOT_DIR/sandbox/sandbox/aborg/wt/shell_wrapper.sh
#zprof



function set_tpu_env() {
  if [ -z "$1" ]; then
    echo "USAGE: set_tpu_env <worker_hostname>"
    return 1
  fi
  echo "TPU_TOPOLOGY=2x2 TPU_WORKER_ID=0 TPU_SKIP_MDS_QUERY=true TPU_TOPOLOGY_WRAP=false,false,false TPU_CHIPS_PER_HOST_BOUNDS=2,2,1 TPU_ACCELERATOR_TYPE=v5litepod-4 TPU_RUNTIME_METRICS_PORTS=8431,8432,8433,8434 TPU_TOPOLOGY_ALT=false TPU_HOST_BOUNDS=1,1,1 ENABLE_TPUNETD_CLIENT=0 TPU_SOFT_SLICE=true TPU_WORKER_HOSTNAMES=$1-0.$1"
}
