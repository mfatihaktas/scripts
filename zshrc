test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# Ref: https://github.com/spaceship-prompt/spaceship-prompt#-installation
source "/opt/homebrew/opt/spaceship/spaceship.zsh"

eval "$(direnv hook zsh)"

export OPENAI_API_KEY="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"

alias ls="ls -lGH"
alias e="TERM=xterm-16color emacs"
alias k="kubectl"
alias m="minikube"
alias p="pulumi"
alias t="terraform"

gr() { grep --color=always -irn -I "$1" * ;}
grf() { grep --color=always -irn -I "$1" --include="$2" * ;}
ff() { find . -name "*$1*" ;}
eff() { e `ff $@` ;}

pre-commit() { pre-commit run --all-files ;}

lint-code()
{
  (
    isort --skip .direnv/** .
    black --line-length 88 --exclude=".direnv/*" .
    flake8 --max-line-length 88 --exclude=".direnv/*" .
  )
}

# Codespaces
alias clogin="gh auth login"
alias cssh="gh codespace ssh"

# `kubectl`
# Ref: https://kubernetes.io/docs/reference/kubectl/cheatsheet/
alias k-get-pods="kubectl get pods --all-namespaces"

# Ref: https://github.com/orgs/community/discussions/25497#discussioncomment-3248109
# gh codespace ports forward 22:2222 -c mfatihaktas-upgraded-guide-64r6r95gj7cxvpj
cforward() { gh codespace ports forward 22:2222 -c "$1" ;}
alias cssh-forward="ssh -p 2222 -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null vscode@localhost"
alias csshfs="sshfs -o sshfs_debug -o ssh_command='ssh -p 2222 -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null vscode@localhost'"
# alias csshfs="sshfs -o sshfs_debug -o ssh_command='ssh -p 2222 -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null vscode@localhost' vscode@localhost:/workspaces/fw-prototype ~/Desktop/mount"
# sshfs -o ssh_command='ssh -p 2222 -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null vscode@localhost' vscode@localhost:/workspaces/fw-prototype ~/Desktop/mount
# sshfs -o sshfs_debug -p 2222 vscode@localhost:/workspaces/fw-prototype ~/Desktop/mount

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/mehmet/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/mehmet/miniconda3/etc/profile.d/conda.sh" ]; then
	. "/Users/mehmet/miniconda3/etc/profile.d/conda.sh"
    else
	# export PATH="/Users/mehmet/miniconda3/bin:$PATH"
	export PATH="$PATH:/Users/mehmet/miniconda3/bin"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

alias pyflink="pyflink-shell.sh local"

source $(brew --prefix)/share/zsh-autosuggestions/zsh-autosuggestions.zsh
# Ref: https://github.com/zsh-users/zsh-autosuggestions/issues/646#issuecomment-1011417964
bindkey "^[[A" history-beginning-search-backward
bindkey "^[[B" history-beginning-search-forward


source $(direnv hook zsh)
direnv-toggle() {
  if [ -z "${DIRENV_DISABLE:-}" ]; then
    unset -f _direnv_hook
    export DIRENV_DISABLE=1
  else
    unset DIRENV_DISABLE
    source $(direnv_hook_zsh)
  fi
}

export XDG_CONFIG_HOME="$HOME/.config/"

export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"

PATH="/Applications/CMake.app/Contents/bin":"$PATH"
