test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

source /usr/local/opt/spaceship/spaceship.zsh

eval "$(direnv hook zsh)"

export OPENAI_API_KEY="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"

alias ls="ls -lGH"
alias e="TERM=xterm-16color emacs"
alias k="kubectl"
alias m="minikube"
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
    black --exclude=".direnv/*" .
    flake8 --exclude=".direnv/*" .
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
