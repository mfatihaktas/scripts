test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

source /usr/local/opt/spaceship/spaceship.zsh

eval "$(direnv hook zsh)"

alias ls="ls -lGH"
alias e="TERM=xterm-16color emacs"
alias k="kubectl"
alias m="minikube"
alias t="terraform"

gr() { grep --color=always -irn -I "$1" * ;}
grf() { grep --color=always -irn -I "$1" --include="$2" * ;}
ff() { find . -name "*$1*" ;}
eff() { e `ff $@` ;}
