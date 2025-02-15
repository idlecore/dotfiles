# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/carter/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

setopt auto_cd

PROMPT='%F{yellow}%m:%~>%f'

alias "ls"="ls --color"
alias "ll"="ls --color -lah"
alias "x"="exit"

function chpwd() {
  emulate -L zsh
  ls
}

export TERM=xterm-256color
export PATH=$PATH:/home/carter/dotfiles/scripts
export EDITOR="vim"
export VISUAL="$EDITOR"
