#!/bin/bash
set -o vi # set vi keybindings in bash

# general stuff
alias ls="ls --color=yes"  # ls will use the dircolors definitions automatically
alias ll="ls -lh"
alias la="ls -lhA"
alias c=clear_term
alias vi=vim
alias grep='grep --color=auto'

# script aliases (needs the dotfiles/script and lynx directories in PATH)
alias ?='duck'
alias ??='google'

# git
alias gits="git status"  # gs -> is currently ghostscript

# Folders
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

# Python
alias pip="python3 -m pip"

# Typora (Markdown editor) flatpak
# flatpak install flathub io.typora.Typora
alias typora="flatpak run io.typora.Typora"
