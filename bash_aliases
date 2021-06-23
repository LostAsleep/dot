#!/bin/bash
set -o vi # set vi keybindings in bash

# general stuff
unalias -a
alias ls="ls --color=yes"  # ls will use the dircolors definitions automatically
alias ll="ls -lh"
alias la="ls -lhA"
alias c=clear_term
alias vi=vim
alias grep='grep --color=auto'

# This starts an emacsclient in a new window and connects to a running
# emacs-server instance. If no emacs server daemon is running it will start it
# thats what -a and the empty string is for.
alias ec="emacsclient -c --alternate-editor="""

# script aliases (needs the dotfiles/script and lynx directories in PATH)
alias ?='duck'
alias ??='google'

# git
alias gits="git status"  # gs -> is currently ghostscript

# FOLDERS
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

# Python
alias pip="python3 -m pip"

# Typoral (Markdown editor) flatpak
# flatpak install flathub io.typora.Typora
alias typora="flatpak run io.typora.Typora"
