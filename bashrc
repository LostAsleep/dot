#!/bin/bash
# ~/.bashrc: executed by bash(1) for non-login shells. For examples
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# ----------------- history --------------------------------------------

# don't put duplicate lines or lines starting with space in the history.
HISTCONTROL=ignoreboth
shopt -s histappend # append to the history file, don't overwrite it
HISTSIZE=10000
HISTFILESIZE=20000

# ----------------- shell options --------------------------------------

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s "checkwinsize"

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x "/usr/bin/lesspipe" ] && eval "$(SHELL=/bin/sh lesspipe)"

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x "/usr/bin/tput" ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi


blue="$(tput setaf 26)";
grey="$(tput setaf 253)";
orange="$(tput setaf 166)";
yellow="$(tput setaf 228)";
green="$(tput setaf 71)";
white="$(tput setaf 15)";
bold="$(tput bold)";
reset="$(tput sgr0)";

# Special Characters for the PS1 prompt variable
#
# \h    the hostname up to the first .
# \n    newline
# \s    the name of the shell
# \t    the current time in 24-hour format
# \u    the username of the current user
# \w    the current working directory
# \W    the basename of the current working directory

if [ "$color_prompt" = yes ]; then
    PS1="\[${blue}\]\u";  # username
    PS1+="\[${grey}\]@";
    PS1+="\[${orange}\]\h";  # host
    PS1+="\[${grey}\]:";
    PS1+="\[${green}\]\W";   # working directory
    PS1+="\[${grey}\]\$ \[${reset}\]";  # '$' (and reset color)

    export PS1

else
PS1="\u@\h:\w\$ "
fi
unset color_prompt force_color_prompt

# ----------------- dircolors ------------------------------------------

# enable color support of ls
# if [ -x "/usr/bin/dircolors" ]; then
#     if [ -r "$HOME/.dircolors" ]; then
# 		eval "$(dircolors -b ~/.dircolors)"
# 	else
# 		eval "$(dircolors -b)"
# 	fi
# fi

if command -v dircolors &>/dev/null; then
    if test -r "$HOME/.dircolors"; then
        eval "$(dircolors -b ~/.dircolors)"
    else
        eval "$(dircolors -b)"
    fi
fi

# ls will use the dircolors definitions automatically
# -h will output human readable sizes and such
alias ls="ls -h --color=yes"

# colored GCC warnings and errors
# export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# ----------------- path -----------------------------------------------

export PATH="$HOME/.config/lynx:$PATH"
export PATH="$HOME/Repos/github.com/LostAsleep/dot/scripts:$PATH"
export PATH="$HOME/Repos/github.com/LostAsleep/dot/lynx:$PATH"
export PATH="$HOME/Repos/github.com/scripts:$PATH"
export PATH="$HOME/Repos/github.com/LostAsleep/lynx:$PATH"

# ----------------- vim keybindings ------------------------------------

set -o vi # default ist emacs style keybindings

# ----------------- aliases --------------------------------------------

# ls, clear, vi and grep
alias ll='ls -l'
alias la='ls -lA'
alias c=clear_term
alias vi=vim
alias grep='grep --color=auto'

# script aliases (needs dotfiles/script and lynx directories in PATH)
alias ?='duck'
alias ??='google'

# git
alias gits='git status'  # gs -> is currently ghostscript

# Folders
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

# Python
alias pip='python3 -m pip'

# Typora (Markdown editor) flatpak
# flatpak install flathub io.typora.Typora
alias typora='flatpak run io.typora.Typora'

# ----------------- aliases --------------------------------------------

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq "posix"; then
  if [ -f "/usr/share/bash-completion/bash_completion" ]; then
    "." "/usr/share/bash-completion/bash_completion"
  elif [ -f "/etc/bash_completion" ]; then
    "." "/etc/bash_completion"
  fi
fi
