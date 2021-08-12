#!/bin/bash

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# ----------------- history --------------------------------------------

export HISTCONTROL=ignoreboth # no duplicate lines or leading spaces in history.
export HISTSIZE=10000
export HISTFILESIZE=20000

set -o vi # Use vi keybindings.
shopt -s histappend # Append to history file, don't overwrite it.

# ----------------- shell options --------------------------------------

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s "checkwinsize"

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# ----------------- pager ----------------------------------------------

# make less more friendly for non-text input files, see lesspipe(1)
# [ -x "/usr/bin/lesspipe" ] && eval "$(SHELL=/bin/sh lesspipe)"

if test -x "/usr/bin/lesspipe"; then
    export LESSOPEN="| /usr/bin/lesspipe %s";
    export LESSCLOSE="/urs/bin/lesspipe %s %s";
fi

# This is for colored man pages
export LESS_TERMCAP_mb="[35m" # magenta
export LESS_TERMCAP_md="[35m" # yellow
export LESS_TERMCAP_me=""
export LESS_TERMCAP_se=""
export LESS_TERMCAP_so="[34m" # blue
export LESS_TERMCAP_us="[4m"  # underline

# ----------------- fancy prompt stuff ---------------------------------

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

if command -v dircolors &>/dev/null; then
  if test -r "$HOME/.dircolors"; then
    eval "$(dircolors -b ~/.dircolors)"
  else
    eval "$(dircolors -b)"
  fi
fi

# ls will use the dircolors definitions automatically
# -h will output human readable sizes and such

# colored GCC warnings and errors
# export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# ----------------- functions ------------------------------------------

cdtemp() {
  newdirname="$1"
  mkdir -p "/tmp/${newdirname}" # -p is not POSIX
  cd "/tmp/${newdirname}"
} && export -f cdtemp


newzet() {
  gmt_timestamp="$(date -u +%Y%m%d%H%M%S)"
  mkdir -p "${gmt_timestamp}" # -p is not POSIX
  cd "${gmt_timestamp}"
  touch "README.md" && vi "README.md"
} && export -f newzet


clear_term() {
  # This function will clear the terminal with ANSI escapes alone.
  # As no binary for this is required it should be quite fast.
  printf "\e[1;1H\e[2J"
} && export -f clear_term


# ----------------- path -----------------------------------------------

pathappend() { # Appends the given path to PATH
  declare arg  # BASH only, not POSIX compliant
  for arg in "$@"; do
    test -d "${arg}" || continue
    PATH=${PATH//:${arg}:/:} # delete arg from PATH if its in the middle
    PATH=${PATH/#${arg}:/}   # delete arg from PATH if its the first thing
    PATH=${PATH/%:${arg}/}   # delete arg from PATH if its the last thing
    # Then export to PATH with new position. 
    # The {PATH:+ is for the rare case that no PATH exists (create new one)
    export PATH="${PATH:+"${PATH}:"}${arg}"
  done
}


pathprepend() { # Puts the given path at the top of PATH
  declare ARG   # BASH only, not POSIX compliant
  for ARG in "$@"; do
    test -d "${ARG}" || continue
    PATH=${PATH//:${ARG}:/:} # delete ARG from PATH if its in the middle
    PATH=${PATH/#${ARG}:/}   # delete ARG from PATH if its the first thing
    PATH=${PATH/%:${ARG}/}   # delete ARG from PATH if its the last thing
    # Then export to PATH with new position. 
    # The {PATH:+ is for the rare case that no PATH exists (create new one)
    export PATH="${ARG}${PATH:+":${PATH}"}"
  done
}

# Remember last arg will be first in path
pathprepend \
  "$HOME/Repos/github.com/LostAsleep/dot/scripts"

pathappend \
  "$HOME/.config/lynx" \
  "$HOME/Repos/github.com/LostAsleep/dot/scripts" \
  "$HOME/Repos/github.com/LostAsleep/dot/lynx"

# ----------------- aliases --------------------------------------------

unalias -a
# ls, clear, vi, grep, curl
alias ls="ls -h --color=yes" # Needed for dircolors to work.
alias ll='ls -l'
alias la='ls -lA'
alias c=clear_term
alias vi=vim
alias grep='grep -i --colour=auto' # For grep -i means case insensitive
alias egrep='egrep -i --colour=auto'
alias fgrep='fgrep -i --colour=auto'
alias curl='curl -L'

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

# Laziness
alias chmox='chmod +x'

# Python
alias pip='python3 -m pip'

# Typora (Markdown editor) flatpak
# flatpak install flathub io.typora.Typora
alias typora='flatpak run io.typora.Typora'

# ----------------- programmable completion features ? -----------------

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
