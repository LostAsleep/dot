#!/bin/bash

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# ----------------- environment variables ------------------------------

export GITUSER="LostAsleep"
export DOWNLOADS="$HOME/Downloads"
export DOTFILES="$HOME/Repos/github.com/$GITUSER/dot"
export TERM=xterm-256color

export EDITOR=vi
export VISUAL=vi
export EDITOR_PREFIX=vi

export PYTHONDONTWRITEBYTECODE=1

export GOPRIVATE="/github.com/$GITUSER/*"
export GOPATH="$HOME/.local/share/go"
export GOBIN="$HOME/.local/share/go/bin"
export GOPROXY="direct"

# ----------------- history --------------------------------------------

export HISTCONTROL=ignoreboth # no duplicate lines or leading spaces in history.
export HISTSIZE=10000
export HISTFILESIZE=20000

set -o vi # Use vi keybindings.
shopt -s histappend # Append to history file, don't overwrite it.

# ----------------- shell options --------------------------------------

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize
# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar

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

# ----------------- prompt ---------------------------------------------

__ps1() {
  # Remember local ist not POSIX compliant.
  local black='\[\e[30m\]';
  local red='\[\e[31m\]';
  local green='\[\e[32m\]';
  local yellow='\[\e[33m\]';
  local blue='\[\e[34m\]';
  local magenta='\[\e[35m\]';
  local cyan='\[\e[36m\]';
  local white='\[\e[37m\]';
  local reset='\[\e[0m\]';

  # Special Characters for the PS1 prompt variable
  # \h    the hostname up to the first .
  # \n    newline
  # \s    the name of the shell
  # \t    the current time in 24-hour format
  # \u    the username of the current user
  # \w    the current working directory
  # \W    the basename of the current working directory

  local branch=$(git branch --show-current 2>/dev/null)
  test -n "$branch" && branch="($branch)"

  PS1="\[${yellow}\]\u";  # username
  PS1+="\[${white}\]@";
  PS1+="\[${blue}\]\h";  # host
  PS1+="\[${white}\]:";
  PS1+="\[${magenta}\]\W";   # working directory
  PS1+="\[${red}\]$branch";
  PS1+="\[${white}\]\$ \[${reset}\]";  # '$' (and reset color)
}

PROMPT_COMMAND="__ps1"

# ----------------- dircolors ------------------------------------------

if command -v dircolors &>/dev/null; then
  if test -r "$HOME/.dircolors"; then
    eval "$(dircolors -b ~/.dircolors)"
  else
    eval "$(dircolors -b)"
  fi
fi

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
  "$HOME/.local/bin" \
  "$HOME/Repos/github.com/LostAsleep/dot/scripts"

pathappend \
  "$HOME/.config/lynx" \
  "$HOME/Repos/github.com/LostAsleep/dot/scripts" \
  "$HOME/Repos/github.com/LostAsleep/dot/lynx" \
  "/usr/local/go/bin" \
  "$HOME/.local/share/go/bin"

# ------------------------------ cdpath ------------------------------

export CDPATH=.:\
~/Repos/github.com:\
~/Repos/github.com/LostAsleep:\
~/Repos/github.com/LostAsleep/dot:\
~/Repos/github.com/LostAsleep/dot/scripts:\
~/Repos:\
~

# ----------------- aliases --------------------------------------------

unalias -a
# ls, clear, vi, grep, curl
alias ls="ls -h --color=yes" # Needed for dircolors to work.
alias ll='ls -l'
alias la='ls -lA'
alias c=clear_term
alias grep='grep -i --colour=auto' # For grep -i means case insensitive
alias egrep='egrep -i --colour=auto'
alias fgrep='fgrep -i --colour=auto'
alias curl='curl -L'
which vim &>/dev/null && alias vi=vim

# script aliases (needs dotfiles/script and lynx directories in PATH)
alias ?='duck'
alias ??='google'
alias ???='bing'

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
  if [[ -f "/usr/share/bash-completion/bash_completion" ]]; then
    "." "/usr/share/bash-completion/bash_completion"
  elif [[ -f "/etc/bash_completion" ]]; then
    "." "/etc/bash_completion"
  fi
fi
