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
export SNIPPETS="$HOME/Repos/github.com/$GITUSER/dot/snippets"
export TERM=xterm-256color

export EDITOR=vim
export VISUAL=vim
export EDITOR_PREFIX=vim

export PYTHONDONTWRITEBYTECODE=1

export GOPATH="$HOME/.local/share/go"
export GOBIN="$HOME/.local/share/go/bin"
# Note being able to use private repos by default with Go is really
# annoying. This is the standard way to overcome that:
export GOPRIVATE="/github.com/$GITUSER/*"
export GOPROXY="direct"
export GO111MODULE=on # prevent: cannot use path@version syntax in GOPATH mode

# ----------------- macbook settings -----------------------------------

if [[ "$OSTYPE" == "darwin"* ]]; then
  # for ls --color to work:
  export PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
  export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"
fi

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
  local green="\[\033[38;5;2m\]"
  local blue="\[\033[38;5;12m\]"
  local red="\[\033[38;5;1m\]"
  local reset="\[\033[0m\]"

  local bold
  bold="$(tput bold)"

  local venv=""
  if [[ -n "$VIRTUAL_ENV" ]]; then
    venv="(${VIRTUAL_ENV##*/}) "
  fi


  local git_branch
  git_branch="$(git branch 2> /dev/null)"  # Redirect errors to bit bucket
  git_branch="(${git_branch:2:${#git_branch}})"  # Remove leading "* " if any, add parentheses

  if [[ "${git_branch}" == "()" ]]; then  # If only empty parentheses change to empty string
    git_branch=""
  fi


  PS1="${reset}\n";      # Empty line above prompt
  PS1+="${venv}";               # if Python3 venv, get basepath of venv dir
  PS1+="${green}\u@\h:";        # username@host:
  PS1+="${blue}\w";             # working directory
  PS1+="${red}${git_branch}";   # git branch (if any)
  PS1+="${reset}\$ ";           # '$' (and reset color)
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
  touch "README.md" && vim "README.md"
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
  "/usr/local/bin" \
  "$HOME/Repos/github.com/LostAsleep/dot/scripts" \
  "$HOME/.config/lynx"

pathappend \
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
alias ls='ls -h --color' # Needed for dircolors to work.
alias ll='ls -l'
alias la='ls -A'
# alias c=clear_term
alias c='clear'
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

# shortcuts
alias snippets='cd $SNIPPETS'

# Folders
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

# Misc
alias chmox='chmod u+x'
alias ip='ip -c'

# Python
alias pip='python3 -m pip'

# emacsclient
alias ec='emacsclient --create-frame --alternate-editor=""'

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
