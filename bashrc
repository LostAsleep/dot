#!/bin/bash
# ~/.bashrc: executed by bash(1) for non-login shells. For examples
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

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


# enable color support of ls
if [ -x "/usr/bin/dircolors" ]; then
    if [ -r "$HOME/.dircolors" ]; then
		eval "$(dircolors -b ~/.dircolors)"
	else
		eval "$(dircolors -b)"
	fi
fi

alias ls="ls --color=yes" # ls will use the dircolors definitions automatically

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'


# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'


# ##########################################################
# Adding stuff to PATH (hopefully this will fix custom lynx)
# export PATH="$HOME/.config/lynx:$PATH"
# export PATH="$HOME/Repos/gitlab.com/Lost-Asleep/dotfiles/scripts:$PATH"
# export PATH="$HOME/Repos/gitlab.com/Lost-Asleep/dotfiles/lynx:$PATH"
# export PATH="$HOME/Repos/gitlab.com/lynx:$PATH"
# export PATH="$HOME/Repos/gitlab.com/dotfiles/scripts:$PATH"
# export PATH="$HOME/Repos/gitlab.com/Lost-Asleep/lynx:$PATH"

export PATH="$HOME/.config/lynx:$PATH"
export PATH="$HOME/Repos/github.com/LostAsleep/dot/scripts:$PATH"
export PATH="$HOME/Repos/github.com/LostAsleep/dot/lynx:$PATH"
export PATH="$HOME/Repos/github.com/scripts:$PATH"
export PATH="$HOME/Repos/github.com/LostAsleep/lynx:$PATH"


# export PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
# export PATH="/usr/local/bin:$PATH"
# export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"
# export PATH="/usr/local/lib/python3.8/site-packages:$PATH"   # Mac os pyflaces for emacs flymake
# export PATH="$(brew --prefix)/opt/grep/libexec/gnubin:$PATH" # for using gnu utils with normal names
export PATH="$HOME/bin:$PATH"  # For exercism.io
export PATH="$HOME/.local/bin:$PATH"  # For pytest install

export PATH="$PATH:/usr/local/go/bin"



# Python path for virtualenvwrapper
# export PYTHONPATH=${PYTHONPATH}:/usr/bin
# VIRTUALENVWRAPPER_PYTHON="$(command \which python3)"
# and now virtualenvwrapper itself
# source ~/.local/bin/virtualenvwrapper.sh


# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f "$HOME/.bash_aliases" ]; then
    "." "$HOME/.bash_aliases"
fi

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
