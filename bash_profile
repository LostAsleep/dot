#!/bin/sh
# This file is probably only needed for bash on MacOS

if test -r "$HOME/.bashrc"; then
    . "$HOME/.bashrc"
fi

# Old Test for bashrc
# if [ -f ~/.bashrc ]; then
#     source ~/.bashrc
# fi

##
# Your previous /Users/phil/.bash_profile file was backed up as /Users/phil/.bash_profile.macports-saved_2021-05-06_at_17:42:38
##

# MacPorts Installer addition on 2021-05-06_at_17:42:38: adding an appropriate PATH variable for use with MacPorts.
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
# Finished adapting your PATH environment variable for use with MacPorts.

# Setting PATH for Python 3.10
# The original version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.10/bin:${PATH}"
export PATH
