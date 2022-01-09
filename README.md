# My DotFiles

Buyer beware.

## Note for .bashrc use and MacOS (Catalina)
After installing bash via homebrew, changing the default shell to the new bash and symlinking my dotfiles I ran into an error using `ls`:

`s: illegal option -- - usage: ls [-@ABCFGHLOPRSTUWabcdefghiklmnopqrstuwx1%] [file ...]`

To solve this problem I had to install `coreutils` via homebrew and add `gnubin` and `gnuman` to my `$PATH` to be able to use every command properly:

```sh
==> Caveats
All commands have been installed with the prefix 'g'.

If you really need to use these commands with their normal names, you can add a gnubin directory to your PATH from your bashrc like:

PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"

Additionally, you can access their man pages with normal names if you add the gnuman directory to your MANPATH from your bashrc as well:

MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"
```

## Note for Emacs dired on MacOS
To get dired to be able to list directory contents it needs to be able to use `ls`. So coreutils need to be installed and after that put the following into your `init.el`:

```elisp
(when (equal system-type 'darwin)
  (setq insert-directory-program "/usr/local/opt/coreutils/libexec/gnubin/ls"))
```
