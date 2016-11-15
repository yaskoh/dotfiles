# .zshrc

case `uname -s` in

# Mac
Darwin*)
    alias ls='ls -FG'
    alias ll='ls -l'
    alias emacs='open -a /Applications/Emacs.app/Contents/MacOS/Emacs'
    bindkey -e
    
    # rbenv
    export PATH=$HOME/.rbenv/bin:$HOME/.pyenv/shims:$PATH
    eval "$(rbenv init -)"

    # MacPort
    export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
    ;;

# Linux
Linux*)
    alias ls='ls -F --color=tty'
    alias ll='ls -l'
    PROMPT='%n[%~]%# '
    RPROMPT='%D %T'
    bindkey -e
    export PATH="$HOME/.rbenv/bin:$PATH"
    eval "$(rbenv init -)"
    export PATH=$PATH:/sbin # for Debian
    ;;

# Cygwin
CYGWIN*)
    cd
    chcp.com 65001
    alias ls='ls --color=auto --show-control-chars'
    alias ll='ls -l'
    alias l='ls -CF'
    PROMPT='%n[%~]%# '
    RPROMPT='%D %T'
    bindkey -e

    export PATH="$HOME/.rbenv/bin:$PATH"
    eval "$(rbenv init -)"

    export PATH="/usr/local/bin:$PATH"
    ;;

esac
