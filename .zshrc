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
    alias vi='vim'
    alias ls='ls -F --color=tty'
    alias ll='ls -l'
    PROMPT='%n[%~]%# '
    RPROMPT='%D %T'
    bindkey -e
    # rbenv
    export PATH="$HOME/.rbenv/bin:$PATH"
    eval "$(rbenv init -)"
    # pyenv
    export PYENV_ROOT="$HOME/.pyenv"
    export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init -)"
    # for Debian
    export PATH=$PATH:/sbin
    ;;

# Cygwin
CYGWIN* | MSYS*)
    cd
    chcp.com 65001
    alias ls='ls --color=auto --show-control-chars'
    alias ll='ls -l'
    alias l='ls -CF'
    alias vi='vim'
    PROMPT='%n[%~]%# '
    RPROMPT='%D %T'
    bindkey -e

    export PATH="$/usr/local/bin:HOME/.rbenv/bin:$PATH"
    eval "$(rbenv init -)"

    #cygwin path settings about names with white spaces
    #PATH=$(echo ${PATH} | sed -e "s/\s/\\\\\ /g")
    #export PATH
    ;;

esac
