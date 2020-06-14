###
### .zshrc
###

## Common ##

## autoload
# chpwd
autoload -Uz chpwd
chpwd() { if [ $(ls | wc -l ) -lt 20 ]; then ls -CF; fi }
# completion
autoload -Uz compinit && compinit

## setopt
setopt auto_cd
setopt auto_pushd
setopt correct
setopt extended_glob
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt hist_reduce_blanks
setopt interactive_comments
setopt pushd_ignore_dups
setopt print_eight_bit

# keybind
bindkey -e

# prompt
PROMPT="%F{green}%n@%m%f %F{magenta}zsh%f %F{yellow}%~%f
%# "
RPROMPT='%D %T'

# alias
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'
alias vi='vim'
alias mkdir='mkdir -p'

# alias (local)
alias ga='gitall'
alias gaa='gitall addlist'

# path
export PATH=${HOME}/bin:$PATH


## For each OS ##
case `uname -s` in

# Mac
Darwin*)
    # tmux
    [[ -z "${TMUX}" && ! -z "PS1" ]] && tmux
    # alias
    alias ls='ls -FG'
    alias ll='ls -l'
    alias la='ls -a'
    alias l='ls -C'
    alias emacs='open -a /Applications/Emacs.app/Contents/MacOS/Emacs'
    # rbenv
    # export PATH=${HOME}/.rbenv/bin:${PATH}
    # eval "$(rbenv init -)"
    # MacPort
    export PATH="/opt/local/bin:/opt/local/sbin:${PATH}"
    ;;

# Linux
Linux*)
    #keymap
    setxkbmap dvorak
    # alias
    alias ls='ls -F --color=tty'
    alias ll='ls -l'
    alias la='ls -a'
    alias l='ls -C'
    # rbenv
    export PATH="${HOME}/.rbenv/bin:${PATH}"
    eval "$(rbenv init -)"
    # for Debian
    export PATH=${PATH}:/sbin
    ;;

# Cygwin
CYGWIN* | MSYS*)
    cd
    [[ -z "${TMUX}" && ! -z "PS1" ]] && tmux

    # alias
    alias ls='ls -F --color=auto --show-control-chars'
    alias ll='ls -l'
    alias la='ls -a'
    alias l='ls -C'
    alias -g G='| grep'
    alias -g L='| less'

    # rbenv
    export PATH="/usr/local/bin:${HOME}/.rbenv/bin:${PATH}"
    eval "$(rbenv init -)"
    ;;

esac
