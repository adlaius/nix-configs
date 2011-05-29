# Source global definitions
# N.B. **PLEASE** read them; they seem to vary substantially between systems...
if [ -f /etc/bashrc ]; then
	source /etc/bashrc
fi

complete -cf sudo
complete -cf man

# Don't save repeated commands in bash history
export HISTCONTROL="ignoredups"

# Disable ^S/^Q flow control (does anyone like/use this at all?)
# Doing this also allows us to use "C-(r|s)" a la Emacs
stty -ixon

# autocorrects cd misspellings, eg. from 'cd /sur/src/linus', shell
# would correctly guess 'cd /usr/src/linux'
shopt -s cdspell

shopt -s extglob # This is required for the extract() function

alias ls='ls --color=auto'
alias ll='ls -F1A --group-directories-first --color=no'
alias myredshift='redshift -g 1.15 -t 6500:5000 -l 30:-96'
alias e="emacs -nw"
alias idle='python2 -c "from idlelib import idle"'
alias idle3='python -c "from idlelib import idle"'
alias df="df -h"
alias du="du -c -h"
alias grep="grep --color=auto"
alias mkdir="mkdir -p -v"
alias hist="history | grep $1"
alias openports="netstat --all --numeric --programs --inet"
alias pg="ps -Af | grep $1"

# safety features
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -I' # 'rm -i' prompts for every file
alias ln='ln -i'
alias chown='chown --preserve-root'
alias chmod='chmod --preserve-root'
alias chgrp='chgrp --preserve-root'

# work around non-UTF8-aware software
export LANG=en_US.iso88591
export LC_COLLATE=C

export GREP_COLOR="1;31"
# options: off, on, gasp, lcd
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on'
export PATH=/home/brian/bin:$PATH
export EDITOR="emacs -nw"
export PAGER="less"
export VISUAL="emacs"
export http_proxy="127.0.0.1:8118"

# colorize display of less(1)
export LESS_TERMCAP_mb=$'\e[1;37m'
export LESS_TERMCAP_md=$'\e[1;37m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[1;47;30m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[0;36m'

tooLong () 
{
    pfad="`pwd`"
    if [[ ${#pfad} -lt 30 ]]; then
        echo -n "${pfad/\/home\/$USER/~}"
    else 
        echo -n "<...> /`basename "$pfad"`"
    fi
}

# orginal Arch Prompt
# PS1='[\u@\h \W]\$ '
PS1="\[\033[40;1;33m\]\$(tooLong)\$\[\033[0m\] "

if [[ $UID == 0 ]]; then
    PS1="\[\033[1;41m\]\$(tooLong)#\[\033[00m\] "
fi

# colorizing less messes up env(1)'s output, so we sanitize it:
function env() {
  exec /usr/bin/env "$@" | grep -v ^LESS_TERMCAP_
}

function extract() {
  local e=0 i c
  for i; do
    if [[ -r $i ]]; then
        c=''
        case $i in
          *.t@(gz|lz|xz|b@(2|z?(2))|a@(z|r?(.@(Z|bz?(2)|gz|lzma|xz)))))
                 c='bsdtar xvf' ;;
          *.7z)  c='7z x'       ;;
          *.Z)   c='uncompress' ;;
          *.bz2) c='bunzip2'    ;;
          *.exe) c='cabextract' ;;
          *.gz)  c='gunzip'     ;;
          *.rar) c='unrar x'    ;;
          *.xz)  c='unxz'       ;;
          *.zip) c='unzip'      ;;
          *)     echo "$0: cannot extract \`$i': Unrecognized file extension" >&2; e=1 ;;
        esac
        [[ $c ]] && command $c "$i"
    else
        echo "$0: cannot extract \`$i': File is unreadable" >&2; e=2
    fi
  done
  return $e
}

# apply a simple command to multiple args
function apply() {
    cmd=$1
    shift
    for x in $*; do $cmd $x; done
}

# mkgo - create a new directory and cd into it
# NOTE: calling this function "mkcd" causes the function to fail, as mkdir seems to be passed an empty string. 
function mkgo() {
    mkdir "$1"
    cd "$1"
}

# mkmv - creates a new directory and moves the file into it, in 1 step
# Usage: mkmv <file> <directory>
function mkmv() {
    mkdir "$2"
    mv "$1" "$2"
}

# sanitize - set file/directory owner and permissions to normal values (644/755)
# Usage: sanitize <file>
function sanitize() {
    chmod -R u=rwX,go=rX "$@"
    chown -R ${USER}.users "$@"
}

# nh - run command detached from terminal and without output
# Usage: nh <command>
function nh() {
    nohup "$@" &>/dev/null &
}
