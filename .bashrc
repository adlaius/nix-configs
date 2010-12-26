# get the Gentoo goodies (prompt, colors, other small UI tweaks; sanitizing)
if [ -f ~/src/git-repos/nix-configs/gentoo-bashrc.sh ]; then
    . ~/src/git-repos/nix-configs/gentoo-bashrc.sh
fi

# infinality freetype patches now use env. vars for configuration
if [ -f ~/src/git-repos/nix-configs/infinality-settings.sh ]; then
    . ~/src/git-repos/nix-configs/infinality-settings.sh
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

# checks window size after command execution -> prevents wrapping
# problems after window resize
shopt -s checkwinsize

shopt -s extglob # N.B. this is required for the extract() function

alias ls='ls --color=auto'
alias ll='ls -F1A --group-directories-first --color=no'
alias myredshift='redshift -g 1.15 -l 30.3:-96.2 -t 6650:4775 &'
alias idle='python2 -c "from idlelib import idle"'
alias idle3='python -c "from idlelib import idle"'
alias e='emacs -nw'
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
alias rm='rm -I'                    # 'rm -i' prompts for every file
alias ln='ln -i'
alias chown='chown --preserve-root'
alias chmod='chmod --preserve-root'
alias chgrp='chgrp --preserve-root'

export GREP_COLOR="1;31"
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=lcd'
export PATH=/home/brian/bin:$PATH
export EDITOR="emacs -nw"
export PAGER="less"
export VISUAL="emacs -nw"
export http_proxy="127.0.0.1:8118"

# colorize display of less(1)
export LESS_TERMCAP_mb=$'\e[1;37m'
export LESS_TERMCAP_md=$'\e[1;37m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[1;47;30m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[0;36m'

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

# arch linux package manager search
alias pacs="pacsearch"
pacsearch ()
{
       echo -e "$(pacman -Ss $@ | sed \
       -e 's#core/.*#\\033[1;31m&\\033[0;37m#g' \
       -e 's#extra/.*#\\033[0;32m&\\033[0;37m#g' \
       -e 's#community/.*#\\033[1;35m&\\033[0;37m#g' \
       -e 's#^.*/.* [0-9].*#\\033[0;36m&\\033[0;37m#g' )"
}

# apply a simple command to multiple args
apply ()
{
    cmd=$1
    shift
    for x in $*; do $cmd $x; done
}

# mkmv - creates a new directory and moves the file into it, in 1 step
# Usage: mkmv <file> <directory>
mkmv()
{
    mkdir "$2"
    mv "$1" "$2"
}

# sanitize - set file/directory owner and permissions to normal values (644/755)
# Usage: sanitize <file>
sanitize()
{
    chmod -R u=rwX,go=rX "$@"
    chown -R ${USER}.users "$@"
}

# nh - run command detached from terminal and without output
# Usage: nh <command>
nh()
{
    nohup "$@" &>/dev/null &
}

