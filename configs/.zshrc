# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# User prompt
autoload -U compinit promptinit colors
compinit
promptinit
colors
PROMPT="
%{$fg[red]%} >  %{$reset_color%}"
RPROMPT="%{$fg[black]%}%~%{$reset_color%}"

setopt completealiases

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors 'reply=( "=(#b)(*$VAR)(?)*=00=$color[green]=$color[bg-green]" )'
zstyle ':completion:*:*:*:*:hosts' list-colors '=*=30;41'
zstyle ':completion:*:*:*:*:users' list-colors '=*=$color[green]=$color[red]'
zstyle ':completion:*' menu select

bindkey -v

function open()
{ 
	xdg-open $1 &> /dev/null &disown
}

function music()
{
	sudo mount /dev/sda1/ ~/mount/
	sleep 1
	mpd
	sleep 0.5
	mpdscribble
	sleep 3
	ncmpcpp
}

# Pamcman Aliases
alias pacman='sudo pacman'
alias pacmansyu='pacman -Syu'

# Sudo alias
alias svim='sudo vim'

# Mounts
alias music='music'
alias linuxremotefs='sshfs wei001@linuxremote1.eg.bucknell.edu: ~/bucknell'
alias linuxremote='ssh wei001@linuxremote1.eg.bucknell.edu'
alias netspace="lftp -u wei001 ftp.netspace.bucknell.edu"
alias HUB="lftp -u wei001 134.82.224.17"
# HUB PW = SXPS16

# Programs
alias ghci='ghci +s'
alias installfont='sudo fc-cache -f -v'
alias muttb='mutt -F ~/.mutt/acct/wei001'
alias muttg='mutt -F ~/.mutt/acct/windelicato'
alias muttsuns='mutt -F ~/.mutt/acct/suns'
alias bool='espresso -o eqntott'
alias compton='compton -cCfF -I 0.065 -O 0.065 -D 6' 
alias compton='compton -cCGfF -o 0.38 -O 200 -I 200 -t 0.02 -l 0.02 -r 3.2 -D2 -m 0.88'
alias scrot="scrot -q100  -cd 5"
alias alsamixer="alsamixer -g"
alias equalizer="alsamixer -D equal"

# Scripts
alias temps='~/scripts/temps.sh'
alias colors='~/scripts/colors2.sh'
alias screenfetch='~/scripts/screenfetch-dev'
alias tumblrcat='~/scripts/tumblrcat'
alias jazzget='~/scripts/jazzget'
alias tag='~/scripts/tag'
alias installed='~/scripts/installed'

# Shortcuts
#alias rm='rm -i'
alias rmi='rm -i'
#alias mv='mv -i'
alias c='xsel -ib'
alias emac='emacs -nw'
alias h='history | tail'
alias hg='history | grep '
alias ch='chmod 755 '
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'
alias ']'='open'
alias ll='ls -alF'
alias la='ls -A'
alias lla='ls -lA'
alias l='ls -CF'
alias vi='vim'

# tar aliases
alias tarzip='unzip'
alias tarx='tar -xvf'
alias targz='tar -zxvf'
alias tarbz2='tar -jxvf'

#alias mkdir and cd
function mkdircd () { mkdir -p "$@" && eval cd "\"\$$#\""; }

# Bookmarks
alias cs208='cd ~/documents/cs208/'
alias awesomecfg='vim ~/.config/awesome/rc.lua'
alias awesometheme='vim ~/.config/awesome/themes/default/theme.lua'

# enable color support of ls and also add handy aliases
alias ls='ls --color=auto'
alias dir='dir --color=auto'
alias vdir='vdir --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

set -o noclobber
set -o vi


# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# MUTT BG fix
COLORFGBG="default;default"
