# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# User prompt
GREEN="\[\033[0;32m\]"
BLUE="\[\033[0;36m\]"
WHITE="\[\033[0;37m\]"
BLACK="\[\033[1;30m\]"
RED="\[\033[31m\]"
CYAN="\[\033[1;36m\]"
#PS1="\n$WHITE┌──[$BLUE\u$WHITE@$BLUE\h$WHITE]$RED ⠶ $WHITE[$GREEN\w$WHITE]\n└──$RED╼ \[\e[0m\] "
#PS1="\n$BLACK┌──((( $WHITE\u$RED ⠶ $GREEN\w $BLACK)))\n$BLACK└$GREEN> \[\e[0m\] "
PS1="\n $RED> \[\e[0m\] "
 
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

extract() {
	if [[ -f $1 ]]; then
	case $1 in
		*.7z) 7z x $1;;
		*.bz2) bunzip2 $1;;
		*.gz) gunzip $1;;
		*.rar) unrar x $1;;
		*.tar) tar xvf $1;;
		*.tar.bz2) tar xvjf $1;;
		*.tar.gz) tar xvzf $1;;
		*.tbz2) tar xvjf $1;;
		*.tgz) tar xvzf $1;;
		*.zip) unzip $1;;
		*.Z) uncompress $1;;
		*) echo "unable to extract '$1'..." ;;
	esac
	else
		echo "'$1' is not a valid file!"
  	fi
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
alias HUB="lftp -u wei001 134.82.225.193"
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
alias matlab="/home/sunn/Bin/matlab/bin/matlab -nodesktop -nosplash"

# Scripts
alias temps='~/bin/scripts/temps.sh'
alias colors='~/bin/scripts/colors.sh'
alias colors='~/bin/scripts/colors2.sh'
alias invaders='~/bin/scripts/invaders.sh'
alias screenfetch='~/bin/scripts/screenfetch-dev'
alias tumblrcat='~/bin/scripts/tumblrcat'
alias jazzget='~/bin/scripts/jazzget'
alias tag='~/bin/scripts/tag'
alias installed='~/bin/scripts/installed'

# Shortcuts
#alias rm='rm -i'
alias rmi='rm -i'
#alias mv='mv -i'
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
alias log='sudo cat /var/log/everything.log | less'

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

#sets auto complete in terminal to tab key
complete -cf sudo
bind '"\t":menu-complete'

# enable bash completion in interactive shells
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi


# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# MUTT BG fix
COLORFGBG="default;default"
export PERL_LOCAL_LIB_ROOT="/home/sunn/perl5";
export PERL_MB_OPT="--install_base /home/sunn/perl5";
export PERL_MM_OPT="INSTALL_BASE=/home/sunn/perl5";
export PERL5LIB="/home/sunn/perl5/lib/perl5/x86_64-linux-gnu-thread-multi:/home/sunn/perl5/lib/perl5";
export PATH="/home/sunn/perl5/bin:$PATH";
