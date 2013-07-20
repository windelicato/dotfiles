source /usr/share/zsh/plugins/zsh-syntax-highlight/zsh-syntax-highlighting.zsh
autoload -U compinit promptinit colors
compinit
promptinit
colors

PROMPT="
%{$fg[red]%} »  %{$reset_color%}"
RPROMPT="%B%{$fg[cyan]%}%~%{$reset_color%}"

[[ -t 1 ]] || return
case $TERM in
	*xterm*|*rxvt*|(dt|k|E|a)term)
		preexec () {
			print -Pn "\e]2;$1\a"    # edited; %n@%m omitted, as I know who and where I am
		}
		;;
esac

setopt AUTO_CD
setopt CORRECT
setopt completealiases
setopt append_history
setopt share_history
setopt hist_verify
setopt hist_ignore_all_dups
export HISTFILE="${HOME}"/.zsh-history
export HISTSIZE=1000000
export SAVEHIST=$HISTSIZE


zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors 'reply=( "=(#b)(*$VAR)(?)*=00=$color[green]=$color[bg-green]" )'
zstyle ':completion:*:*:*:*:hosts' list-colors '=*=30;41'
zstyle ':completion:*:*:*:*:users' list-colors '=*=$color[green]=$color[red]'
zstyle ':completion:*' menu select

bindkey -v
bindkey '^R' history-incremental-search-backward
bindkey "^j" history-beginning-search-backward
bindkey "^k" history-beginning-search-forward



function open() { xdg-open $1 &> /dev/null &disown; }
function lt() { ls -ltrsa "$@" | tail; }
function psgrep() { ps axuf | grep -v grep | grep "$@" -i --color=auto; }
function fname() { find . -iname "*$@*"; }

conf() {
	case $1 in
		xmonad)		vim ~/.xmonad/xmonad.hs ;;
		conky)		vim ~/.xmonad/.conky_dzen ;;
		homepage)	olddir=$(pwd) && cd ~/scripts/homepage.py && vim homepage.py && ./homepage.py; cd $olddir ;;
		menu)		vim ~/scripts/menu ;;
		mpd)		vim ~/.mpdconf ;;
		mutt)		vim ~/.mutt/acct/wei001 ;;
		ncmpcpp)	vim ~/.ncmpcpp/config ;;
		pacman)		svim /etc/pacman.conf ;;
		ranger)		vim ~/.config/ranger/rc.conf ;;
		rifle)		vim ~/.config/ranger/rifle.conf ;;
		tmux)		vim ~/.tmux.conf ;;
		vim)		vim ~/.vimrc ;;
		xinit)		vim ~/.xinitrc ;;
		xresources)	vim ~/.Xresources && xrdb ~/.Xresources ;;
		zathura)	vim ~/.config/zathura/zathurarc ;;
		theme2)		vim ~/.themes/FlatStudioCustom/gtk-2.0/gtkrc ;;
		theme3)		vim ~/.themes/FlatStudioCustom/gtk-3.0/gtk.css ;;
		gtk2)		vim ~/.gtkrc-2.0 ;;
		gtk3)		vim ~/.config/gtk-3.0/settings.ini ;;
			tint2)		vim ~/.config/tint2/xmonad.tint2rc ;;
		zsh)		vim ~/.zshrc && source ~/.zshrc ;;
		*)			echo "Unknown application: $1" ;;
	esac
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
# alias pacman='pacman'
# Sudo alias 
alias svim='sudo vim' 
alias linuxremotefs='sshfs wei001@linuxremote1.eg.bucknell.edu:/nfs/unixspace ~/bucknell'
alias linuxremote='ssh -Y wei001@linuxremote1.eg.bucknell.edu'
alias netspace="lftp -u wei001 ftp.netspace.bucknell.edu"
alias HUB='sudo mount -t cifs //bucknellhub.com/HUB /mnt/HUB -o user=hubguest'
alias HUBDropbox='sudo mount -t cifs //bucknellhub.com/Dropbox /mnt/HUBDropbox -o user=hubguest'

# Programs
alias installfont='sudo fc-cache -f -v'
alias muttb='mutt -F ~/.mutt/acct/wei001'
alias muttg='mutt -F ~/.mutt/acct/windelicato'
alias muttsuns='mutt -F ~/.mutt/acct/suns'
alias bool='espresso -o eqntott'
#alias compton='compton -cCfF -I 0.065 -O 0.065 -D 6' 
#alias compton='compton -cCGfF -o 0.38 -O 200 -I 200 -t 0.02 -l 0.02 -r 3.2 -D2 -m 0.88'
#alias scrot="scrot -q100  -d 5"
alias alsamixer="alsamixer -g"
alias equalizer="alsamixer -D equal"
alias mysqlt="mysql -u root -p cargo_science"
alias redwm='cd ~/dwm; makepkg -g >> PKGBUILD; makepkg -efi --noconfirm; killall dwm; /home/sunn/scripts/dwm-status;'

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
function cdl () { cd "$@" && ls; }

# Bookmarks

# enable color support of ls and also add handy aliases
alias ls='ls --color=auto'
alias dir='dir --color=auto'
alias vdir='vdir --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

set -o noclobber
set -o vi


# MUTT BG fix
COLORFGBG="default;default"

pathdirs=(
    ~/scripts
)
for dir in $pathdirs; do
    if [ -d $dir ]; then
        path+=$dir
    fi
done

export EDITOR="vim"

#if [[ "$TERM" == "rxvt-unicode-256color" ]]; then
#	xseticon -id $WINDOWID /home/sunn/.icons/AwOkenWhite/clear/128x128/apps/terminal1.png
#fi
