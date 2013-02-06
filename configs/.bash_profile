#
# ~/.bash_profile
#

export LM_LICENSE_FILE="2100@keyserver.bucknell.edu"

if [ -n "$DISPLAY" ]; then
     BROWSER=chromium
fi

[[ -f ~/.bashrc ]] && . ~/.bashrc
