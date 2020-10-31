#
# ~/.bashrc
#
. $HOME/.bash_aliases 		#source bash_aliases
# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# If running in tty1, run startx
if [ "$(tty)" = "/dev/tty1" ]; then startx; fi

shopt -s extglob dotglob globstar

function conda-shell {
    nix-shell ~/.conda-shell.nix
}

# # Notes
#stat -- display file permissions -- stat -c %A %n
#lspci -- list PCI bus devices
#ping 8.8.8.8
#sftp cameron@10.0.0.2
#xdg-mime  #check / modify file-type associations
#wpa_passphrase Frontier4704 21422325889218 > /etc/wpa_supplicant.conf #5148275597
#ip a -- get status of wireless devices
#alias ni='nix-instantiate --eval'
#xinput list

# Nix & NixOS
alias ns='nix-shell'
alias cr='less ~/.nixenv | grep'   # check nixpkgs repo file
alias crd='nix-env -qaP --description | grep'   # check nixpkgs repo
alias rb='sudo nixos-rebuild switch'  # rebuild NixOS
alias clean='nix-collect-garbage -d'
alias chupd='nix-channel --update'
alias upg='nix-env --upgrade'
alias upgv='nix-env --upgrade --always'
alias rollback='nix-env --rollback'
alias inst='nix-env -iA'
alias nist='nix-env -iA'
alias isnt='nix-env -iA'
alias lgen='nix-env --list-generations'
alias sgen='nix-env --switch-generation'
alias deletegen='nix-env --delete-generations'
alias unin='nix-env --uninstall'
alias lhave='nix-env -q'
alias linst='nix-env -q --installed'
# alias anp='nix-env -f '<nixpkgs>' -iA'   # nodePackages.searchterm

# program aliases
alias e='exit'
alias c='clear'
alias rm='command mv -t /home/cameron/.trash'
alias rmu='command rm' # remove unsafe
alias rmdir='command rm -r' # remove recursive
alias ls='ls --color=auto'
alias charmap='gucharmap'
alias grep='egrep'
alias mv='mv -i'
alias cp='cp -i'
alias sudo='sudo '
alias kill='kill -9'
alias xflux='xflux -z 75044'
alias redshift='redshift -l 32.96:-96.67 -t 6500:2000'

# wifi
alias conh='sudo nmcli device wifi connect Frontier4704 password 21422325889218' #5148275597
alias con='nmcli device wifi connect'
alias wifi='nmcli device wifi' # list wifi networks
alias wpabg='sudo wpa_supplicant -i wlp1s0 -c f -B'  # (-d for debugging, -B for background/daemon)
alias ipsu='sudo ip link set wlp1s0 up'
alias aip='sudo ip addr add 10.0.0.1/8 dev enp0s31f6'
# alias wpastart='sudo systemctl start wpa_supplicant' # I think this doesn't work -- wpa is controlled by systemd

# Misc

# alias ydl='nohup youtube-dl -x --no-playlist -o "~/Music/youtube-dl/%(title)s.%(ext)s" --audio-format mp3 &>/dev/null &'
# alias ydlp='nohup "youtube-dl -cix --yes-playlist -o "~/Music/youtube-dl/%(playlist)s/%(title)s.%(ext)s" --audio-format mp3" &>/dev/null &'
# alias ydlv='nohup "youtube-dl --no-playlist -o "~/Videos/youtube-dl/%(title)s.%(4]+bestaudio[ext=m4a]/best[ext=mp4]/best"" &>/dev/null &'
# alias ydlpv='nohup "youtube-dl -ci --yes-playlist -o "~/Videos/youtube-dl/%(title)s.%(4]+bestaudio[ext=m4a]/best[ext=mp4]/best"" &>/dev/null &'

alias ydl='youtube-dl -x --no-playlist -o "~/Music/youtube-dl/%(title)s.%(ext)s" --audio-format mp3'
alias ydlp='youtube-dl -cix --yes-playlist -o "~/Music/youtube-dl/%(playlist)s/%(title)s.%(ext)s" --audio-format mp3'
alias ydlv='youtube-dl --no-playlist -o "~/Videos/youtube-dl/%(title)s.%(4]+bestaudio[ext=m4a]/best[ext=mp4]/best"'
alias ydlpv='youtube-dl -ci --yes-playlist -o "~/Videos/youtube-dl/%(playlist)s/%(title)s.%(4]+bestaudio[ext=m4a]/best[ext=mp4]/best"'

# alias emacs-def='git stash push -m "push cac .spacemacs" && mkdir /home/cameron/tmp && mv /home/cameron/.spacemacs /home/cameron/tmp/ && mv -f /home/cameron/spacemacs-default /home/cameron/.spacemacs && emacs -mm'    # "emacs default"
# alias emacs-cac='git stash pop'
# # alias emacs-cac='mv -f home/cameron/tmp/.spacemacs  /home/cameron/ && mv -f /home/cameron/spacemacs-default /home/cameron/.spacemacs && emacs -mm'

alias uvi='mv -f /home/cameron/Downloads/vimium-options.json  /home/cameron/.vimium-options.json'
alias name='echo -e "Instance, Class, Title:\n" && xprop | grep "WM_CLASS|^WM_NAME"' # get [instance, class, title] of whatever window we click on next
alias getkey='xev'
alias key='xev'
alias wifioff='sudo rfkill block all'
alias wifion='sudo rfkill unblock all'
alias size='du -sh'
alias lmon='xrandr --current' # list monitors
alias findp='find . -ipath'
alias r='command' # "raw"
alias show='alias'
alias ir='i3-msg reload'
alias et='emacs -nw'
alias vimode='set -o vi'
alias tr='racket -I typed/racket'
alias ra='racket'
alias fonts='gucharmap'
alias lfonts='gtk2fontsel'
alias lst='ps -A | grep'
alias xr='xrdb -merge ~/.Xdefaults'
alias bat='acpi'    # or 'upower ...'
# alias music='cmuse'
alias zip='7z a' # zip <NAME>.7z <SOURCE>   # yes, the ".7z" is needed, even though it's semantically redundant
alias unzip='7z x'
alias bhi='sudo brightnessctl set 100%'
alias bmhi='sudo brightnessctl set 75%'
alias bmed='sudo brightnessctl set 50%'
alias bmlo='sudo brightnessctl set 25%'
alias blo='sudo brightnessctl set 10%'
alias br3='sudo brightnessctl set 3%'
alias br2='sudo brightnessctl set 2%'
alias br1='sudo brightnessctl set 1%'
alias pade="xinput set-prop "$(xinput list | grep -i touchpad | cut -f 2 | grep -oE '[[:digit:]]+')" 'Device Enabled' 1"
alias sticke="xinput set-prop "$(xinput list | grep -i stick | cut -f 2 | grep -oE '[[:digit:]]+')" 'Device Enabled' 1"
alias alsamutetoggle='amixer -q -D pulse sset Master toggle'

#alias mvfiles='find . type f -regextype egrep -regex '.*[a-z]+\.el$' -execdir mv -t dest {} \+'  # example only

# config edits
alias enc='emacs -nw /etc/nixos/configuration.nix -fs'
alias eba='emacs -nw ~/.bashrc -fs'
alias evi='emacs -nw ~/.vimrc -fs'
alias eiwm='emacs -nw ~/.i3/config -fs'
alias eis='emacs -nw ~/.i3status.conf -fs'
alias ete='emacs -nw ~/.config/termite/config -fs'
alias eki='emacs -nw ~/.config/kitty/kitty.conf -fs'
alias etm='emacs -nw ~/.tmux.conf -fs'
alias est='emacs -nw ~/.config/stretchly/config.json -fs'
# alias es='emacs -nw ~/.config/sway/config -fs'

# git commands
alias uns='git restore' # "unstage"
alias unt='git rm --cached' # "untrack"
alias s='git switch'
alias st='git status'
alias com='git commit'
alias grm='git rm'
alias grc='git rm --cached' # "git untrack"
alias rest='git restore'
alias restore='git restore'
alias br='git branch'
alias cbr='git switch -c'
alias cbrO='echo "use cbr [-f | --force]"'
alias cbrF='echo "use cbr [-f | --force]"'
alias pull='git pull'
alias pullr='git pull --rebase'
alias push='git push'
alias add='git add'
alias amend='git commit amend'
alias reba='git rebase'
alias rebase='git rebase'
alias reset='git reset'
alias log='git log'
alias logp='git log --pretty=oneline'
alias logpp='git log --all --decorate --oneline --graph'
alias ltracked='git ls-tree -r --name-only' #list tracked files in given branch
alias ltr='git ls-tree -r --name-only' #list tracked files in given branch
alias tag="git tag"



PS1='[\u@\h \W]\$ '

stty -ixon

