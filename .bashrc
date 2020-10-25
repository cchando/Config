#
# ~/.bashrc
#
. $HOME/.bash_aliases 		#source bash_aliases
# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# If running in tty1, run startx
if [ "$(tty)" = "/dev/tty1" ]; then startx; fi

shopt -s extglob dotglob globstar

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
alias rm='mv -t /home/cameron/.trash'
alias rmu='rm' # remove unsafe
alias inst='nix-env -iA'
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
alias charmap='gucharmap'
alias grep='egrep'
alias mv='mv -i'
alias cp='cp -i'
alias sudo='sudo '
alias ls='ls --color=auto'
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
alias ydl='echo "Download MP3s:/n" && youtube-dl -x --no-playlist -o "~/Music/youtube-dl/%(title)s.%(ext)s" --audio-format mp3'
alias ydlp='echo "Download MP3s:/n" && youtube-dl -citx --yes-playlist -o "~/Music/youtube-dl/%(title)s.%(ext)s" --audio-format mp3'
alias ydlv='youtube-dl --no-playlist -o "~/Videos/youtube-dl/%(title)s.%(ext)s" -f "bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best"'
alias ydlpv='youtube-dl -cit --yes-playlist -o "~/Videos/youtube-dl/%(title)s.%(ext)s" -f "bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best"'
alias r='command' # "raw"
alias show='alias'
alias et='emacs -nw'
alias vimode='set -o vi'
alias tr='racket -I typed/racket'
alias ra='racket'
alias fonts='gucharmap'
alias lfonts='gtk2fontsel'
alias lst='ps -A | grep'
alias xr='xrdb -merge ~/.Xdefaults'
alias bat='acpi'    # or 'upower ...'
alias music='cmuse'
alias zip='7z a'
alias unzip='7z x'
alias zipt='7z a -tzip'
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
function conda-shell {
    nix-shell ~/.conda-shell.nix
}
#alias mvfiles='find . type f -regextype egrep -regex '.*[a-z]+\.el$' -execdir mv -t dest {} \+'  # example only

# config edits
alias enc='emacs /etc/nixos/configuration.nix -fs'
alias eba='emacs ~/.bashrc -fs'
alias evi='emacs ~/.vimrc -fs'
alias eiwm='emacs ~/.i3/config -fs'
alias eis='emacs ~/.i3status.conf -fs'
alias ete='emacs ~/.config/termite/config -fs'
alias eki='emacs ~/.config/kitty/kitty.conf -fs'
alias etm='emacs ~/.tmux.conf -fs'
alias est='emacs ~/.config/stretchly/config.json -fs'
# alias es='emacs ~/.config/sway/config -fs'

# git commands
alias uns='git restore' # "unstage"
alias unt='git rm --cached' # "untrack"
alias s='git switch'
alias st='git status'
alias com='git commit'
alias grm='git rm'
# alias untr='git rm --cached' # "git untrack"
alias grc='git rm --cached' # "git untrack"
alias br='git branch'
alias cbr='git switch -c'
alias cbrO='echo "use cbr --force"'
alias cbrF='echo "use cbr --force"'
alias pull='git pull'
alias pullr='git pull --rebase'
alias push='git push'
alias add='git add'
alias amend='git commit amend'
alias reba='git rebase'
alias rebase='git rebase'
alias log='git log'
alias logp='git log --pretty=oneline'
alias logpp='git log --all --decorate --oneline --graph'
alias ltracked='git ls-tree -r --name-only' #list tracked files in given branch
alias ltr='git ls-tree -r --name-only' #list tracked files in given branch
alias tag="git tag"



PS1='[\u@\h \W]\$ '

stty -ixon

