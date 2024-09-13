
    pkgs: with pkgs; [
      # libs
      # gmp zlib #should I even have this here? After all, Haskell stack projects already use nix:packages:zlib.
      # libcrafter #high-level C++ net packet sniffer & fabricator. See libnet for a low-level one. libtins is another high-level one.

      # hw
      acpi usbutils pciutils #lspci
      # hwinfo #strictly better than lshw?
#     dmidecode #what is? sounds interesting.

      # network tools, services, & utilities
      wget curl wpa_supplicant wavemon traceroute # macchanger nettools netcat ngrep nmap xorg.xmodmap
#     connect #make net connections via SOCKS and https proxy
#     badvpn #vpn- and network-related tools
#     cjdns #"encrypted networking for regular people"
#     gnome3.gnome-nettool #collection of net tools
#     dsniff #net auditing & pentesting
#     ethtool #utility for controlling net drivers & hw
#     #flent #"the FLExible network tester" #Failed to build: AssertionError: 'net.ipv4.tcp_autocorking' not found in {}. Looks like a python error.
#     #inetutils #looks like it's already installed in nixos
#     iproute iputils
#     kismet #wireless net sniffer
#     libdnet #"simplified, portable iface to many low-level net routines"
#     netsniff-ng #"swiss army knife for daily Linux network plumbing"
#     #what's openconnect? There's a package for OpenConnect "extended to support Palo Alto's Networks' GlobalProtcect VPN" (openconnect_pa)
#     #what's WireGuard?
#     toxiproxy #proxy which simulates various network difficiencies
#     wireshark-gtk
#     proxytunnel #bind stdin and stdout to a server via an HTTPS proxy

      ## monitors, stats, and analysis
#     jnettop
#     iptraf #see also iptraf-ng, a fork of iptraf
#     bro #monitor for vulnerable or malicious software
#     #cytoscape: for "complex networks" and "visualization"
#     bmon #bandwidth monitor. see too bwm_ng
#     iftop #display bandwidth for a given device
#     dnstop #libpcap program that displays DNS traffic
#     mtr #monitor. traceroute + ping. monitors for overloaded links [connections]
#     #netalyzr #no manpage, no -h or --help flags. no output; just hangs, for any arguments.
#     netperf #benchmark nets
#     #nuttcp #performance & measurement. Somehow failed to build nixos config with this.
#     nload #ncurses. measures amount of traffic
#     #note that network manager has OpenConnect and OpenVPN plugins (see the nixos package list; there are multiple packages for each)
#     p0f #passive net recon & fingerprinting tool
#     #packetbeat7 #ES7 beat about packets ; there're packetbeat5 for ES5, and packetbeat for ES6, too.
#     riemann #no idea wtf it's supposed to do
#     skydive #wtf is it?
#     snort #intrusion prevention & detection (IDS/IPS)
#     snscrape #"social network service scraper" in Python...?
#     ssldump #TLS analyzer
#     tcpdump #sniffer
#     vnstat #generate reports in many formats, from data recorded by *NIX OSes themselves

      ## interesting
#     tcpreplay #edit and replay
#     thc-hydra #logon cracker for many different services
#     iodine #tunnel IPv4 data through DNS servers
#     testssl #get a server's SSL/TLS capabilities

      ## network-based userland programs
      netrw #simple tool for transporting data via network
      lftp #ftp-like programs
      mldonkey #client for many p2p nets, w/multiple frontends
      driftnet #watches network traffic, "picking-out JPEG and GIF images for display"
      enet #"simple & robust net com layer atop UDP"
      freenet #decentralized & cencorship-free network
#     ndn-cxx
#     zeronet #decentralized net built on blockchain

      ## network managers
#     connman connman-ncurses
#     iwd

      # admin tools
      lsof nix-index

      # utilities
      youtube-dl
      #python37Packages.conda
      tmux tmuxinator
      file
      wl-clipboard # wayland version of xclip
      pulseaudio pulseaudio-ctl pavucontrol
      # fcitx-configtool
      htop gotop p7zip
      tree gnome3.zenity
      xdotool
      xorg.xev
#     xorg.xkbcomp
#     xorg.xwininfo
      killall
      bluez blueman
      # screenkey grim
      # pamix #ponymix
      #minio minio-client #An S3-compatible replacement for ls, cp, mkdir, diff and rsync commands for filesystems and object storage
      #consider using in conjunction with [sphinxsearch]( http://sphinxsearch.com/about/sphinx/ )
      #corgi-v0.2.4
#     wpgtk #executable name is wpg
      # neofetch

      # managers
      # anthy fcitx-engines.anthy
      # sylpheed

      #languages
      #clojure leiningen
      # dapl
      # gnuapl
      # j

      # window
      # haskellPackages.kmonad
      (import ./kmonad.nix) # kmonad expr
      xwayland
      compton # enable window transparency
      xbanish unclutter
      glib # provides gsettings, which is needed for GNOME icons
      pkgs.gsettings-desktop-schemas
      # vwm wtftw ly

      #sickrage-v2018.07.21-1 # video library

      # binutils
      patchelf binutils

      # general everyday applications
      #fira-code fira-code-symbols
      gammastep #redshift #xflux
      nitrogen #wallpaper
      breeze-icons #for dolphin
      kitty # tmux cool-retro-term gnupg #alacritty
      # nextcloud-client pwgen hexio theharvester
      brightnessctl
      git
      #nixos.gnome3.gnome-clocks
      #sublime-merge #git. There's no nix package in the repo (yet)
      kde-cli-tools #needed for dolphin file browser

      # catamorphic (map/filter)
      # ffmpeg pdfgrep graphviz graphicsmagick jhead sourceHighlight
      audacity
      #split2flac #unfree?

      # streaming
      aria uget hexchat

      # editors. Remember that an editor merely selects data and perform a manipulation on them. Thus an editor's worth is measured by its ability to identify subsets of data and define functions on data. Vim is a good text editor because it defines text well (in terms of columns, rows, words, letters), can select text by pattern or not (i.e. moving the cursor by its movement keys or by searching w/regex); and vim allows easy definition of transforms via macro recording. Of course, these operations are all in terms of selecting data and deleting, inserting, overwriting, or the occasional character case toggle.
      # as ever, the most important aspect of an editor is defining type well and making convenient expressions in terms of the atoms/basis.
      # no editor is good if it doesn't support scripting/macros -- does not need to be in text, as long as arbitrary expressions can be elegantly defined it atomic terms.
      vim emacs #howl #text
#     aegisub #subtitles
#     #cinelerra non-free #video
#     shotcut pitivi openshot-qt avidemux #video
#     puddletag #audio tags
#     brackets #webpage
      # tikzit #mathematical graphs
      # gimp inkscape #image
      #lyx #texmaker texstudio gummi #latex
      libreoffice
      #eagle (non-free) #pcb & schematics
      massren #emv "mass rename" filepaths
      #masterpdfeditor (non-free)
#     ht #executables. includes analysis
#     ipe #"figures"?
#     k3d #?
#     leo-editor #something new?
#     ised #numeric sequences
#     sops #encrypted data
      sqlite

      ## simulators: edits to environment data beget realtime changes to a simulation environment
      # these are the next generation of editor.
#     lighttable #programs
      #processing #does not install. Says to set oraclejdk.accept_license = true;, but that attribute does not exist

      ## notes. It's still LaTeX vs. MMD for me. All markdown-using programs should be hacked to use MMD.
      # notes-up sux.
      multimarkdown # mindforger mdp qownnotes # ghostwriter

      ## hex editors
      #dhex #features diff mode and handy hex calculator.
           #has some rendering trouble, at least on kitty.
           #curses style. sensible keybinds. allows custom binds, too. has statusbar with common keybinds for easy reference.
           #search by hex or string. Use arrow keys to select UI elements in curses windows. Very easy.
           #search is a little odd: found search term always occupies first byte position.
      # okteta hexd xxd bviplus hecate hyx tweak watchexec wxhexeditor
      # hexcurse # better than hexedit at least

      # browsers / viewers
      mpd vimpc
      # volctl # individual slider bars for each app for pulseaudio
      vlc mcomix3 gpicview cmus qpdfview
      # haxor-news newsboat
      gtk2fontsel gucharmap
      # w3m

      ## internet
      google-chrome
      chromium
      # luakit
      # palemoon # forked from firefox. supports legacy, more capable extensions, plus themes!
      # these may be libs, not exes, though that contrasts the nixos docs: dleyna-server python27Packages.weboob python27Packages.twill perl528Packages.HTMLClean

      # diffs
#     diff-pdf diffpdf #for pdfs
#     diffuse diffoscope
#     perceptualdiff #for images
#     wdiff #by words
      #nixos.python27Packages.htmltreediff

      ## binary diffs
#     bsdiff vbindiff

      ## filesystem diffs
#     xxdiff

      # emulators
      # mgba

      # fonts
      inconsolata-lgc fira-code hasklig hanazono libertine lmodern #gyre-fonts liberation-fonts
      # math fonts
      lmmath xits-math stix-two libertinus tex-gyre-bonum-math tex-gyre-schola-math
      corefonts #microsoft's core fonts


      # runtimes & compilers
      python37 stack lua luarocks gcc gnumake unzip #unzip IS NEEDED FOR LUAROCKS?!

      # servers
      #darkhttpd #small & secure static webserver. Suxadik.
      #droopy #allows others to upload files to localhost
      pshs #pretty small HTTP server. CLI tool for file sharing
      # woof #"web offer one file": easily make a file available over net
      # webfs
      shairplay #apple Airplay and raop protocol server
      #sipswitch #secure P2P VoIP server that uses the SIP protocol
      #sks #easily-deployable & decentralized OpenPGP keyserver
      #dovecot #open-source secure IMAP & POP3

      #analyzers
#     crex

      # others
      # check-out bloop for scala production; ccls or cquery for c[++] repl; caddy for HTTP/2 web server with "automatic" HTTPS
      #websocketd #redirect stdin/out to WS

      ## iphone stuff
      #usbmuxd #run this first: sudo usbmuxd -fv
      #ifuse #then run this after iphone's trusted the computer
    ]
