# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, callPackage, ... }: {


  # Load fonts
  fonts.fonts = with pkgs; [
    inconsolata fira-code hasklig 
  ];

  fonts.fontconfig = {
    ultimate.enable = true; # shiranai, demo maybe good.
    defaultFonts = { #used when current/default font cannot display glyphs
                     #TODO: does this override ~/.config/fontconfig/fonts.conf?
        monospace = [
            "Hasklig"
            "Fira Code"
            "monospace"
        ];
        sansSerif = [ "Nimbus Roman" ];
        serif = [ "Nimbus Roman" ];
    };
  };
 imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the GRUB 2 boot loader.
  boot.kernel.sysctl."vm.overcommit_memory" = "1";
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.useOSProber = true;
  boot.loader.systemd-boot.enable = true;
  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only

  # networking.hostName = "nixos"; # Define your hostname.
   networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  # networking.networkmanager.enable = true;  

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  # Set your time zone.
  time.timeZone = "America/Chicago";

  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
      lsof wget emacs vim curl killall htop pstree xorg.xkill
      xorg.xinit  xorg.xrandr xorg.xmodmap xbanish  #and some xkb package
      xorg.xev  # discover key names for use in sway (or i3) config file
      vlc encryptr youtube-dl google-chrome firefox dolphin notepadqq 
      konsole  kitty  termite kcolorchooser 
      breeze-icons
      ffmpegthumbnailer
      #gtk-engine-murrine  gtk_engines  # these are needed for some themes
      lxqt.pavucontrol-qt  # volume control
      vimPlugins.purescript-vim  vimPlugins.haskell-vim  vimPlugins.awesome-vim-colorschemes
      networkmanager  networkmanagerapplet
      redshift xflux geoclue2  wlroots
      pax-rs 
      audacity
      nodejs
      yarn
      cups-brother-hl1110
      wayland  xwayland  wayland-protocols
      #kcolorchooser 
      wpgtk  # generate theme from a picture -- command 'wpg'
      slack 
      audacity
      cmus
      glxinfo
      mplayer
      p7zip
      stretchly
      tmux
      psc-package
      zsh
      libreoffice
      massren
      vimPlugins.intero-neovim
      vimPlugins.vim-plug
      #adoptopenjdk-jre-openj9-bin-11
      #clojure
      #leiningen
      #tmux
      #tmuxinator
      #source-code-pro
      #tldr
   
      #hardware utils
      acpi usbutils lshw pciutils

      xwayland brightnessctl git ffmpeg 
      ghostwriter multimarkdown

      #viewers
      geeqie gpicview qpdfview

      #fonts
      inconsolata-lgc fira-code hasklig hanazono  

      # runtimes & compilers
      ghc python37 stack lua gcc gnumake


      ###############################################################
      # other compositors/window-managers
      # waybox   # An openbox clone on Wayland
      # bspwc    # Wayland compositor based on BSPWM
      # cage     # A Wayland kiosk (runs a single app fullscreen)
      # wayfire   # 3D wayland compositor
      # wf-config # wayfire config manager
      ###############################################################
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
    services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  
  programs.sway.enable = true;
  # Enable the X11 windowing system.
  services.logind.lidSwitch = "hybrid-sleep";
  services.logind.lidSwitchDocked = "ignore";
  services.logind.lidSwitchExternalPower = "ignore";
  services.xserver = {
    enable = true;
    autorun = false;
    exportConfiguration = true; # link X config to /etc, so that X config is like in most Linux distros
    synaptics = { #affects only after reboot (or maybe logging-out then in again?) maybe same for libinput
      enable = true;
      tapButtons = false;
      vertEdgeScroll = false;
      horizEdgeScroll = false;
      vertTwoFingerScroll = true;
    };
    desktopManager = {
      default = "none";   #or "xfce"
      xterm.enable = false;
      xfce = {
        enable = true;
        noDesktop = true;
        enableXfwm = false;
      };
    };
    windowManager.i3 = {
      enable = true;
      extraPackages = with pkgs; [
        dmenu #application launcher most people use
        i3status # gives you the default i3 status bar
        i3lock  #default i3 screen locker
        i3blocks  #default i3 screen locker
      ];
    };
  };
  services.redshift = {  # when are these supposed to take effect? When any window manager is running?
    enable = true;
    temperature.day = 6500;
    temperature.night = 2700;
    brightness.day = "1.0";
    brightness.night = "0.25";
  };
  location.latitude = 32.9629;
  location.longitude = -96.6692;
  hardware.trackpoint.enable = true;
  hardware.trackpoint.sensitivity = 1;
  #services.xserver.dpi = 180;
  

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.cameron = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?

}
