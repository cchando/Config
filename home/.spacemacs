;; <SPC><SPC> byte-compile-file xah-math-input.el

; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;; /= ->> *** <<< <*> >>> >>- -<< ==> .. ... <$> <+> <* *> |> <| <-> >- -< :: :::
;; <=> =<< >>= <=< >=> <> ++ || !! && -> => \\ +++ <|> != !==
;; ∀ ∃ ∊ ∈ ⋸ ∉ ∑ Π ¬ ⊼ ⊽ ⊻ ∅ ⊖ ⊕ ⊗ ⊃ ⊇ ⊉ ∩ ∪ ⋂ ⋃ ⋀ ⋁ ⋵ ⋹ ⋺ ⋻ ⋽ ∵ ∴ ⋙ ⨁ ⨂ ⨯ ⩽ ⩾ ⨌ ↰ ↦
;; → ← ↑ ↓ ∆ ∇ ⍴ ∊ ⍳ ⍺ ⍵ ⍷ ⍸ ⍶ ⍹ ⌐ ∘ ⍛ ¨ ⍨ ⍤ ⍥ ⍩ ᑈ ⍡ ⍢ ⍫ ∧ ∨ ⍲ ⍱  ∩ ∪ ∼ ≡ ≢ ≤ ≥ ⊂ ⊃ ⊖ ⊢ ⊣ ⊤ ⊥ ⋄ ⌈ ⌊ ⌶ ⌷ ⌹ ⌽
;; ⌿ ⍀ ⍉ ⍋ ⍒ ⍎ ⍕ ⍝ ⍞ ⍪ ⎕ ○ × ÷
;; ⍬ ⍔ ⌻ ⍫ ⌾ ⍁ ⍍ ⍠ ⌼ ⍟ ⍏ ⍛ ⍓ ¨ ⍢ ⍤ ⍰ ⍂ ⍃ ⍅ ⍖ ⍘ ⍙ ⍯ ⍆ ⍌ ⍑ ⍦ ⍧ ⍚ ⍄ ⍊ ⍮ ⍭ ⍇ ⍐ ⍈ ⍗
;; 𝔹 𝕃 𝕍
;; ℕ ℤ ℝ ℍ ℚ
;; 𝐋  𝐙  𝑭
;; ⮢ ⬅ ⮈ ⮲ ⭅
;; f⍥g⍤∘f
;; ~R∊R∘.×R

;; spacemacs-dark: font-lock-comment-face:   fg: #2aa1ae   bg: #292e34

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     lua
	 haskell
	 purescript
	 html
	   vimscript
	 racket
	 ruby
	 asciidoc
	 javascript
	 clojure
		 ;; themes-megapack
	 ;; ----------------------------------------------------------------
	 ;; Example of useful layers you may want to use right away.
	 ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
	 ;; <M-m f e R> (Emacs style) to install them.
	 ;; ----------------------------------------------------------------
	 helm
	 ;; evil-snipe
	 auto-completion
	 ;; better-defaults
	 emacs-lisp
	 git
	 ;; markdown
	 ;; org
	 (shell :variables
						shell-default-shell 'ansi-term ; eshell shell term ansi-term (default on Linux/macOS) multi-term vterm
						close-window-with-terminal t
			shell-default-height 20
			shell-default-position 'bottom)
	 ;; spell-checking
	 ;; syntax-checking
	 ;; version-control
	 )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(evil-easymotion j-mode dyalog-mode gnu-apl-mode workgroups rebecca-theme nix-mode auto-sudoedit xah-math-input dyalog-mode hasklig-mode pretty-mode fira-code-mode moe-theme intellij-theme lab-themes flucui-themes base16-theme afternoon-theme color-theme-modern sublime-themes heroku-theme atom-one-dark-theme solarized-theme light-soap-theme color-theme-sanityinc-tomorrow apropospriate-theme underwater-theme occidental-theme ample-theme flatui-theme alect-themes night-owl-theme tldr parinfer disable-mouse highlight-indent-guides highlight-indentation
(kbd-mode
  :location
  (recipe
    :fetcher github
    :repo "kmonad/kbd-mode"))) ;fira-code-mode doom-themes sudo-edit auto-sudoedit
   ;; solarized-dark-theme tomorrow-blue-theme pheonix-dark-mono-theme apropospriate-light-theme adwaita-theme alect-light-theme
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '(xah-math-input pretty-mode)
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(company-tern company-ghc ghc intero)
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
								(projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(

												 ;; --- Dark themes ---
												 spacemacs-dark
												 spacemacs-dark-cac
												 ;; base16-atelier-cave ;; rabi-ribi
												 moe-dark ;; colorful
												 ;; night-owl-cac
												 ;; harmonic-dark-cac ;; like night-owl but a bit lighter
												 ;; base16-solarflare
												 ;; flucui-dark ;; based off of lab-dark
												 ;; base16-snazzy ;; similar-ish to atelier-cave
												 ;; lab-dark ;; medium pale blue w/ greens and orange
												 ;; base16-atelier-plateau ;; similar to cave
												 ;; base16-atelier-savanna ;; dark green, orange, ocean blue
												 ;; base16-bespin ;; dark brown, light brown, green, blue
												 ;; estuary ;; dark brown
												 ;; misterioso ;; light-blue bar, gray bg
												 ;; tango-dark ;; yellow bar, gray bg
												 ;; outrun-dark ;; very dark purple-blue w/ bright contrasts
												 ;; harmonic-dark ;; like night-owl but a bit lighter
												 ;; night-owl ;; midnight blue
												 ;; heetch ;; deep purple w/ magenta, red
												 ;; fogus
												 ;; graham ;; dark-blue w/ bright orange-gold

												 ;; --- Colored themes ---
												 moe-light ;; colorful
												 ;; underwater ;; medium-dark blue
												 ;; alect-light
												 ;; ample-light
												 ;; occidental
												 ;; base16-greenscreen
												 ;; heroku
												 ;; kingsajz
												 ;; materia ;; not a typo. dark-grey w/ green, orange, yellow
												 ;; shaman
												 ;; bharadwaj
												 ;; bharadwaj-slate
												 ;; solarized-dark
												 ;; word-perfect ;; bright blue, Win95 style

												 ;; --- Light themes ---
												 ;; lab-light
												 intellij
												 ;; spacemacs-light
												 ;; cupertino
												 ;; flucui-light ;; based off of lab-light
												 ;; ritchie
												 ;; flatui ;; colorful
												 ;; whiteboard
												 ;; ia-light ;; similar to whiteboard
												 ;; default
												 ;; adwaita
												 ;; unikitty-light ;; white-purple w/ magenta, vim med purple, aqua, gold
												 ;; apropospriate-light
												 ;; harmonic-light

												 )

   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state nil
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '(

															 ;; NOTE: It's not clear what effect these have exactly. Putting a font first sets it as the default,
															 ;;       but font B may be used for e.g. most math chars, even though font A was specified here
															 ;;       before B (and supports the same char range), or even if A was specified and B was not.
															 ;;       Also, [("Fira Code" :size 12)] gives Fira Code size 13. Go figure.

															 ;; ("Hasklig"
															 ;;  	:size 13
															 ;;  	:weight normal
															 ;;  	:width normal
															 ;;  	:powerline-scale 1.1)

															  ("Fira Code"
															   :size 12
															   :weight normal
															   :width condensed
															   :powerline-scale 1.1)

															 ;; ("Iosevka"
															 ;;  :size 12
															 ;;  :weight normal
															 ;;  :width condensed
															 ;;  :powerline-scale 1.1)

															 ;; ("STIX"
															 ;;  :size 12
															 ;;  :weight medium
															 ;;  :width condensed
															 ;;  :powerline-scale 1.1)

															 ;; ("Code2000"
															 ;;  :size 12
															 ;;  :weight semibold
															 ;;  :width condensed
															 ;;  :powerline-scale 1.1)

															 ;; ("Noto Sans Mono"
															 ;;  :size 12
															 ;;  :weight semibold
															 ;;  :width condensed
															 ;;  :powerline-scale 1.1)

															 )
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."



	;; speed up emacs
	(setq gc-cons-threshold 200000000)            ; 200MB
	(run-with-idle-timer 120 t #'garbage-collect)

  (setq-default xah-math-input-mode t)

	(add-hook 'adoc-mode-hook 'xah-math-input-mode)
	(add-hook 'prog-mode-hook 'xah-math-input-mode)

	(remove-hook 'prog-mode-hook 'global-highlight-parentheses-mode)
	(remove-hook 'prog-mode-hook 'highlight-parentheses-mode)
	;; (remove-hook 'prog-mode-hook 'line-number-mode)
	;; (add-hook 'adoc-mode-hook 'smartparens-mode)
	;; (add-hook 'prog-mode-hook 'smartparens-mode)
	(remove-hook 'adoc-mode-hook 'smartparens-mode)
	(remove-hook 'prog-mode-hook 'smartparens-mode)
	(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

	;; Fira Code    diaeresis
	;; Hasklig      diaeresis
  ;; --------- APL: ---------
  ;; Cambria Math
  ;; Noto Sans Math
  ;; Noto Sans Mono
  ;; Code2000
	;; DejaVu Sans Mono


  ;; --------- Mathematics: ---------
  ;; TeX Gyre DejaVu Math
  ;; DejaVu Serif
	;; Latin Modern Math
	;; XITS Math
	;; STIX Two Math     ⍲ ⍱ ∆ ∇ ⍫  ≤ ≥ ⍎ ⍕ → ← ↑ ↓
	;; Libertinus Math
	;; TeX Gyre Bonum Math
	;; TeX Gyre Schola Math

  ;; --------- Arrows: ---------
	;; Verdana     arrows
	;; Liberation Sans     arrows
	;; TeX Gyre Schola (Math)    arrows
	;; FreeMono     ⍸ ⍷ ⍘ ⍚  (⍹ ⍜ ⍪ ⍤)


  ;; --------- Mathematical Letter-like Symbols:---------
  ;; TeX Gyre DejaVu Math
	;; Latin Modern Math
	;; XITS Math
	;; STIX Two Math     ⍲ ⍱ ∆ ∇ ⍫  ≤ ≥ ⍎ ⍕ → ← ↑ ↓
	;; Libertinus Math
	;; TeX Gyre Bonum Math
	;; TeX Gyre Schola Math
	;; TeX Gyre Pagella Math

	;; Hasklig:pixelsize=13:foundry=ADBO:weight=bold:slant=normal:width=normal:spacing=100:scalable=true
  ;; -ADBO-Hasklig-bold-normal-normal-*-*-*-*-*-m-0-iso10646-1
	;; -CTDB-Fira Code-bold-normal-normal-*-*-*-*-*-m-0-iso10646-1
	;; -PfEd-DejaVu Sans Mono-bold-normal-normal-*-*-*-*-*-m-0-iso10646-1
	;; -PfEd-DejaVu Sans-bold-normal-normal-*-*-*-*-*-m-0-iso10646-1

	;; (add-to-list 'face-ignored-fonts "FreeMono")


	;; NOTE: when this list is too long, the earlier ones get pushed off!!



	;; ----------- For minimal font set -------------------------------------------------------------

	;; (set-fontset-font "fontset-default" '(#x2200 . #x237A) "Cambria Math") ; APL symbols
	;; (set-fontset-font "fontset-default" '(#x2200 . #x237A) "Iosevka") ; APL symbols
	(set-fontset-font "fontset-default" '(#x2200 . #x237A) "DejaVu Sans Mono") ; APL symbols
	;; (set-fontset-font "fontset-default" '(#x2200 . #x237A) "Fira Code") ; APL symbols
	;; (set-fontset-font "fontset-default" '(#x2200 . #x237A) "Code2000") ; APL symbols
  (set-fontset-font "fontset-default" '(#x2200 . #x22FF) "Cambria Math") ; math operators block
	;; (set-fontset-font "fontset-default" '(#x2190 . #x21FF) "Iosevka") ; arrows
	(set-fontset-font "fontset-default" '(#x2190 . #x21FF) "Cambria Math") ; arrows
	;; (set-fontset-font "fontset-default" '(#x2190 . #x21FF) "STIX") ; arrows
	;; (set-fontset-font "fontset-default" '(#x2190 . #x21FF) "DejaVu Sans Mono") ; arrows
	(set-fontset-font "fontset-default" '(#x1D400 . #x214F) "Cambria Math") ; mathematical alphanumeric and letterlike symbols
	;; (set-fontset-font "fontset-default" '(#x1D400 . #x214F) "TeX Gyre Pagella Math") ; mathematical alphanumeric and letterlike symbols
	;; (set-fontset-font "fontset-default" '(#x2200 . #x237A) "Iosevka") ; APL symbols
	;; (set-fontset-font "fontset-default" '(#x2200 . #x237A) "Code2000") ; APL symbols

	;; (set-fontset-font "fontset-default" '(#x2200 . #x22FF) "Code 2000") ; math operators block

	;; (set-fontset-font "fontset-default" '(#x2190 . #x21FF) "STIX Two Math") ; arrows
	;; ;; (set-fontset-font "fontset-default" '(#x2190 . #x21FF) "DejaVu Sans Mono") ; arrows
	;; (set-fontset-font "fontset-default" '(#x1D400 . #x214F) "TeX Gyre") ; mathematical alphanumeric and letterlike symbols

	;; (set-fontset-font "fontset-default" '(#x2200 . #x22FF) "TeX Gyre") ; math operators block
	;; ;; (set-fontset-font "fontset-default" '(#x2200 . #x22FF) "Code 2000") ; math operators block




	;; (set-fontset-font "fontset-default" '(#x2336 . #x237A) "Code 2000") ; APL symbols
	;; ;; (set-fontset-font "fontset-default" '(#x2336 . #x237A) "APL333") ; APL symbols
	;; ;; (set-fontset-font "fontset-default" '(#x2336 . #x237A) "DejaVu Sans Mono") ; APL symbols
	;; (set-fontset-font "fontset-default" #x233D "Code 2000") ; ⌽
	;; (set-fontset-font "fontset-default" '(#x2190 . #x21FF) "Code 2000") ; arrows
	;; (set-fontset-font "fontset-default" '(#x1D400 . #x214F) "Code 2000") ; mathematical alphanumeric and letterlike symbols
	;; (set-fontset-font "fontset-default" '(#x2200 . #x22FF) "Code 2000") ; math operators block
	;; (set-fontset-font "fontset-default" '(#x22F2 . #x22FF) "Code 2000") ; ⋸ ⋶ ⋽ ⋻ etc
	;; (set-fontset-font "fontset-default" '(#x2308 . #x230B) "Code 2000") ; left/right floor/ceiling  FreeMono (corners), FreeSerif
	;; (set-fontset-font "fontset-default" #x222A "Code 2000") ; union
	;; (set-fontset-font "fontset-default" #x234b "Code 2000") ; ⍋
	;; (set-fontset-font "fontset-default" #x2352 "Code 2000") ; ⍒
	;; (set-fontset-font "fontset-default" #x2205 "Code 2000") ; empty set
	;; (set-fontset-font "fontset-default" '(#x22D8 . #x22D9) "Code 2000") ; ⋘ ⋙
	;; (set-fontset-font "fontset-default" '(#x22C0 . #x22C3) "Code 2000") ;; ⋀ ⋁ ⋂ ⋃  (medium-large)
	;; (set-fontset-font "fontset-default" '(#x2282 . #x2294) "Code 2000") ;; sub- / superset series
	;; (set-fontset-font "fontset-default" '(#x2070 . #x209C) "Code 2000") ; sub- & superscripts
	;; (set-fontset-font "fontset-default" #x2319 "Code 2000") ; ⌙
	;; (set-fontset-font "fontset-default" '(#x2A00 . #x2AFF) "Code 2000") ;; ⨁ ⨂ ⩽ ⩾ ⨌ etc
	;; (set-fontset-font "fontset-default" '(#x2234 . #x2235) "Code 2000") ;; ∴ ∵
	;; ;; (set-fontset-font "fontset-default" '(#x2234 . #x2235) "-PfEd-DejaVu Sans-bold-normal-normal-*-*-*-*-*-m-0-iso10646-1") ; ∴ ∵
	;; ;; (set-fontset-font "fontset-default" '(#x22C0 . #x22C3) "DejaVu Sans") ;; ⋀ ⋁ ⋂ ⋃
	;; ;; (set-fontset-font "fontset-default" '(#x22C0 . #x22C1) "FreeSans") ;; ⋀ ⋁
	;; ;; (set-fontset-font "fontset-default" '(#x22C2 . #x22C3) "DejaVu Sans") ;; ⋂ ⋃
	;; ;; (set-fontset-font "fontset-default" '(#x2A7D . #x2A7E) "DejaVu Sans") ;; ⩽ ⩾
	;; ;; (set-fontset-font "fontset-default" '(#x2A7D . #x2A7E) "TeX Gyre Bonum") ;; ⩽ ⩾




	;; ;; ----------- For minimal font set -------------------------------------------------------------
	;; ;; (set-fontset-font "fontset-default" '(#x2336 . #x237A) "DejaVu Sans Mono") ; APL symbols
	;; (set-fontset-font "fontset-default" #x233D "DejaVu Sans Mono") ; ⌽
	;; (set-fontset-font "fontset-default" '(#x2190 . #x21FF) "DejaVu Math TeX Gyre") ; arrows
	;; (set-fontset-font "fontset-default" '(#x1D400 . #x1D7FF) "DejaVu Math TeX Gyre") ; alphanumeric symbols
	;; (set-fontset-font "fontset-default" '(#x2100 . #x214F) "DejaVu Math TeX Gyre") ; letterlike symbols
	;; (set-fontset-font "fontset-default" '(#x2200 . #x22FF) "DejaVu Math TeX Gyre") ; math operators block
	;; (set-fontset-font "fontset-default" '(#x22F2 . #x22FF) "DejaVu Sans") ; ⋸ ⋶ ⋽ ⋻ etc
	;; (set-fontset-font "fontset-default" '(#x2308 . #x230B) "FreeSerif") ; left/right floor/ceiling  FreeMono (corners), FreeSerif
	;; (set-fontset-font "fontset-default" #x222A "DejaVu Serif") ; union
	;; (set-fontset-font "fontset-default" #x234b "DejaVu Sans Mono") ; ⍋
	;; (set-fontset-font "fontset-default" #x2352 "DejaVu Sans Mono") ; ⍒
	;; (set-fontset-font "fontset-default" #x2205 "Free Sans") ; empty set
	;; (set-fontset-font "fontset-default" '(#x22D8 . #x22D9) "DejaVu Math TeX Gyre") ; ⋘ ⋙
	;; (set-fontset-font "fontset-default" '(#x22C0 . #x22C3) "FreeSans") ;; ⋀ ⋁ ⋂ ⋃  (medium-large)
	;; (set-fontset-font "fontset-default" '(#x2282 . #x2294) "DejaVu Sans") ;; sub- / superset series
	;; (set-fontset-font "fontset-default" '(#x2070 . #x209C) "DejaVu Sans") ; sub- & superscripts
	;; (set-fontset-font "fontset-default" #x2319 "DejaVu Math TeX Gyre") ; ⌙
	;; (set-fontset-font "fontset-default" '(#x2A00 . #x2AFF) "DejaVu Sans") ;; ⨁ ⨂ ⩽ ⩾ ⨌ etc
	;; (set-fontset-font "fontset-default" '(#x2234 . #x2235) "DejaVu Sans") ;; ∴ ∵
	;; ;; (set-fontset-font "fontset-default" '(#x2234 . #x2235) "-PfEd-DejaVu Sans-bold-normal-normal-*-*-*-*-*-m-0-iso10646-1") ; ∴ ∵
	;; ;; (set-fontset-font "fontset-default" '(#x22C0 . #x22C3) "DejaVu Sans") ;; ⋀ ⋁ ⋂ ⋃
	;; ;; (set-fontset-font "fontset-default" '(#x22C0 . #x22C1) "FreeSans") ;; ⋀ ⋁
	;; ;; (set-fontset-font "fontset-default" '(#x22C2 . #x22C3) "DejaVu Sans") ;; ⋂ ⋃
	;; ;; (set-fontset-font "fontset-default" '(#x2A7D . #x2A7E) "DejaVu Sans") ;; ⩽ ⩾
	;; ;; (set-fontset-font "fontset-default" '(#x2A7D . #x2A7E) "TeX Gyre Bonum") ;; ⩽ ⩾



	;; ;; ----------- For proper font set -------------------------------------------------------------
	;; (set-fontset-font "fontset-default" '(#x2190 . #x21FF) "STIX Two Math") ; arrows
	;; (set-fontset-font "fontset-default" '(#x1D400 . #x1D7FF) "DejaVu Math TeX Gyre") ; alphanumeric symbols
	;; (set-fontset-font "fontset-default" '(#x2100 . #x214F) "DejaVu Math TeX Gyre") ; letterlike symbols
	;; (set-fontset-font "fontset-default" '(#x2200 . #x22FF) "STIX Two Math") ; math operators block
	;; (set-fontset-font "fontset-default" '(#x2336 . #x237A) "DejaVu Sans Mono") ; APL symbols
	;; (set-fontset-font "fontset-default" '(#x2336 . #x237A) "HanaMinA") ; APL symbols
	;; (set-fontset-font "fontset-default" #x2205 "Free Sans") ; empty set
	;; (set-fontset-font "fontset-default" '(#x2308 . #x230B) "FreeSerif") ; left/right floor/ceiling  FreeMono (corners), FreeSerif
	;; (set-fontset-font "fontset-default" '(#x2208 . #x2209) "DejaVu Math TeX Gyre") ; elem / notElem
	;; (set-fontset-font "fontset-default" '(#x220B . #x220C) "DejaVu Math TeX Gyre") ; contains / notContains
	;; (set-fontset-font "fontset-default" #x222A "DejaVu Serif") ; union
	;; (set-fontset-font "fontset-default" '(#x22C0 . #x22C3) "FreeSans") ;; ⋀ ⋁ ⋂ ⋃  (medium-large)
	;; (set-fontset-font "fontset-default" #x2319 "DejaVu Math TeX Gyre") ; ⌙
	;; (set-fontset-font "fontset-default" '(#x2A00 . #x2AFF) "DejaVu Sans") ;; ⨁ ⨂ ⩽ ⩾ ⨌ etc
	;; (set-fontset-font "fontset-default" '(#x2371 . #x2372) "STIX Two Math") ; ⍲ ⍱
	;; (set-fontset-font "fontset-default" '(#x2373 . #x237A) "DejaVu Sans Mono") ; ⍳ ⍴ ⍵ ⍶ ⍷ ⍸ ⍹ ⍺
	;; (set-fontset-font "fontset-default" #x2378 "HanaMinA") ; iota bar
	;; (set-fontset-font "fontset-default" #x2377 "HanaMinA") ; epsilon bar (alternate)
	;; (set-fontset-font "fontset-default" '(#x235A . #x235C) "DejaVu Sans Mono") ; ⍚ ⍛ ⍜
	;; (set-fontset-font "fontset-default" #x233D "DejaVu Sans Mono") ; ⌽
	;; (set-fontset-font "fontset-default" '(#x2234 . #x2235) "DejaVu Sans") ;; ∴ ∵
	;; (set-fontset-font "fontset-default" #x29FA "DejaVu Sans") ;; ⧺
	;; ;; (set-fontset-font "fontset-default" '(#x2234 . #x2235) "-PfEd-DejaVu Sans-bold-normal-normal-*-*-*-*-*-m-0-iso10646-1") ; ∴ ∵
	;; ;; (set-fontset-font "fontset-default" '(#x2190 . #x21FF) "FreeSans") ; arrows
	;; ;; (set-fontset-font "fontset-default" '(#x2190 . #x21FF) "FreeSerif") ; arrows
	;; ;; (set-fontset-font "fontset-default" '(#x2190 . #x21FF) "HanaMinA") ; arrows
	;; ;; (set-fontset-font "fontset-default" '(#x2190 . #x21FF) "DejaVu Math TeX Gyre") ; arrows
	;; ;; (set-fontset-font "fontset-default" '(#x2336 . #x237A) "STIX Two Math") ; APL symbols
	;; ;; (set-fontset-font "fontset-default" '(#x1D538 . #x1D56B) "STIX Two Math") ; double-stroke letters
	;; ;; (set-fontset-font "fontset-default" '(#x2100 . #x214F) "STIX Two Math") ; letterlike symbols
	;; ;; (set-fontset-font "fontset-default" '(#x2336 . #x234A) "HanaMinA") ; I-beam to up-tack underbar
	;; ;; (set-fontset-font "fontset-default" '(#x234F . #x2350) "HanaMinA") ; upwards vane, quad upwards arrow
	;; ;; (set-fontset-font "fontset-default" '(#x2356 . #x2357) "HanaMinA") ; downwards vane, quad downwards arrow
	;; ;; (set-fontset-font "fontset-default" '(#x2345 . #x2346) "HanaMinA") ; left / right vanes
	;; ;; (set-fontset-font "fontset-default" #x235A "HanaMinA") ; diamond underbar
	;; ;; (set-fontset-font "fontset-default" #x2360 "HanaMinA") ; quad colon
	;; ;; (set-fontset-font "fontset-default" #x2365 "HanaMinA") ; ⍥  circle diaeresis (large)
	;; ;; (set-fontset-font "fontset-default" '(#x236D . #x2370) "HanaMinA")
	;; ;; (set-fontset-font "fontset-default" #x235A "DejaVu Sans Mono") ; ⍚
	;; ;; (set-fontset-font "fontset-default" #x2364 "HanaMinA") ; ⍛
	;; ;; (set-fontset-font "fontset-default" #x235C "DejaVu Sans Mono") ; ⍜
	;; ;; (set-fontset-font "fontset-default" '(#x2376 . #x2379) "DejaVu Sans Mono") ; alpha bar, epsilon bar, iota bar, omega bar
	;; ;; (set-fontset-font "fontset-default" #x236B "DejaVu Sans Mono") ; del tilde      or STIX Two Math
	;; ;; (set-fontset-font "fontset-default" '(#x2366 . #x2369) "DejaVu Sans Mono") ; ⍦ ⍧ ⍨ ⍩
	;; ;; (set-fontset-font "fontset-default" #x2377 "DejaVu Sans Mono") ; epsilon bar
	;; ;; (set-fontset-font "fontset-default" #x2374 "DejaVu Sans") ; rho
	;; ;; (set-fontset-font "fontset-default" #x2375 "DejaVu Sans") ; omega
	;; ;; (set-fontset-font "fontset-default" #x237A "DejaVu Sans") ; alpha
	;; ;; (set-fontset-font "fontset-default" #x235B "FreeMono") ; jot underbar
	;; ;; (set-fontset-font "fontset-default" #x235F "STIX Two Math") ; circle star
	;; ;; (set-fontset-font "fontset-default" #x2378 "FreeMono") ; iota underbar
	;; ;; (set-fontset-font "fontset-default" '(#x2208 . #x2209) "HanaMinA") ; elem, notElem
	;; ;; (set-fontset-font "fontset-default" '(#x2206 . #x2207) "TeX Gyre Schola Math") ; delta, nabla
	;; ;; (set-fontset-font "fontset-default" '(#x2234 . #x2237) "STIX Math Two") ; therefore, because, ratio, proportion
	;; ;; (set-fontset-font "fontset-default" #x22C6 "DejaVu Math TeX Gyre") ; star operator (APL)
	;; ;; (set-fontset-font "fontset-default" #x220F "Liberation Serif") ; product
	;; ;; (set-fontset-font "fontset-default" #x2211 "Liberation Serif") ; sum
	;; ;; (set-fontset-font "fontset-default" #x2205 "STIX Math Two") ; empty set
	;; ;; (set-fontset-font "fontset-default" #x223C "STIX Math Two") ; tilde
	;; ;; (set-fontset-font "fontset-default" #x220D "STIX Math Two") ; small contains
	;; ;; (set-fontset-font "fontset-default" '(#x2260 . #x2262) "STIX Math Two") ; equal, NE, NEQ
	;; ;; (set-fontset-font "fontset-default" '(#x2282 . #x228B) "STIX Math Two") ; subset, superset, etc
	;; ;; (set-fontset-font "fontset-default" '(#x2295 . #x2298) "STIX Math Two") ; circled plus, circled times, etc
	;; ;; (set-fontset-font "fontset-default" #x236A "DejaVu Sans Mono") ; comma bar   --------------------------------------------------------------
	;; ;; (set-fontset-font "fontset-default" #x235F "DejaVu Sans Mono") ; circle star --------------------------------------------------------------
	;; ;; (set-fontset-font "fontset-default" #x235B "DejaVu Sans") ; jot underbar       ------------------------------------------------------------
	;; ;; (set-fontset-font "fontset-default" #x235C "DejaVu Sans") ; circle underbar    ------------------------------------------------------------
	;; ;; (set-fontset-font "fontset-default" #x235A "DejaVu Sans Mono") ; ⍚ ------------------------------------------------------------------------
	;; ;; (set-fontset-font "fontset-default" '(#x2363 . #x2364) "") ; ⍤ ⍣
	;; ;; (set-fontset-font "fontset-default" #x2229 "DejaVu Serif") ; intersection
	;; ;; (set-fontset-font "fontset-default" #x220A "FreeSans") ; small elem     FreeSans    TeX Gyre Schola Math   DejaVu Sans
	;; ;; (set-fontset-font "fontset-default" '(#x2282 . #x2294) "DejaVu Sans") ;; sub- / superset series
	;; ;; (set-fontset-font "fontset-default" '(#x22C0 . #x22C3) "DejaVu Sans") ;; ⋀ ⋁ ⋂ ⋃
	;; ;; (set-fontset-font "fontset-default" '(#x22C0 . #x22C1) "FreeSans") ;; ⋀ ⋁
	;; ;; (set-fontset-font "fontset-default" '(#x22C2 . #x22C3) "DejaVu Sans") ;; ⋂ ⋃
	;; ;; (set-fontset-font "fontset-default" '(#x22D8 . #x22D9) "DejaVu Sans") ; ⋘ ⋙
	;; ;; (set-fontset-font "fontset-default" #x2355 '("FreeMono" :weight bold)) ; ⍕  downtack jot
	;; ;; (set-fontset-font "fontset-default" #x234E "STIX Two Math") ; ⍎  uptack jot
	;; ;; (set-fontset-font "fontset-default" #x234b "DejaVu Sans Mono") ; ⍋
	;; ;; (set-fontset-font "fontset-default" #x2352 "DejaVu Sans Mono") ; ⍒
	;; ;; (set-fontset-font "fontset-default" '(#x2A7D . #x2A7E) "DejaVu Sans") ;; ⩽ ⩾



	;; quad up caret 2353 S2M
	;; down tack jot 2355 S2M
	;; up tack jot 234E S2M
	;; turned-not sign 2319 S2M
	;; (can't change diaeresis (#xA8) b/c it's already covered by Fira Code / Hasklig...)



	;; TODO: add ..  ...  [multi-line guards]
  ;; Font Ligatures
(use-package composite
  :defer t
  :init
  (defvar composition-ligature-table (make-char-table nil))
  :hook
  (((prog-mode conf-mode nxml-mode markdown-mode help-mode)
	. (lambda () (setq-local composition-function-table composition-ligature-table))))
  :config
  ;; support ligatures, some toned down to prevent hang (originally removed are 42-2, 43-2, 46-2, 47-2, 92)
  (when (version<= "27.0" emacs-version) ; when using 27.0 or newer
	(let ((alist
		   '((33 . ".\\(?:\\(==\\|[!=]\\)[!=]?\\)")
			 (35 . ".\\(?:\\(###?\\|_(\\|[(:=?[_{]\\)[#(:=?[_{]?\\)")
			 (36 . ".\\(?:\\(>\\)>?\\)")
			 (37 . ".\\(?:\\(%\\)%?\\)")
			 (38 . ".\\(?:\\(&\\)&?\\)")
			 (42 . ".\\(?:\\(\\*\\*\\|[*>]\\)[*>]?\\)")
			 ;; (42 . ".\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
			 (43 . ".\\(?:\\([>]\\)>?\\)")
			 ;; (43 . ".\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
			 (45 . ".\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
			 (46 . ".\\(?:\\(\\.<\\|[-=]\\)[-<=]?\\)")
			 ;; (46 . ".\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")
			 (47 . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
			 ;; (47 . ".\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
			 (48 . ".\\(?:\\(x[a-fA-F0-9]\\).?\\)")
			 (58 . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
			 (59 . ".\\(?:\\(;\\);?\\)")
			 (60 . ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
			 (61 . ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
			 (62 . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
			 (63 . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
			 (91 . ".\\(?:\\(|\\)[]|]?\\)")
			 ;; (92 . ".\\(?:\\([\\n]\\)[\\]?\\)")
			 (94 . ".\\(?:\\(=\\)=?\\)")
			 (95 . ".\\(?:\\(|_\\|[_]\\)_?\\)")
			 (119 . ".\\(?:\\(ww\\)w?\\)")
			 (123 . ".\\(?:\\(|\\)[|}]?\\)")
			 (124 . ".\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
			 (126 . ".\\(?:\\(~>\\|[-=>@~]\\)[-=>@~]?\\)"))))
	  (dolist (char-regexp alist)
		(set-char-table-range composition-ligature-table (car char-regexp)
							  `([,(cdr char-regexp) 0 font-shape-gstring]))))
	(set-char-table-parent composition-ligature-table composition-function-table))
  )



	;; (use-package fira-code-mode
  ;; :custom (fira-code-mode-disabled-ligatures '("[]" "x"))
  ;; :hook prog-mode
	;; )
  ;; (defun my-correct-symbol-bounds (prettify-symbols-alist)
  ;;   "Prepend a TAB character to each symbol in this alist,
  ;; this way compose-region called by prettify-symbols-mode
  ;; will use the correct width of the symbols
  ;; instead of the width measured by char-width."
  ;;   (mapcar (lambda (el)
  ;;             (setcdr el (string ?\t (cdr el)))
  ;;             el)
  ;;           prettify-symbols-alist))
  ;; (defun my-ligature-list (ligatures codepoint-start)
  ;;   "Create an alist of strings to replace with
  ;; codepoints starting from codepoint-start."
  ;;   (let ((codepoints (-iterate '1+ codepoint-start (length ligatures))))
  ;;     (-zip-pair ligatures codepoints)))
  ;; (setq my-fira-code-ligatures
  ;;       (let* ((ligs '("***" "*>" "*/"  "&&" "||"
  ;;                      "::" ":::" ":=" "!!" "!=" "!=="
  ;;                      "-->" "->" "->>" "-<" "-<<" "..." "?=" "/*"
  ;;                      "/**" "/=" "|>" "$>" "++" "+++" "+>" "=="
  ;;                      "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
  ;;                      ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
  ;;                      "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
  ;;                      "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
  ;;                      "<~~" "~@" "~=" "~>" "~~>" "*")))
  ;;         (my-correct-symbol-bounds (my-ligature-list ligs #Xe100))))
  ;; (defun my-set-fira-code-ligatures ()
  ;;   "Add fira code ligatures for use with prettify-symbols-mode."
  ;;   (setq prettify-symbols-alist
  ;;         (append my-fira-code-ligatures prettify-symbols-alist))
  ;;   (prettify-symbols-mode))
  ;; (when (version<= emacs-version "27.0") ; when using version lower than 27.0
	;;  (add-hook 'prog-mode-hook 'my-set-fira-code-ligatures))

	;; (global-pretty-mode t)
	;; (pretty-deactivate-groups
  ;;   ;; operator, equality and arrow groups interfere with Fira Code operators
	;;   ;; logic group interferes with prettify-symbols
  ;;	 '(:equality :ordering :ordering-double :ordering-triple
	;;						 :arrows :arrows-twoheaded :punctuation
	;;						 :logic :sets))
	;; (pretty-activate-groups
	;;  '(:sub-and-superscripts :greek :arithmetic-nary))



	;; (add-hook 'after-make-frame-functions (lambda (frame) (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Medium")))
	;; (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Medium")
	;; (add-hook 'prog-mode-hook #'ligatures-fira-code-setup)
	;; (add-hook 'prog-mode-hook 'global-prettify-symbols-mode)
	(remove-hook 'prog-mode-hook 'global-prettify-symbols-mode)
	(remove-hook 'prog-mode-hook 'global-pretty-mode)
	(remove-hook 'prog-mode-hook 'pretty-mode)
	;; (add-hook 'prog-mode-hook 'pretty-mode)
	;; (remove-hook 'prog-mode-hook 'prettify-symbols-mode)
	;; (add-hook 'prog-mode-hook 'prettify-symbols-mode)
	;; (add-hook 'prog-mode-hook 'fira-code-mode)
	(remove-hook 'prog-mode-hook 'fira-code-mode)
	;; (add-hook 'prog-mode-hook 'hasklig-mode)
	(remove-hook 'prog-mode-hook 'hasklig-mode)
	(add-hook 'purescript-mode-hook 'prettify-symbols-mode)
	(add-hook 'haskell-mode-hook 'prettify-symbols-mode)
	(add-hook 'racket-mode-hook 'prettify-symbols-mode)
	(add-hook 'lisp-mode-hook 'prettify-symbols-mode)
	(add-hook 'c-mode-hook 'prettify-symbols-mode)
	(add-hook 'prog-mode-hook 'toggle-word-wrap)
	(custom-set-variables
	 '(gnutls-algorithm-priority "normal:-vers-tls1.3"))


	(add-hook
	 'prog-mode-hook
	 (lambda ()
		 (mapc (lambda (pair) (push pair prettify-symbols-alist))
					 '(
						 ;; --- Syntax ---
						 ("define" .   "𝑓")
						 ;; ("defun" .    "𝑓")
						 ("define-type" . "Ƒ")
						 ("data" . "Ƒ")
						 ("type" . "Ƒ")
						 ;; ("::"    .    "∷")
						 ("ann"   .    "∷")
						 ("inst"   .    "∷")
						 ("cast"   .   "⭆")
						 ("for" .      "∀")
						 ;; ("for_" .      "∀")
						 ;; ("forM" .      "∀")
						 ;; ("forM_" .      "∀")
						 ("for-each" .      "∀")
						 ("\\" .      "λ")
						 ("lambda" .      "λ")



						 ;; ;; --- Functional ---
						 ;; ;; ("<=" .    "⩽")
						 ;; ;; (">=" .    "⩾")
						 ;; ;; ("list" .      "ε")
						 ;; ("exact-ceiling" .     "⌈")
						 ;; ("exact-floor" .     "⌊")
						 ;; ("take" .     "↑") ;
						 ;; ("drop" .     "↓") ;
						 ;; ("car" .     "⊃") ;
						 ;; ("¯" .     "⁻") ; high-minus (macron) → superscript -
						 ;; ("~" .     "∼") ; tilde → tilde operator
						 ;; ("cdr" .     "↓") ;
						 ;; ;; ("modulo" .      "%")
						 ;; ("modulo" .      "|")
						 ;; ;; ("mod" .      "%")
						 ;; ("mod" .      "|")
						 ;; ("abs" .      "|")
						 ;; ;; ("+" .    "∑")
						 ;; ("sum" .   "∑")
						 ;; ;; ("*" .    "∏")
						 ;; ("product" .    "∏") ; 𝚷
						 ;; ;; ("/" .    "÷")
						 ;; ;; ("sqrt" .    "√")
						 ;; ;; --- decent-readability-to-others separater ---
						 ;; ;; ("list-ref" .     "‼")
						 ;; ;; ("if" .     "?")
						 ;; ("let" .     "∴")
						 ;; ("let*" .     "∴")
						 ;; ;; ("where" .     "∵")
						 ;; ("cons" .     ":")
						 ;; ("head" .     "⊃")
						 ;; ("tail" .    "↓")
						 ;; ("first" .     "⊃")
						 ;; ("rest" .     "↓")
						 ;; ("take" .      "↑")
						 ;; ("drop" .      "↓")
						 ;; ("compose1" .  "∘")
						 ;; ;; ("<<<" .  "∘")
						 ;; ;; (">>>" .  "⋙")
						 ;; ;; ("compose" .  "⍛")
						 ;; ("log" .      "⍟")
						 ;; ("add1" .      "∆")
						 ;; ;; ("select" .      "σ")
						 ;; ("!!" .      "⊃")
						 ;; ("flip" .      "⍨")
						 ;; ("equal?" .      "≡")
						 ;; ("eq?" .      "≡")
						 ;; ("not-equal?" .      "≢")
						 ;; ;; ("map" .    "¨") ; ↦ ⮕ ⍈ ¨
						 ;; ;; ("fmap" .    "¨") ; ↥ ↦ ¨
						 ;; ("max" .    "⌈")
						 ;; ("min" .    "⌊")
						 ;; ("length" .      "⍴")
						 ;; ("reverse" .      "⌽")
						 ;; ("build-list" .      "⍳")
						 ;; ("range" .      "⍳")
						 ;; ("exact-floor" .    "⌊")
						 ;; ("exact-ceiling" .    "⌈")
						 ;; ("ceiling" .     "⌈")
						 ;; ("floor" .     "⌊")
						 ;; ("exact-floor" .    "⌋")
						 ;; ("exact-ceiling" .    "⌉")
						 ;; ;; ("ceiling" .     "⌉")
						 ;; ;; ("floor" .     "⌋")
						 ;; ;; ("foldl" .  "⮢")
						 ;; ;; ("foldr" .  "⮣")
						 ;; ("sort" .     "⍋")
						 ;; ("remove*" .      "∼")
						 ;; ("remove" .      "∼")
						 ;; ("apply" .   "↥")
						 ;; ("format" .    "⍕")
						 ;; ("eval" .    "⍎")
						 ;; ;; ("list-ref" .  "⊃")
						 ;; ("filter" .      "⌿")
						 ;; ;; ("concat" .    ",")
						 ;; ;; ("append" .      ",")
						 ;; ;; ("append*" .      ",")
						 ;; ;; ("string-append" .    ",")
						 ;; ;; ("string-append*" .    ",")
						 ;; ;; ("curry" .   "⫶") ; ⫶ ⋮ ⦙
						 ;; ;; ("compose1" .  "⋘")
						 ;; ;; ("compose" .  "⍛")
						 ;; ;; ("."  .   "∘") ; haskell infix use
						 ;; ;; ("powerset" .    "℘")
						 ;; ;; ("cross" .    "⨯")
						 ;; ;; ("del" .  "∇")
						 ;; ;; ("nabla" .  "∇")
						 ;; ;; ("-o" .  "⊸")
						 ;; ;; ("++" .      "◇")
						 ;; ;; ("<*>" .   "⊛")
						 ;; ;; ("mappend" .   "⊕")
						 ;; ;; ("`mappend`" .   "⊕")
						 ;; ("return" .   "⮩") ;⮩ ⮨ ⮡ ⤷ ⤶ ⏎ ⭅ ➥ ⟾ ⟻ ⟼ η
						 ;; ;; ("join" .   "⨝") ; µ ⨝
						 ;; ;; ("bind" .   "η")
						 ;; ;; ("yield" .   "⮨")
						 ;; ;; ("divide" .    "÷")
						 ;; ;; ("sub" .      "-")
						 ;; ;; ("product" .    "∏")
						 ;; ;; ("coproduct" .    "∐")

						 ;; --- Types ---
						 ("Any" .      "𝔸")
						 ("Integer" .      "ℤ")
						 ("Int" .      "ℤ")
						 ("Boolean" .      "𝔹")
						 ;; ("Bool" .      "𝔹")
						 ;; ("Char" .      "𝐂")
						 ("Char" .      "𝑪")
						 ("Float"  .  "ℝ")
						 ("Flonum"  .  "ℝ")
						 ("Complex"  .   "ℂ")
						 ("Number"  .   "ℝ")
						 ("Real"  .  "ℝ")
						 ("Natural"  .  "ℕ")
						 ("Index"  .  "𝕀")
						 ("Exact-Rational"  .  "ℚ")
						 ("String" .      "𝕊") ; 𝐒
						 ("Symbol" .      "𝑺")
						 ("#t" .     "𝑇")
						 ("#true" .     "𝑇")
						 ("true" .     "𝑇")
						 ("True" .     "𝑻")
						 ("False" .    "𝑭")
						 ("#f" .    "𝐹")
						 ("#false" .    "𝐹")
						 ("false" .    "𝐹")
						 ("Listof" .    "𝑳")
						 ("List" .    "𝑳")
						 ;; ("List" .    "𝗟")
						 ("Vectorof" .    "𝑽")
						 ("Vector" .    "𝑽")
						 ("Array" .    "𝑽") ; "Vector"
						 ;; ("Vector" .    "𝗩")
						 ("Pair" .    "⊕")
						 ("Pairof" .    "⊕")
						 ("Tuple" .    "⊕")
						 ("HashTable" .    "𝑯")
						 ("Option"  .  "𝑴") ; "Maybe"
						 ("Void" .   "∅")
						 ("All" .      "∀")
						 ("forall" .      "∀")
						 ;; ("int" .      "ℤ")
						 ;; ("Negative-Integer" .      "ℤ")
						 ;; ("Nonpositive-Integer" .      "ℤ")
						 ;; ("Nonnegative-Integer" .      "ℤ")
						 ;; ("Positive-Integer" .      "ℤ")
						 ;; ("Negative-Float"  .  "ℝ")
						 ;; ("Nonpositive-Float"  .  "ℝ")
						 ;; ("Nonnegative-Float"  .  "ℝ")
						 ;; ("Positive-Float"  .  "ℝ")
						 ;; ("Negative-Flonum"  .  "ℝ")
						 ;; ("Nonpositive-Flonum"  .  "ℝ")
						 ;; ("Nonnegative-Flonum"  .  "ℝ")
						 ;; ("Positive-Flonum"  .  "ℝ")
						 ;; ("Complex"  .   "ℂ")
						 ;; ("Negative-Real"  .  "ℝ")
						 ;; ("Nonpositive-Real"  .  "ℝ")
						 ;; ("Nonnegative-Real"  .  "ℝ")
						 ;; ("Positive-Real"  .  "ℝ")
						 ;; ("Negative-Exact-Rational"  .  "ℚ")
						 ;; ("Nonpositive-Exact-Rational"  .  "ℚ")
						 ;; ("Nonnegative-Exact-Rational"  .  "ℚ")
						 ;; ("Positive-Exact-Rational"  .  "ℚ")
						 ;; ("Set" .      "𝑺")
						 ;; ("Tuple" .    "⊗")
						 ;; ("HashTableTop" .    "𝐇")
						 ;; ("Immutable-HashTable" .    "𝑯")
						 ;; ("Mutable-HashTable" .    "𝑯")
						 ;; ("Hash" .    "𝑯")
						 ;; ("HashMap" .    "𝑯")
						 ;; ("True" .     "𝑻")
						 ;; ("#true" .     "𝑻")
						 ;; ("#false" .    "𝑭")
						 ;; ("Top" .      "⟙")
						 ("Bottom" .   "⟘")
						 ;; --- ADTs ---
						 ("Option"  .  "𝑴")
						 ("Maybe"  .  "𝑴")
						 ("Just"  .  "𝐽")
						 ("Nothing"  .  "𝑁")
						 ("Either"  .  "𝑬")
						 ("Right"  .  "𝑅")
						 ("Left"  .  "𝐿")
						 ;; --- Equality ---
						 ;; ("!=" .     "≠") ;
						 ;; ("<=" .      "⩽") ;≤ ⩽
						 ;; (">=" .      "⩾") ;≥ ⩾
						 ;; ("≤" .      "⩽") ;≤ ⩽
						 ;; ("≥" .      "⩾") ;≥ ⩾
						 ("not equal" . "≠")
						 ;; --- Predicate Logic ---
						 ("not" .      "⌙") ;; ¬
						 ("negate" .      "⌙")
						 ("and" .      "∧")
						 ("or" .      "∨")
						 ("xor" .      "⊻")
						 ("nor" .      "⊽")
						 ("nand" .      "⊼")
						 ("all" .      "∀")
						 ("any" .      "∃")
						 ("andmap" .      "∀")
						 ("ormap" .      "∃")
						 ("exists" .      "∃")
						 ("findf" .      "∃")
						 ("memf" .      "∃")
						 ("some" .      "∃")
						 ("none" .      "∄")
						 ;; --- Set Logic ---
						 ("empty" .      "∅")
						 ("'()" .      "∅")
						 ("'[]" .      "∅")
						 ("(set)" .      "∅")
						 ("null" .      "∅")
						 ("elem" .      "∈")
						 ("member" .      "∈")
						 ("not-member" .   "∉")
						 ("notElem" .   "∉")
						 ("U" .      "⋃")
						 ("in" .       "∈")
						 ("not in" .   "∉")
						 ;; ("element" .      "∈")
						 ("Union" .      "⋃")
						 ("union" .      "⋃")
						 ;; ("∪" .      "⋃")
						 ("Intersection" .      "⋂")
						 ("intersection" .      "⋂")
						 ;; ("∩" .      "⋂")
						 ;; ("complement" .      "∁")
						 ))))  ;ends add-hook for prettify-symbols-alist


(global-git-commit-mode t)

;; Disable mouse and arrow keys (not working... research how to do this)
(define-key evil-normal-state-map (kbd "<up>") nil)
(define-key evil-normal-state-map (kbd "<down>") nil)
(define-key evil-normal-state-map (kbd "<left>") nil)
(define-key evil-normal-state-map (kbd "<right>") nil)
;; Disable mouse globally
(global-disable-mouse-mode)
(define-key global-map (kbd "<mouse-1>") nil)
(global-unset-key (kbd "<mouse-1>"))
(define-key evil-normal-state-map (kbd "<mouse-2>") nil)
(define-key evil-normal-state-map (kbd "<down-mouse-2>") nil)
(define-key evil-normal-state-map (kbd "<mouse-3>") nil)
(define-key evil-normal-state-map (kbd "<down-mouse-3>") nil)
  (global-unset-key (kbd "<up>"))
  (global-unset-key (kbd "<right>"))
  (global-unset-key (kbd "<up>"))
  (global-unset-key (kbd "<down>"))
  (global-unset-key (kbd "<C-left>"))
  (global-unset-key (kbd "<C-right>"))
  (global-unset-key (kbd "<C-up>"))
  (global-unset-key (kbd "<C-down>"))
  (global-unset-key (kbd "<M-left>"))
  (global-unset-key (kbd "<M-right>"))
  (global-unset-key (kbd "<M-up>"))
  (global-unset-key (kbd "<M-down>"))


  (evil-define-motion vile-scroll-down (count)
	"Scroll down 10 lines."
	(setq count (or count 1))
	(evil-next-line (* 10 count)))


  (evil-define-motion vile-scroll-up (count)
	"Scroll up 10 lines."
	(setq count (or count 1))
	(evil-previous-line (* 10 count)))



	;; TODO: how to get ';' and ',' to work with this?
	;; TODO: how to get an alt. version of this to take 2 chars, like evil-snipe?
	(evil-define-motion vile-goto-word-by-first-char (count char)
		"Move to the next COUNT'th word beginning with CHAR.
		movement is restricted to the current line unless `evil-cross-lines' is non-nil.
		slightly modified version of evil-find-char."
		:type inclusive
		(interactive "<c><C>")
		(setq count (or count 1))
		(let ((fwd (> count 0))
					(visual (and evil-respect-visual-line-mode
											visual-line-mode)))
			(setq evil-last-find (list #'evil-find-char char fwd))
			(when fwd (forward-char))
			(let ((case-fold-search nil))
				(unless (prog1
										(re-search-forward (concat "[^[:alnum:]]" (char-to-string char)) ; modified this line
																		(cond (evil-cross-lines
																					nil)
																					((and fwd visual)
																					(save-excursion
																						(end-of-visual-line)
																						(point)))
																					(fwd
																					(line-end-position))
																					(visual
																					(save-excursion
																						(beginning-of-visual-line)
																						(point)))
																					(t
																					(line-beginning-position)))
																		t count)
									(when fwd (backward-char)))
					(user-error "Can't find %c" char)))))

;; backwards version
(evil-define-motion vile-goto-word-by-first-char-backward (count char)
  "Move to the previous COUNT'th word beginning with CHAR.
	movement is restricted to the current line unless `evil-cross-lines' is non-nil.
	slightly modified version of evil-find-char-reverse."
  :type exclusive
  (interactive "<c><C>")
  (vile-goto-word-by-first-char (- (or count 1)) char)
  (forward-char 1))

(defun shrink-window ()
	"Shrink the selected window by one line vertically."
	(interactive (enlarge-window -1)))


;; ;; replacement for ';'
;; (evil-define-motion vile-repeat-goto-word-by-first-char (count)
;;   "Repeat the last find COUNT times."
;;   :type inclusive
;;   (setq count (or count 1))
;;   (if evil-last-find
;;       (let ((cmd (car evil-last-find))
;;             (char (nth 1 evil-last-find))
;;             (fwd (nth 2 evil-last-find))
;;             evil-last-find) ; ? "let … in <var>" doesn't seem to make sense…
;;         ;; ensure count is non-negative
;;         (when (< count 0)
;;           (setq count (- count)
;;                 fwd (not fwd)))
;;         ;; skip next character when repeating t or T
;;         (and (eq cmd #'evil-find-char-to)
;;              evil-repeat-find-to-skip-next
;;              (= count 1)
;;              (or (and fwd (= (char-after (1+ (point))) char))
;;                  (and (not fwd) (= (char-before) char)))
;;              (setq count (1+ count))) ; ? what kind of stmt is "∧ cond1 cond2 …"? Nothing being defined nor executed?
;;         (funcall cmd (if fwd count (- count)) char)
;;         (unless (nth 2 evil-last-find)
;;           (setq evil-this-type 'exclusive)))
;;     (user-error "No previous search")))

;; ;; replacement for ','
;; (evil-define-motion vile-repeat-goto-word-by-first-char-reverse (count)
;;   "Repeat the last find COUNT times in the opposite direction."
;;   :type inclusive
;;   (vile-repeat-goto-word-by-first-char (- (or count 1)))) ; ??? but that fn seems to ignore neg. count anyway…




;; (defun vile-goto-word-by-first-char-unbounded (&optional count)
;;   "Move to the next COUNT'th word beginning with CHAR.
;; Slightly modified version of evil-find-char."
;;   (interactive)
;;   (setq count (or count 1))
;;   (let ((fwd (> count 0))
;;         (visual (and evil-respect-visual-line-mode
;;                      visual-line-mode)))
;;     (when fwd (forward-char))
;;     (let ((case-fold-search nil)
;;					(ks (evil-snipe--collect-keys 2)))
;;       (unless (prog1
;;                   (re-search-forward (concat "[^[:alnum:]]" ks) ; modified this line
;;                                   nil
;;                                   t count)
;;                 (when fwd (backward-char)))
;;         (user-error "Can't find %c" char)))))

;; ;; backwards version
;; (evil-define-motion vile-goto-word-by-first-char-backward-unbounded (count char1 char2)
;;   "Move to the previous COUNT'th word beginning with CHAR."
;;   :type exclusive
;;   (interactive "<c><C>")
;;   (vile-goto-word-by-first-char-unbounded (- (or count 1)) char1 char2)
;;   (forward-char 1))


;; (evil-define-motion vile-replace-from-register-0 ()
;;   "Replace word under cursor with previously yanked or deleted word." ; ciw <C-r> 0 b
;;   :type exclusive
;;   (interactive "<c><C>")
;;   (forward-char 1))


(evil-define-motion vile-backward-paragraph (count)
  "Move to the beginning of the COUNT-th previous paragraph."
  :jump t
	(evil-backward-paragraph)
	(evil-next-line))

(evil-define-motion vile-forward-paragraph (count)
  "Move to the beginning of the COUNT-th previous paragraph."
  :jump t
  (evil-forward-paragraph)
	(evil-previous-line))

(evil-define-operator vile-invert-char (beg end type)
  "Invert case of character."
  :motion evil-forward-char
  (if (eq type 'block)
      (evil-apply-on-block #'evil-invert-case beg end nil)
    (evil-invert-case beg end)))


;; (evil-define-motion vile-append-WORD (count) ; error somewhy
;;   "Jump to end of the word under the cursor and switch to insert mode"
;;	(evil-forward-WORD-end)
;;	(evil-append))


(defun open-typed-racket-docs ()
  (commandp)
	;; NixOS:
  (eww-browse-url "/nix/store/m90vpw02m80mqhw30i1cw4y3xhkfcldx-racket-7.8/share/doc/racket/ts-reference/index.html")
	;; Non-NixOS:
  ;; (eww-browse-url "file:///usr/share/doc/racket/ts-reference/index.html")
	)



(defun keymap+ (&rest bindings)
  (if (stringp (car bindings))
	  (progn (setq k (pop bindings) f (pop bindings))
			 (while k
			   (global-set-key (kbd k) f)
			   (setq k (pop bindings) f (pop bindings))))
	(let ((m (pop bindings)))
	  (setq k (pop bindings) f (pop bindings))
	  (while k
		(define-key m (kbd k) f)
		(setq k (pop bindings) f (pop bindings))))))




(defun avy-goto-word-curr-line ()
  "Jump to a word start on the current line only."
  (interactive)
  (avy-with avy-goto-word-0
	(avy-goto-word-0 nil (line-beginning-position) (line-end-position))))
;; optional evil integration example:
   ;; (declare-function 'avy-goto-word-curr-line "avy")
   ;; (evil-define-avy-motion avy-goto-word-curr-line inclusive)
	 ;; (define-key evil-motion-state-map (kbd "w") #'evil-avy-goto-word-curr-line)






;; leader keys mappings
(keymap+ evil-normal-state-local-map ;; c.f. (spacemacs/set-leader-keys (kdb "⍵") 'f) instead of <SPC ⍵>
         "SPC t w"   'toggle-word-wrap ; shadow toggle-whitespace
         "SPC t p"   'smartparens-mode
         "SPC t P"   'pretty-mode
         "SPC T t"   'nil ; disable toggle-tool-bar
         "SPC f S"   'write-file ; "save as"
         "SPC f A"   'evil-write-all ; save all open files
         "SPC b h"   'previous-buffer
         "SPC b l"   'next-buffer
         "SPC g m"   'magit-dispatch ; replace obsolete magit-dispatch-popup
         "SPC w g"   'enlarge-window
         "SPC w G"   'shrink-window
         "SPC i c"   'insert-char
         "SPC t s"   'prettify-symbols-mode
         "SPC k w"   'evil-window-up ; shadow evil-lisp-state-wrap binding
         "SPC j w"   'evil-window-down ; shadow evil-avy-goto-word-or-subword-1
         "SPC m w"   'spacemacs/toggle-maximize-buffer
         "SPC h w"   'helm-man-woman
         "SPC b H"   'spacemacs/home
         "SPC H k"  'describe-key
         "SPC H f"  'describe-function
         "SPC H c"  'describe-char
         "SPC H K"  'describe-key-briefly
         "SPC H v"  'describe-variable
         "SPC b b"  'helm-buffers-list ; was helm-mini
         "SPC t t"  'helm-buffers-list ; was helm-mini
         "SPC '"   'spacemacs/shell-pop-ansi-term
         "SPC W"   'whitespace-cleanup
         ;; "SPC '"   'spacemacs/shell-pop-eshell ; no aliases
         ;; "SPC '"   'spacemacs/shell-pop-shell ; no highlighting
         ;; "SPC T c"  'try-theme ;; mnemonic: "try colors"
         )

;; user prefix mappings
(define-prefix-command 'cm) ;; prefix key map, "cac's m"
(define-key evil-normal-state-map (kbd "m") cm)
(keymap+ cm
         "a"   'evil-set-marker
         "b"   'evil-scroll-line-to-bottom
         "e"   'vile-backward-paragraph
         ;; "f"    'avy-goto-word-curr-line ;; poor design, as it should take 2 chars——with only a single-char search, there are too many display results; TODO: instead write a fn that does evil-avy-goto-word-1-below, but restricts it to the current line, and searches in both directions.
         "j"   'evil-scroll-line-to-bottom
         "m"   'evil-scroll-line-to-center
         "k"   'evil-scroll-line-to-top
         ;; "t"   "zt3K3J"  ; not working somewhy
         "M"   'evil-scroll-line-to-bottom
         ;; "b"   "zb3J3K"  ; not working somewhy
         "r"   'vile-forward-paragraph
         "t"   'evil-scroll-line-to-top
         "n"   'org-timer-set-timer
         "N"   'toggle-timer-bell
         )

;; normal state mappings
;; ;; "SPC-;-<motion>" for e.g., comment-to-end-of-line (evilnc-comment-operator)
(keymap+ evil-normal-state-map
         ;; "C-d"    'vile-scroll-down
         ;; "C-u"    'vile-scroll-up
         ;; "C-f"    'evil-scroll-down
         ;; "C-b"    'evil-scroll-up
         ;; "~e"   'evil-forward-word-end ; temp key
         ;; "E"     "~ea" ; append at end of word
         ;; "~E"   'evil-forward-WORD-end ; temp key
         ;; "e"     "~Ea" ; append at end of WORD
         ;; "a"    'evil-append  ; default
         ;; "gd"   'racket-jump-visit-definition
         ;; "A"    'evil-insert-line
         "~"     nil ;; free-up prefix key -- shadows evil-invert-char
         "C-r"  nil ;; unmap C-r from unto-tree-redo
         "r"  'evil-replace  ; was originally evil-replace-state
         "R"  'undo-tree-redo
         "C-e"  'iedit-mode
         "C-;"  'evil-indent
         "\\"  'evil-switch-to-windows-last-buffer
         "s-;"  'evil-switch-to-windows-last-buffer
         "t"    'evil-find-char
         "T"    'evil-find-char-backward
         "J"    'vile-scroll-down
         "K"    'vile-scroll-up ;; often gets overridden by other modes due to non-determinism
         "w"    'evil-forward-WORD-begin
         "b"    'evil-backward-WORD-begin
         "W"    'evil-forward-word-begin
         "B"    'evil-backward-word-begin
         "e"  'evil-forward-WORD-end
         "E"  'evil-forward-word-end
         "f"   'vile-goto-word-by-first-char
         "F"    'vile-goto-word-by-first-char-backward
         "s"   'evil-avy-goto-word-1-below ; easymotion-style hints ; TODO replace with a vile 2-char version
         "S"   'evil-avy-goto-word-1-above ; easymotion-style hints ; TODO replace with a vile 2-char version
         "go"   'isearch-forward-word
         "gk"   'vile-invert-char
         "gc"   'evil-invert-case
         "gu"   'evil-upcase
         "gU"   'evil-downcase
         "("    'evil-backward-paragraph
         ")"    'evil-forward-paragraph
         "gh"   'evil-first-non-blank ;; for use with d,c,y, etc
         "ga"   'evil-digit-argument-or-evil-beginning-of-line
         "gl"   'evil-end-of-line
         "gi"   'evil-append-line
         "gI"   'evil-insert-resume
         "gd"   'spacemacs/jump-to-definition
         "gm"   'evil-jump-item
         "gn"   'spacemacs/enter-ahs-forward
         "gN"   'spacemacs/enter-ahs-backward
         "ZQ"   'kill-current-buffer
         "gr"   'racket-run
         "gR"   'racket-run-and-switch-to-repl
         "C-h"  'previous-buffer
         "C-l"  'next-buffer
         "C-m"  'spacemacs/evil-insert-line-below
         ;; "S-C-m"  'spacemacs/evil-insert-line-above  ; how to do Shift-Ctrl bindings?
         "C-."  'call-last-kbd-macro ; q for evil-record-macro
         "C-o"  'evil-jump-forward
         "C-i"  'evil-jump-backward
         "C-0"  'evil-jump-forward
         "C-9"  'evil-jump-backward
         "C-p"  'evil-paste-pop-next
         "C-j"  'evil-join
         "C-n"  'electric-newline-and-maybe-indent ;; split-line
         "C-k"  'spacemacs/evil-smart-doc-lookup
         "zm"    nil ;; disable close-folds function
         "zr"    nil ;; disable open-folds function
         "gy"    nil ;; disable spacemacs/copy-and-comment-lines
         ;; "mc"   "ciw <C-r> 0 <ESC>" ;; how to pass e.g., <C-r> or <ESC> to string macro?
         ;; "M-o"  'find-file-at-point
         ;; "g"    "cg"
         ;; "ZZ"   'save-modified-and-close-buffer ;; overrides evil-save-modified-and-close. i want to close only buffer
         ;; "gr"   'cider-load-buffer
         ;; "gR"   'spacemacs/cider-send-buffer-in-repl-and-focus
         ;; "M-d"   nil ;; disable kill-word
         ;; "C-d"   'evil-scroll-down
         ;; "C-u"   'evil-scroll-up
         ;; "C-f"   nil
         ;; "C-b"   nil
         ;; "C-p"   'replace-from-register-0
         )

;; visual-state mappings
(keymap+ evil-visual-state-map
		 "("  'evil-backward-paragraph
		 ")"  'evil-forward-paragraph
		 )

;; global mappings
(keymap+ "M-t"          'toggle-timer-bell
		 "<C-return>"   'shell
		 "<C-S-return>" 'eshell
		 ;; "C-x C-e"      'eval-print-last-sexp
		 ;; "C-x C-u"      'eval-last-sexp
		 ;; "M-+"          'text-scale-increase ;; error somewhy
		 ;; "M--"          'text-scale-decrease ;; error somewhy
		 ;; "M-="          (cmd (text-scale-set 0)) ;; error somewhy
		 )


;; ;; Global key bindings
;; (global-set-key (kbd "C-n") 'keyboard-quit)


;; key translation
(define-key key-translation-map (kbd "<f1>") (kbd "•")) ;; example





;; TODO: map gb -> toggle-between-this-buffer-and-most-recently-visited-buffer (write this function)


(setq-default evil-escape-key-sequence "fd") ;; key sequence to go from insert to normal mode, etc
(define-key evil-insert-state-map (kbd "C-n") 'evil-force-normal-state)
(setq-default evil-escape-delay 0.3)




(define-key evil-motion-state-map (kbd "C-,") 'evil-repeat-find-char-reverse) ;; why does setting "C-," in evil-normal-state-map make this not work when this is set as "C-,"? They're two different maps…


	;; ;; magit mode mappings
	;; (evil-define-key evil-magit-state magit-mode-map "?" 'evil-search-backward)
	;; (add-hook 'magit-log-mode-hook
	;;		  (lambda ()
	;;			(map (lambda (pair)
	;;				   (define-key magit-log-mode-map (kdb (car pair)) (cdr pair))))
	;;			'(
	;;			  ("K" . 'vile-scroll-up)
	;;			  ;; ("C-k" . 'magit-file-untrack)
	;;			  )))
	;; (add-hook 'magit-diff-mode-hook
	;;		  (lambda ()
	;;			(map (lambda (pair)
	;;				   (define-key magit-diff-mode-map (kdb (car pair)) (cdr pair))))
	;;			'(
	;;			  ("K" . 'vile-scroll-up)
	;;			  ;; ("C-k" . 'magit-file-untrack)
	;;			  )))



;; ;; ;; vimscript-like commands
;; ;;	TODO: write vile motion-command similar to "ds(" that, when executed inside a set of parenthesis, deletes everything only in the immediately-surrounding set of parentheses. Just like with evil-surround, have two options,one of which keeps the surrounding parentheses themselves, and one of which discards them. This is very helpful when you add e.g. a cast statement like
;; ;;		(cast (hash-ref stockmap stock) (Listof Candle))
	;; ;; except it's a longer expression that wraps to the next line, and you later want to remove the surrounding expression. See how evil-surround finds out what comprises the outer expr. Basically, the logic is to keep going forward, "collecting" parenthesis, until you have one more right-parentheses than left ones, and then delete that one. And do the same thing for left-parens, but while going backward.
	;; ;; `[(` and `])` move to previous/next paren, resp., even if it's on a different line.

;;	 (define-key evil-normal-state-map (kbd "mdl") "2[(dt(%ldf)") ;; doesn't work for above example, nor for multi-line exprs



(setq org-timer-default-timer 1)

(defun timer-bell () (call-process (car (file-expand-wildcards "/nix/store/6slsms782x2rzik7xa9fiyr39kg3jnkb-system-path/bin/cvlc")) nil nil nil "--play-and-exit" "/home/cameron/Music/takagi-ost/38 - Donyori.mp3"))

(defun toggle-timer-bell nil
	(interactive)
	(if (boundp 'org-timer-done-hook)
			(if (memql 'timer-bell org-timer-done-hook)
		  (progn (message "removed \"timer done\" bell") (remove-hook 'org-timer-done-hook 'timer-bell))
				(progn (message "added \"timer done\" bell") (add-hook 'org-timer-done-hook 'timer-bell)))
		(error "org-timer-done-hook does not exist. you need to start a timer first")))



(defmacro cmd (&rest c) "make a function a command by (interactive)" `(lambda () (interactive) ,@c))
(define-key evil-normal-state-map (kbd "ZZ")
	(cmd (when (buffer-modified-p)
				 (save-buffer))
			 (funcall 'kill-current-buffer)))


  (defadvice ido-find-file (after find-file-sudo activate) ;; doesn't work for counsel-find-file somewhy
	;; so use C-x C-f for ido-find-file
	"open file as root as needed."
	(unless (and buffer-file-name
				 (file-writable-p buffer-file-name))
	  (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))


  ;; ido everywhere
  ;; (setq ido-enable-flex-matching t)
  ;; (setq ido-everywhere t)

  ;; (ido-mode 1)

	(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
	(setq highlight-indent-guides-method 'character)


;; ;; enable easymotion bindings w/ given key prefix
;;	(evilem-default-keybindings ",")



	;; ;; Disable evil-snipe repeat keys 's' and 'S'
  ;; (setq evil-snipe-repeat-keys nil)

;; ;; Disable evil-snipe highlighting
;;   (setq evil-snipe-enable-highlight nil)
;;   (setq evil-snipe-enable-incremental-highlight nil)

  ;; (setq evil-snipe-override-local-mode nil)


	;; ;; replace evil-mode's f/F/t/T functionality with (1-character) sniping:
	;; (evil-snipe-override-mode 1)

	;; ;; enable snipe replacements of 'f' and 't'
	;; (setq-default dotspacemacs-configuration-layers
	;;							'((evil-snipe :variables evil-snipe-enable-alternate-f-and-t-behaviors t)))

	;; Disable evil search persistent highlighting
  (global-evil-search-highlight-persist 0)

  ;; ;; Disable evil-snipe-override for , and ; keys (doing "(define-key evil-snipe-mode-map (kbd ",") nil)" doesn't work )
  ;; (setq evil-snipe-override-evil-repeat-keys nil)
  ;; (setq evil-snipe-override-local-mode-map
	;;			(let ((map (make-sparse-keymap)))
	;;				(evil-define-key* 'motion map
	;;													"s" #'evil-snipe-s
	;;													"S" #'evil-snipe-S)
	;;				map))
  ;; (setq evil-snipe-parent-transient-map
	;;			(let ((map (make-sparse-keymap)))
	;;				(define-key map ";" #'evil-snipe-repeat)
	;;				(define-key map "," #'evil-snipe-repeat-reverse)
	;;				map))


	;; ;; Enable evil-snipe to do a longer n-char search using TAB
	;; (setq evil-snipe-tab-increment t)


	;; ;; doesn't seem to work; default is still 2 chars
	;; (setq-default evil-snipe--match-count 3)


	;; ;; Disable evil-snipe
	;; (evil-snipe-mode 0)


	;; (defun cac-toggle-fold ()
	;;	"Toggle fold all lines larger than indentation on current line"
	;;	(interactive)
	;;	(let ((col 1))
	;;		(save-excursion
	;;			(back-to-indentation)
	;;			(setq col (+ 1 (current-column)))
	;;			(set-selective-display
	;;			 (if selective-display nil (or col 1))))))
	;; (global-set-key (kbd "M-C-i") 'cac-toggle-fold)



  (setq tab-width 4)
	(setq-default indent-tabs-mode nil)
	(setq standard-indent 4)

;; Set indent levels
  (defun my-setup-indent (n)
	;; java/c/c++
	(setq c-basic-offset n)
	;; web development
	(setq coffee-tab-width n) ; coffeescript
	(setq javascript-indent-level n) ; javascript-mode
	(setq js-indent-level n) ; js-mode
	(setq js2-basic-offset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
	(setq web-mode-markup-indent-offset n) ; web-mode, html tag in html file
	(setq web-mode-css-indent-offset n) ; web-mode, css in html file
	(setq web-mode-code-indent-offset n) ; web-mode, js code in html file
	(setq css-indent-offset n) ; css-mode
	)
  (my-setup-indent 4) ;



;; ;; Fix conflicts between snipe and Magit buffers
;;   (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)



;; ;; NIGHT OWL THEME
;; ;; To style the non-selected ivy items:
;;   (defun night-owl/ivy-format-function-line (cands)
;;     "Transform CANDS into a string for minibuffer."
;;     (let ((str (ivy-format-function-line cands)))
;;       (font-lock-append-text-property 0 (length str) 'face 'ivy-not-current str)
;;       str))
;;   (setq ivy-format-function #'night-owl/ivy-format-function-line)
;; ;; To style spacemacs cursors:
;; (setq night-owl-evil-cursors
;;       '(("normal" night-owl-cursor box)
;;         ("insert" night-owl-cursor (bar . 2))
;;         ("emacs" night-owl-cursor (bar . 2))
;;         ("hybrid" night-owl-cursor (bar . 2))
;;         ("replace" night-owl-gray (hbar . 2))
;;         ("evilified" night-owl-yellow box)
;;         ("visual" night-owl-gray (hbar . 2))
;;         ("motion" night-owl-violet box)
;;         ("lisp" night-owl-red box)
;;         ("iedit" night-owl-magenta box)
;;         ("iedit-insert" night-owl-magenta (bar . 2))))
;; (defun night-owl-set-evil-cursor (state color shape)
;;   (set (intern (format "evil-%s-state-cursor" state))
;;        (list color shape)))
;; (defun night-owl-update-evil-cursors ()
;;   (let ((current-theme (symbol-name (car custom-enabled-themes))))
;;     (if (string-prefix-p "night-owl" current-theme)
;;         (cl-loop for (state color style) in night-owl-evil-cursors
;;                  do
;;                  (night-owl-set-evil-cursor state (symbol-value color) style))
;;       ;; not night-owl theme, restore default spacemacs cursors
;;       (if (functionp 'spacemacs/add-evil-cursor)
;;           (cl-loop for (state color shape) in spacemacs-evil-cursors
;;                    do (spacemacs/add-evil-cursor state color shape))))))
;; (add-hook 'after-init-hook #'night-owl-update-evil-cursors)
;; ;; For spacemacs (comment this out if not using spacemacs):
;; (add-hook 'spacemacs-post-theme-change-hook #'night-owl-update-evil-cursors)

  ;; (setq clojure-enable-fancify-symbols t)
  ;; (define-key evil-motion-state-map (down-mouse-1) 'silence)
  )
;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#19171c" "#be4678" "#2a9292" "#a06e3b" "#576ddb" "#955ae7" "#576ddb" "#8b8792"])
 '(avy-case-fold-search nil)
 '(compilation-message-face 'default)
 '(custom-safe-themes
   '("e8349abfd1c6513e40322ccce5652b5ef6b6665d3fed6e2d9447617c3cf35ee9" "702b04f42e51ad9889fd34b9fd065e79ec33f5fc4b17334cba9e49729a6d59d7" "65f35d1e0d0858947f854dc898bfd830e832189d5555e875705a939836b53054" "5c75e3fa3c2153e149bea54ef5324bdafea4e7ba9ae4b12314dd3ad13211e89e" "c2efd2e2e96b052dd91940b100d86885337a37be1245167642451cf6da5b924a" "6271fc9740379f8e2722f1510d481c1df1fcc43e48fa6641a5c19e954c21cc8f" "0feb7052df6cfc1733c1087d3876c26c66410e5f1337b039be44cb406b6187c6" "cba5ebfabc6456e4bbd68e0394d176161e1db063c6ca24c23b9828af0bdd7411" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "13880fa28757754bc40c85b05689c801ddaa877f2fe65abf1779f37776281ef1" "bbb521edff9940ba05aeeb49f9b247e95e1cb03bd78de18122f13500bda6514f" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(ensime-sem-high-faces
   '((var :foreground "#000000" :underline
          (:style wave :color "yellow"))
     (val :foreground "#000000")
     (varField :foreground "#600e7a" :slant italic)
     (valField :foreground "#600e7a" :slant italic)
     (functionCall :foreground "#000000" :slant italic)
     (implicitConversion :underline
                         (:color "#c0c0c0"))
     (implicitParams :underline
                     (:color "#c0c0c0"))
     (operator :foreground "#000080")
     (param :foreground "#000000")
     (class :foreground "#20999d")
     (trait :foreground "#20999d" :slant italic)
     (object :foreground "#5974ab" :slant italic)
     (package :foreground "#000000")
     (deprecated :strike-through "#000000")))
 '(evil-snipe-enable-highlight nil t)
 '(evil-snipe-enable-incremental-highlight nil t)
 '(evil-surround-pairs-alist
   '((48 "----
" . "
----")
     (109 "----" . "----")
     (113 "`" . "'")
     (80 "+++" . "+++")
     (112 "+" . "+")
     (68 "``" . "''")
     (100 "${" . "}")
     (108 "(" . ")")
     (76 "( " . " )")
     (115 "[" . "]")
     (40 "( " . " )")
     (83 "[ " . " ]")
     (67 "{ " . " }")
     (41 "(" . ")")
     (93 "[" . "]")
     (125 "{" . "}")
     (104 "#{" . "}")
     (72 "#{ " . " }")
     (117 "_" . "_")
     (98 "*" . "*")
     (66 "**" . "**")
     (105 "_" . "_")
     (73 "__" . "__")
     (59 "`" . "`")
     (58 "`" . "`")
     (99 "{" . "}")
     (67 "{ " . " }")
     (97 "<" . ">")
     (65 "< " . " >")
     (116 . evil-surround-read-tag)
     (60 . evil-surround-read-tag)
     (102 . evil-surround-function)))
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-color "#010F1D" t)
 '(global-evil-search-highlight-persist nil)
 '(global-xah-math-input-mode t)
 '(gnutls-algorithm-priority "normal:-vers-tls1.3")
 '(helm-completion-style 'emacs)
 '(highlight-changes-colors '("#EF5350" "#7E57C2"))
 '(highlight-tail-colors
   '(("#010F1D" . 0)
     ("#B44322" . 20)
     ("#34A18C" . 30)
     ("#3172FC" . 50)
     ("#B49C34" . 60)
     ("#B44322" . 70)
     ("#8C46BC" . 85)
     ("#010F1D" . 100)))
 '(indent-tabs-mode nil)
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   '(kbd-mode lua-mode evil-easymotion rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler inf-ruby j-mode gnu-apl-mode workgroups hlint-refactor hindent helm-hoogle haskell-snippets company-ghci haskell-mode company-cabal cmm-mode web-mode tagedit slim-mode scss-mode sass-mode pug-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data kaolin-themes vimrc-mode dactyl-mode nix-mode auto-sudoedit sudo-edit org-noter xah-math-input dyalog-mode zenburn-theme zen-and-art-theme white-sand-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme subatomic256-theme subatomic-theme spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme rebecca-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme organic-green-theme omtose-phellack-theme oldlace-theme obsidian-theme noctilux-theme naquadah-theme mustang-theme monokai-theme molokai-theme minimal-theme material-theme majapahit-theme madhat2r-theme lush-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatland-theme farmhouse-theme exotica-theme espresso-theme dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme anti-zenburn-theme ample-zen-theme flucui-dark-theme lab-dark-theme hasklig-mode pretty-mode sublime-themes solarized-theme occidental-theme moe-theme light-soap-theme lab-themes intellij-theme heroku-theme flucui-themes flatui-theme fira-code-mode color-theme-sanityinc-tomorrow color-theme-modern base16-theme apropospriate-theme ample-theme alect-themes afternoon-theme yapfify racket-mode pos-tip faceup pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode pythonic adoc-mode markup-faces xterm-color shell-pop multi-term helm-company helm-c-yasnippet fuzzy eshell-z eshell-prompt-extras esh-help company-statistics clojure-snippets auto-yasnippet ac-ispell auto-complete smeargle orgit magit-gitflow magit-popup helm-gitignore gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link evil-magit magit git-commit with-editor transient evil-snipe parinfer tldr disable-mouse atom-one-dark-theme underwater-theme night-owl-theme monochrome-theme web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor js2-mode js-doc coffee-mode psci purescript-mode psc-ide flycheck company dash-functional clj-refactor inflections edn multiple-cursors paredit yasnippet peg cider-eval-sexp-fu cider sesman queue parseedn clojure-mode parseclj a ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra lv hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile projectile pkg-info epl helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))
 '(pos-tip-background-color "#FFF9DC")
 '(pos-tip-foreground-color "#011627")
 '(psc-ide-add-import-on-completion t t)
 '(psc-ide-rebuild-on-save nil t)
 '(standard-indent 4)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#C792EA")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#FFEB95")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#F78C6C")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#7FDBCA")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#82AAFF")))
 '(vc-annotate-color-↦
   '((20 . "#C792EA")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#FFEB95")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#F78C6C")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#7FDBCA")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#82AAFF")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#011627" "#010F1D" "#DC2E29" "#EF5350" "#D76443" "#F78C6C" "#D8C15E" "#FFEB95" "#5B8FFF" "#82AAFF" "#AB69D7" "#C792EA" "#AFEFE2" "#7FDBCA" "#D6DEEB" "#FFFFFF")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(font-lock-comment-face ((t (:inherit italic :underline nil))))
 '(font-lock-constant-face ((t (:inherit italic :underline nil))))
 '(font-lock-keyword-face ((t (:inherit italic :underline nil :weight normal))))
 '(sp-show-pair-match-content-face ((t (:underline nil))) t))
