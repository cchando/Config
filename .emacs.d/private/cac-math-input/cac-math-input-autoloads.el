;;; cac-math-input-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "cac-math-input" "cac-math-input.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from cac-math-input.el

(defvar global-cac-math-input-mode nil "\
Non-nil if Global CAC-Math-Input mode is enabled.
See the `global-cac-math-input-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-cac-math-input-mode'.")

(custom-autoload 'global-cac-math-input-mode "cac-math-input" nil)

(autoload 'global-cac-math-input-mode "cac-math-input" "\
Toggle CAC-Math-Input mode in all buffers.
With prefix ARG, enable Global CAC-Math-Input mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

CAC-Math-Input mode is enabled in all buffers where
`cac-math-input-mode-on' would do it.
See `cac-math-input-mode' for more information on CAC-Math-Input mode.

\(fn &optional ARG)" t nil)

(autoload 'cac-math-input-mode-on "cac-math-input" "\
Turn on `cac-math-input-mode' in current buffer.

\(fn)" t nil)

(autoload 'cac-math-input-mode-off "cac-math-input" "\
Turn off `cac-math-input-mode' in current buffer.

\(fn)" t nil)

(autoload 'cac-math-input-mode "cac-math-input" "\
Toggle cac-math-input minor mode.

A mode for inputting a math and Unicode symbols.

Type “inf”, then press \\[cac-math-input-change-to-symbol] (or M-x `cac-math-input-change-to-symbol'), then it becomes “∞”.

Other examples:
 a → α
 p → π
 /= → ≠ or ne
 >= → ≥ or ge
 -> → → or rarr
 and → ∧
etc.

If you have a text selection, then selected word will be taken as input. For example, type 「extraterrestrial alien」, select the phrase, then press \\[cac-math-input-change-to-symbol], then it becomesJ👽.

For the complete list of abbrevs, call `cac-math-input-list-math-symbols'.

Decimal and hexadecimal can also be used. Example:

 945     ← decimal
 #945    ← decimal with prefix #
 &#945;  ← XML entity syntax

 x3b1    ← hexadimal with prefix x
 #x3b1   ← hexadimal with prefix #x
 &#x3b1; ← XML entity syntax


If you wish to enter a symbor by full unicode name but do not know the full name, M-x `insert'. Asterisk “*” can be used as a wildcard to find the char. For example, M-x `insert' , then type 「*arrow」 then Tab, then emacs will list all unicode char names that has “arrow” in it. (this feature is part of Emacs 23)

This package is a derivative of the xah-math-input MELPA package, which was made by Xah Lee.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cac-math-input" '("cac-math-input-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cac-math-input-autoloads.el ends here
