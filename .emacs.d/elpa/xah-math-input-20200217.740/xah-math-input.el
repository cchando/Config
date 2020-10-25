;;; xah-math-input.el --- a minor mode for inputting math and Unicode symbols. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2010-2020 by Xah Lee

;; Author: Xah Lee ( http://xahlee.info/ )
;; Version: 2.7.20200216233900
;; Package-Version: 20200217.740
;; Package-Commit: c1e72c4578a134e9aa3ec9ef425038d8c16fba94
;; Created: 08 Dec 2010
;; Package-Requires: ((emacs "24.1"))
;; Keywords: abbrev, convenience, unicode, math, LaTex
;; License: GPL v3
;; URL: http://ergoemacs.org/emacs/xmsi-math-symbols-input.html

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A minor mode for inputing math symbols and Unicode symbols.

;; M-x `global-xah-math-input-mode' to toggle on/off for all buffers.
;; M-x `xah-math-input-mode' to toggle on/off for current buffer.

;; In lisp code:
;; (global-xah-math-input-mode 1) ; turn on globally
;; (global-xah-math-input-mode 0) ; turn off globally
;; (xah-math-input-mode 1) or (xah-math-input-mode-on) ; turn on for current buffer
;; (xah-math-input-mode 0) or (xah-math-input-mode-off) ; turn off for current buffer

;; Type “inf”, then press 【Shift+Space】 `xah-math-input-change-to-symbol', then it becomes “∞”.
;; Other examples:

;; Other examples:
;;  a → α
;;  p → π
;;  /= → ≠ or ne
;;  >= → ≥ or ge
;;  -> → → or rarr
;;  and → ∧
;; etc.

;; M-x `xah-math-input-list-math-symbols' to see all abbrevs.

;; Home page: http://ergoemacs.org/emacs/xmsi-math-symbols-input.html

;;; Install:

;; Manual install.
;; To have emacs automatically load the file when it restarts, follow these steps:

;; Place the file in the dir 〔~/.emacs.d/lisp/〕. Create the folder if you don't have it.

;; Put the following lines in your emacs init file:
;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;; (require 'xah-math-input)
;; (global-xah-math-input-mode 1) ; activate the mode globally

;; References
;; http://xahlee.info/comp/unicode_index.html
;; http://xahlee.info/comp/unicode_math_operators.html
;; 〈How Mathematica does Unicode?〉 http://xahlee.info/math/mathematica_unicode.html



;;; Code:

(defvar xah-math-input-abrvs nil "A abbreviation hash table that maps a string to unicode char.")
(setq xah-math-input-abrvs (make-hash-table :test 'equal))

(defun xah-math-input--add-to-hash (@pairs)
  "Add @pairs to the hash table `xah-math-input-abrvs'.
@pairs is a sequence of pairs. Each element is a sequence of 2 items, [key, value]."
  (mapc
   (lambda (x) (puthash (elt x 0) (elt x 1) xah-math-input-abrvs))
   @pairs))


(xah-math-input--add-to-hash
 [
	;; ["AA" "𝔸"] ["BB" "𝔹"] ["CC" "ℂ"] ["DD" "𝔻"] ["EE" "𝔼"] ["FF" "𝔽"] ["GG" "𝔾"] ["HH" "ℍ"] ["II" "𝕀"] ["JJ" "𝕁"] ["KK" "𝕂"] ["LL" "𝕃"] ["MM" "𝕄"] ["NN" "ℕ"] ["OO" "𝕆"] ["PP" "ℙ"] ["QQ" "ℚ"] ["RR" "ℝ"] ["SS" "𝕊"] ["TT" "𝕋"] ["UU" "𝕌"] ["VV" "𝕍"] ["WW" "𝕎"] ["XX" "𝕏"] ["YY" "𝕐"] ["ZZ" "ℤ"]
	["AA" "𝔸"] ["BB" "𝔹"] ["CC" "ℂ"] ["DD" "𝔻"] ["EE" "𝔼"] ["FF" "𝔽"] ["GG" "𝔾"] ["HH" "ℍ"] ["ID" "𝕀"] ["I2" "𝕀"] ["JJ" "𝕁"] ["KK" "𝕂"] ["LL" "𝕃"] ["MM" "𝕄"] ["NN" "ℕ"] ["OO" "𝕆"] ["PP" "ℙ"] ["QQ" "ℚ"] ["RR" "ℝ"] ["SS" "𝕊"] ["TT" "𝕋"] ["UD" "𝕌"] ["U2" "𝕌"] ["VV" "𝕍"] ["WW" "𝕎"] ["XX" "𝕏"] ["YY" "𝕐"] ["ZZ" "ℤ"]
	;; ["dd" "ⅆ"] ["ee" "ⅇ"] ["ii" "ⅈ"] ["jj" "ⅉ"]
	["dd" "ⅆ"] ["ee" "ⅇ"] ["id" "ⅈ"] ["i2" "ⅈ"] ["jj" "ⅉ"]
	;; ["N" "ℕ"]
	;; ["integer" "ℤ"]
	;; ["rational" "ℚ"]
	;; ["Q" "ℚ"]
	;; ["real" "ℝ"]
	;; ["R" "ℝ"]
	;; ["C" "ℂ"]
	["quaternion" "ℍ"]
	["quat" "ℍ"]
	;; ["H" "ℍ"]
	["sedenion" "𝕊"]
	;; ["S" "𝕊"]
	])






(xah-math-input--add-to-hash
 '(

	 ;; personal
	 ["cam" "Cameron"]
	 ["cac" "Cameron Chandoke"]

    ;; internet abbrev
    ["afaik" "as far as i know"]
    ["atm" "at the moment"]
    ["dfb" "difference between"]
    ["ty" "thank you"]
    ["ui" "user interface"]
    ["und" "understand"]
    ["ur" "you are"]
    ["btw" "by the way"]
    ["ie" "i.e.,"]
    ["eg" "e.g.,"]

    ["cant" "can't"]
    ["didnt" "didn't"]
    ["dont" "don't"]

    ;; english word abbrev
    ["ann" "announcement"]
    ["arg" "argument"]
    ["autom" "automatic"]
    ["bc" "because"]
    ["bg" "background"]
    ["bt" "between"]
    ["math" "mathematics"]

    ;; computing
    ["ahk" "AutoHotkey"]
    ["cfg" "context-free grammar"]
    ["cj" "Clojure"]
    ["csi" "computer science"]

    ;; tech company
    ["gc" "Google Chrome"]
    ["gm" "Google Map"]
    ["macos" "Mac OS"]
    ["msw" "Microsoft Windows"]

    ;; programing
    ["ev" "environment variable"]
    ["ipa" "IP address"]
    ["jvm" "Java Virtual Machine"]
    ["rsi" "repetitive-strain injury"]
    ["subdir" "sub-directory"]
    ["subf" "sub-folder"]
    ["wd" "web development"]

    ["db" "database"]
    ["guif" "graphical user interface"]
    ["gui" "GUI"]
    ["oopf" "object oriented programing"]
    ["oop" "OOP"]

    ["osf" "operating system"]
    ["os" "OS"]

    ;; programming
    ["eqe" "=="]
    ["ret" "return"]
    ["utf8" "-*- coding: utf-8 -*-"]

		;; catch typos
    ["pwoer" "power"]

    ;; regex
		;; digits
		["xAZ" "\\([A-Za-z0-9]+\\)"]
    ["xPhone" "/^\b\d{3}[-.]?\d{3}[-.]?\d{4}\b$/"]
    ["xWholenums" "/^\d+$/"]
    ["xDec" "/^\d*\.\d+$/"]
    ["xWhole+dec" "/^-?\d*(\.\d+)?$/"]
    ["xWhole+dec+frac" "/[-]?[0-9]+[,.]?[0-9]*([\/][0-9]+[,.]?[0-9]*)*/"]
		;; alphanumeric
    ["xAN-nospace" "/^[a-zA-Z0-9]*$/"]
    ["xAN" "/^[a-zA-Z0-9 ]*$/"]
		;; email
    ["xEmail" "/^([a-z0-9_\.-]+)@([\da-z\.-]+)\.([a-z\.]{2,5})$/"]
    ["xEmailc" "/^([a-zA-Z0-9._%-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,6})*$/"]
    ["xEmailu" "/^([a-z0-9_\.\+-]+)@([\da-z\.-]+)\.([a-z\.]{2,6})$/"]
		;; Time Format HH:MM 12-hour, optional leading 0
    ["xHH:MM-12h-0o" "/^(0?[1-9]|1[0-2]):[0-5][0-9]$/"]
		;; Time Format HH:MM 12-hour, optional leading 0, Meridiems [AM/PM]
    ["xHH:MM-12h-0o-AP" "/((1[0-2]|0?[1-9]):([0-5][0-9]) ?([AaPp][Mm]))/"]
		;; match duplicates in a string
    ["xStringdups" "/(\b\w+\b)(?=.*\b\1\b)/"]
		;; file path w/ filename & extension
    ["xPathNameExt" "/((\/|\\|\/\/|https?:\\\\|https?:\/\/)[a-z0-9 _@\-^!#$%&+={}.\/\\\[\]]+)+\.[a-z]+$/"]
		;; file path w/ optional filename, extension
    ["xPathonamEext" "/^(.+)/([^/]+)$/"]
		;; file name w/ 3-char extension
    ["xFileNameExt3" "/^[\w,\s-]+\.[A-Za-z]{3}$/"]
		;; social security number
		;; can use either hypen(-) or space( ) character as separator
    ["xSSN" "/^((?!219-09-9999|078-05-1120)(?!666|000|9\d{2})\d{3}-(?!00)\d{2}-(?!0{4})\d{4})|((?!219 09 9999|078 05 1120)(?!666|000|9\d{2})\d{3} (?!00)\d{2} (?!0{4})\d{4})|((?!219099999|078051120)(?!666|000|9\d{2})\d{3}(?!00)\d{2}(?!0{4})\d{4})$/"]
		;; passport
    ["xPassport" "/^[A-PR-WY][1-9]\d\s?\d{4}[1-9]$/"]



    ;; misc. unicode
    ["mda" "—"]
    ["nda" "–"]
    ["dlim" "-----------------------------------------------------------------------------------"]
    ["bul" "•"]
    ["aub" "↥"]
    ["arb" "↦"]
    ["alb" "↤"]
    ["adb" "↧"]



    ;; Mathematics
    ["U" "∪"]
    ["I" "∩"]
    ["inter" "∩"]
    ["UU" "⋃"]
    ["aU" "⋃"]
    ["II" "⋂"]
    ["aI" "⋂"]
    ["C" "⊂"]
    ["C_" "⊆"]
    ["Cl" "⊃"]
    ["C_l" "⊇"]
    ["Cn" "⊄"]
    ["nC" "⊄"]
    ["Cnl" "⊅"]
    ["C_nl" "⊉"]
    ["sc" "∁"]
    ["comp" "∁"]
    ["scom" "∁"]
    ["\\" "∖"]
    ["setm" "∖"]
    ["smin" "∖"]
    ["fora" "∀"]
    ["All" "∀"]
    ;; ["all" "∀"]
    ["exi" "∃"]
    ["any" "∃"]
    ["some" "∃"]
    ["nex" "∄"]
    ["none" "∄"]
    ["in" "∈"]
    ["nin" "∉"]
    ["con" "∋"]
    ["cont" "∋"]
    ["ncon" "∌"]
    ["j" "∘"] ; jot (composition)
    ["and" "∧"]
    ["or" "∨"]
    ["nor" "⊽"]
    ["nand" "⊼"]
    ["xor" "⊻"]
    ["Or" "⋁"]
    ["OR" "⋁"]
    ["AND" "⋀"]
    ["let" "∴"]
    ["where" "∵"]
    ["wh" "∵"]
    ["em" "∅"]
    ["emp" "∅"]
    ["cro" "⨯"]
    ["le" "⩽"]
    ["gr" "⩾"]
    ["le'" "≤"]
    ["ge'" "≥"]
    ["not" "¬"]
    ["nega" "⌙"]
    ["abs" "∣"]
    ["neq" "≠"]
    ["S" "∑"]
    ["P" "∏"]
    ["cop" "∐"]
    ["acop" "⨿"]
    ["G" "𝚪"]
    ["l" "λ"]
    ["pi" "𝛑"]
    ["eul" "ℯ"]
    ["eul'" "ℇ"]
		["planck" "ℎ"]
		["pla" "ℎ"]
    ["g" "𝛄"]
    ["est" "℮"]
    ["fourier" "ℱ"]
    ["ft" "ℱ"]
    ["int" "∫"]
    ["ii" "∬"]
    ["iii" "∭"]
    ["iiii" "⨌"]
    ["io" "∮"]
    ["iio" "∯"]
    ["iiio" "∰"]
    ["icl" "∱"]
    ["iacl" "⨑"]
    ["iocl" "∲"]
    ["ioacl" "∳"]
    ["intf" "⨍"]
    ["prec" "≺"]
    ["succ" "≻"]
    ["prer" "⊰"]
    ["sucr" "⊱"]
    ["pro" "∷"]
    ["prop" "∝"]
    ["inf" "∞"]
    ["o+" "⊕"]
    ["o--" "⊖"]
    ["o*" "⊛"]
    ["od" "⨸"]
    ["o/" "⊘"]
    ["cir+" "⊕"]
    ["cir-" "⊕"]
    ["cirx" "⊗"]
    ["cir*" "⊛"]
    ["cird" "⨸"]
    ["cir/" "⊘"]
    ;; ["maps" "↦"]
    ["bagl" "⟅"]
    ["bagr" "⟆"]
    ["inbag" "⋿"]
		["power" "℘"]
    ["o-" "⟜"]
    ["-o" "⊸"]
    ["o-o" "⧟"]
    ;; ["cur" "⫶"] ; curry



		;; ;; APL
    ;; ["al" "←"]
    ;; ["ar" "→"]
    ;; ["au" "↑"]
    ;; ["ad" "↓"]
    ;; ["=" "←"]
    ;; ["take" "↑"]
    ;; ["tak" "↑"] ; take
    ;; ["ta" "↑"] ; take
    ;; ["mix" "↑"]
    ;; ["drop" "↓"]
    ;; ["dro" "↓"] ; drop
    ;; ["dr" "↓"] ; drop
    ;; ["spl" "↓"] ; split
    ;; ["head" "⊃"]
    ;; ["pick" "⊃"]
    ;; ["encl" "⊂"] ; enclose
    ;; ["paen" "⊂"] ; partitioned enclose
    ;; ["nest" "⊆"]
    ;; ["part" "⊆"] ; partition
    ;; ["no" "~"] ; not
    ;; ["wo" "~"] ; without
    ;; ["lv" "⍅"]
    ;; ["rv" "⍆"]
    ;; ["uv" "⍏"]
    ;; ["dv" "⍖"]
    ;; ["div" "÷"]
    ;; ["x'" "×"]
    ;; ["-" "−"]
    ;; ["log" "⍟"]
    ;; ["eq" "≡"]
    ;; ["dep" "≡"] ; depth
    ;; ["mat" "≡"] ; match
    ;; ["neq" "≢"]
    ;; ["til" "∼"]
    ;; ["p" "⍴"] ; rho
    ;; ["r" "⍴"] ; rho
    ;; ["w" "⍵"] ; omega
    ;; ["w_" "⍹"]
    ;; ["i" "⍳"] ; iota
    ;; ["i_" "⍸"]
    ;; ["a" "⍺"] ; alpha
    ;; ["a_" "⍶"]
    ;; ["enl" "∊"] ; enlist
    ;; ["en" "∊"] ; enlist
    ;; ["es" "∊"]
    ;; ["se" "∊"]
    ;; ["find" "⋸"]
    ;; ["e_" "⋸"]
    ;; ["in_" "⋸"]
    ;; ["flr" "⌊"] ; floor
    ;; ["min" "⌊"]
    ;; ["cei" "⌈"] ; ceiling
    ;; ["max" "⌈"]
		;; ["lcm" "∧"] ; lowest common denomenator
		;; ["gcd" "∨"] ; greatest common divisor
		;; ["conj" "+"] ; conjugate
		;; ["neg" "-"] ; negate
		;; ["nega" "-"] ; negate
		;; ["dire" "×"] ; direction
		;; ["dir" "×"] ; direction
    ;; ["mag" "∣"] ; magnitude
    ;; ["norm" "∣"] ; magnitude
    ;; ["res" "∣"] ; residue
    ;; ["inc" "∆"] ; increment
    ;; ["delta" "∆"]
    ;; ["del" "∇"]
    ;; ["nab" "∇"] ; nabla
    ;; ["j_" "⍛"]
    ;; ["enc" "⊤"] ; encode
    ;; ["enco" "⊤"] ; encode
    ;; ["dec" "⊥"] ; decode
    ;; ["deco" "⊥"] ; decode
    ;; ["left" "⊣"]
    ;; ["lef" "⊣"] ; left
    ;; ["righ" "⊢"] ; right
    ;; ["ri" "⊢"] ; right
    ;; ["rig" "⊢"] ; right
    ;; ["mdv" "⌹"] ; matrix divide
    ;; ["min" "⌹"] ; matrix inverse
    ;; ["minv" "⌹"] ; matrix inverse
    ;; ["o" "○"] ; APL pi-times (not composition)
    ;; ["trig" "○"] ; trigonometric functions
    ;; ["o_" "⍜"]
    ;; ["ro" "⌽"]
    ;; ["rot" "⌽"]
    ;; ["rev" "⌽"]
    ;; ["rof" "⊖"]
    ;; ["tra" "⍉"]
    ;; ["xf" "⍀"] ; expand 1st
    ;; ["sf" "⍀"] ; scan 1st
    ;; ["rf" "⌿"] ; [replicate / reduce] 1st
    ;; ["ref" "⌿"] ; [replicate / reduce] 1st
    ;; ["repf" "⌿"] ; replicate 1st
    ;; ["redf" "⌿"] ; reduce 1st
    ;; ["gu" "⍋"]
    ;; ["gd" "⍒"]
    ;; ["gru" "⍋"]
    ;; ["grd" "⍒"]
    ;; ["sou" "⍋"]
    ;; ["sod" "⍒"]
    ;; ["nn" "¯"]
    ;; ["zil" "⍬"]
    ;; [",_" "⍪"]
    ;; ["fmt" "⍕"]
    ;; ["exc" "⍎"]
    ;; ["at" "@"]
    ;; ["IB" "⌶"]
    ;; ["Ib" "⌶"]
    ;; ["ib" "⌶"]
    ;; ["map" "¨"]
    ;; ["each" "¨"]
    ;; ["dia" "¨"]
    ;; ["pwr" "⍣"]
    ;; ["strd" "⍣"]
    ;; ["flip" "⍨"]
    ;; ["smrk" "⍨"]
    ;; ["rank" "⍤"]
    ;; ["hoot" "⍤"]
    ;; ["sep" "◇"] ; statement separator
    ;; ["dia" "◇"]
    ;; ["holl" "⍥"]
    ;; ["vari" "⍠"] ; variant
    ;; ["opt" "⍠"] ; option
    ;; ["quc" "⍠"]
    ;; ["qua" "⎕"]
    ;; ["sys" "⎕"]
    ;; ["mat" "⌷"] ; materialize
    ;; ["ind" "⌷"] ; index
    ;; ["squ" "⌷"]
    ;; ["key" "⌸"]
    ;; ["que" "⌸"]
    ;; ["sten" "⌺"]
    ;; ["qudi" "⌺"]
    ;; ["com" "⍝"]
    ;; ["c" "⍝"]


		;; Haskell
		("fmap" "↥")
		;; ("fmap" "↦")



		;; Racket
    ("t" "#t" )
    ("f" "#f" )
    ["flr" "exact-floor"]
    ["cei" "exact-ceiling"]
    ["flrr" "floor"] ; result is Real
    ["flrs" "floor"] ; result is Real
    ["ceir" "ceiling"] ; result is Real
    ["ceis" "ceiling"] ; result is Real
    ["cur" "curry"] ; curry
    ["fil" "filter"] ; filter
    ["fno" "filter-not"] ; filter-not
    ["repl" "replicate"]
    ["cat" "concat"]
    ["ap" "append"]
    ["zipw" "zip-with"]
    ["zw" "zip-with"]
    ["ir" "in-range?"]
    ["csae" "case"] ; catch typo
    ["casea" "case->"]
    ["csaea" "case->"] ; catch typo
    ["casel" "case-λ"]
    ["csael" "case-λ"] ; catch typo
    ["lsit" "list"] ; catch typo
    ["let" "let*"]
    ["for" "∀:"]
    ["forn" "∀*:"] "nested"
    ["forl" "∀:l"]
    ["fornl" "∀*:l"]
    ["forh" "∀:h"]
    ["fornh" "∀*:h"]
    ["forv" "∀:v"]
    ["fornv" "∀*:v"]
    ["fors" "∀:∑"]
    ["forns" "∀*:∑"]
    ["forp" "∀:∏"]
    ["fornp" "∀*:∏"]
    ["forse" "∀:s"]
    ["fornse" "∀*:s"]
    ["forhe" "∀:hq"]
    ["fornhe" "∀*:hq"]
    ["forhv" "∀:hv"]
    ["fornhv" "∀*:hv"]
    ["foro" "∀:or"]
    ["forno" "∀*:or"]
    ["foran" "∀:and"]
    ["fornan" "∀*:∧"]
    ["forfi" "∀:1st"]
    ["fornfi" "∀*:1st"]
    ["forla" "∀:last"]
    ["fornla" "∀*:last"]
    ["forls" "∀:lists"]
    ["fornls" "∀*:lists"]
    ["forfo" "∀:↰"]
    ["fornfo" "∀*:↰"]
    ["fll" "foldl"]
    ["frr" "foldr"]
    ["eq" "equals?"]
    ["neq" "not-equals?"]
    ["til" "∼"]
    ["p" "length"] ; rho
    ["r" "length"] ; rho
    ["i" "build-list"] ; iota
    ;; ["i_" "⍸"]
    ["es" "∊"]
    ;; ["e_" "⋸"]
    ["j" "compose1"]
    ["j_" "compose"]
    ["rev" "reverse"]
    ["rot" "reverse"]
    ["gu" "sort"]
    ["sou" "sort"]
    ["gru" "sort"]
    ["sou" "sort"]
    ["fmt" "format"]
    ["exc" "eval"]
    ["at" "@"]
    ;; ["IB" "⌶"]
    ;; ["Ib" "⌶"]
    ;; ["ib" "⌶"]
    ["map" "¨"]




    ;; --- Types ---
		;; If it's not parameterized (Integer, Natural, Boolean, etc), it has a double-struck letter.
		;; If it is parameterized, it has a bold letter.
		;;     Normal parameterized types (Listof, Vectorof, Pairof, HashTable, etc) are slanted.
		;;     Fixed parameterized types akin to tuples (List, Vector, etc) are upright
		;; The exception is Symbol, which is bold/italic, which String is double-struck.

    ["N" "ℕ"]
    ["IN" "𝐈"]
    ["In" "𝐈"]
    ["Nu" "ℂ"]
    ["Zn" "ℤ⁻"]
    ["Zn0" "ℤ⁰⁻"]
    ["Z" "ℤ"]
    ["Zp0" "ℤ⁰⁺"]
    ["Zp" "ℤ⁺"]
    ["L" "𝑳"]
    ["L'" "𝗟"]
    ["V" "𝑽"]
    ["V'" "𝗩"]
    ;; ["Pa" "Pair"]
    ["Pa" "𝐏"]
    ;; ["Pa" "𝑷"]
    ["M" "𝑴"]
    ["O" "𝑴"]
    ["Rn" "ℝ⁻"]
    ["Rn0" "ℝ⁰⁻"]
    ["R" "ℝ"]
    ["Rp0" "ℝ⁰⁺"]
    ["Rp" "ℝ⁺"]
    ["Fln" "Fl⁻"]
    ["Fln0" "Fl⁰⁻"]
    ["Fl" "Fl"]
    ["Flp0" "Fl⁰⁺"]
    ["Flp" "Fl⁺"]
    ["T" "𝑻"]
    ["F" "𝑭"]
    ["B" "𝔹"]
    ["str" "𝕊"]
    ["STR" "𝕊"]
    ["sy" "𝑺"]
    ["SY" "𝑺"]
    ["H" "𝑯"]
    ["Hi" "𝑯i"]
    ["Hm" "𝑯m"]
    ["A" "𝔸"]
    ["Qn" "ℚ⁻"]
    ["Qn0" "ℚ⁰⁻"]
    ["Q" "ℚ"]
    ["Qp0" "ℚ⁰⁺"]
    ["Qp" "ℚ⁺"]
    ["L^" "𝑳^"]
    ["JS" "JSExpr"]
    ["JH" "JSHash"]

    ;; ["N" "𝐍"]
    ;; ["IN" "𝐈"]
    ;; ["In" "𝐈"]
    ;; ["Nu" "ℂ"]
    ;; ["Zn" "𝐙⁻"]
    ;; ["Zn0" "𝐙⁰⁻"]
    ;; ["Z" "𝐙"]
    ;; ["Zp0" "𝐙⁰⁺"]
    ;; ["Zp" "𝐙⁺"]
    ;; ["L" "𝑳"]
    ;; ["L'" "𝗟"]
    ;; ["V" "𝑽"]
    ;; ["V'" "𝗩"]
    ;; ["Pa" "Pair"]
    ;; ["M" "𝑴"]
    ;; ["O" "𝑴"]
    ;; ["Rn" "𝐑⁻"]
    ;; ["Rn0" "𝐑⁰⁻"]
    ;; ["R" "𝐑"]
    ;; ["Rp0" "𝐑⁰⁺"]
    ;; ["Rp" "𝐑⁺"]
    ;; ["Rp" "ℝ⁺"]
    ;; ["Fln" "Fl⁻"]
    ;; ["Fln0" "Fl⁰⁻"]
    ;; ["Fl" "Fl"]
    ;; ["Flp0" "Fl⁰⁺"]
    ;; ["Flp" "Fl⁺"]
    ;; ["T" "𝑻"]
    ;; ["F" "𝑭"]
    ;; ["B" "𝐁"]
    ;; ["St" "𝕊"]
    ;; ["Sy" "𝑺"]
    ;; ["H" "𝑯"]
    ;; ["Hi" "𝑯i"]
    ;; ["Hn" "𝑯m"]
    ;; ["A" "𝐀"]
    ;; ["Qn" "𝐐⁻"]
    ;; ["Qn0" "𝐐⁰⁻"]
    ;; ["Q" "𝐐"]
    ;; ["Qp0" "𝐐⁰⁺"]
    ;; ["Qp" "𝐐⁺"]
    ;; ["L^" "𝑳^"]
    ;; ["JS" "JSExpr"]
    ;; ["JH" "JSHash"]

;; ----------------------------------------------

    ;; ["N" "Natural"]
    ;; ["IN" "Index"]
    ;; ["In" "Index"]
    ;; ["Com" "Number"] ; Complex number
    ;; ["Zn" "Negative-Integer"]
    ;; ["Zn0" "Nonpositive-Integer"]
    ;; ["Z" "Integer"]
    ;; ["Zp0" "Nonnegative-Integer"]
    ;; ["Zp" "Positive-Integer"]
    ;; ["L" "Listof"]
    ;; ["L'" "List"]
    ;; ["V" "Vectorof"]
    ;; ["V'" "Vector"]
    ;; ["Pa" "Pair"]
    ;; ["M" "Maybe"]
    ;; ["O" "Option"]
    ;; ["FLn" "Negative-Float"]
    ;; ["Fln0" "Nonpositive-Float"]
    ;; ["Fl" "Float"]
    ;; ["Flp0" "Nonnegative-Float"]
    ;; ["Flp" "Positive-Float"]
    ;; ["Ren" "Negative-Real"]
    ;; ["Ren0" "Nonpositive-Real"]
    ;; ["Re" "Real"]
    ;; ["Rep0" "Nonnegative-Real"]
    ;; ["Rep" "Positive-Real"]
    ;; ["T" "True"]
    ;; ["F" "False"]
    ;; ["B" "Boolean"]
    ;; ["St" "String"]
    ;; ["Sy" "Symbol"]
    ;; ["H" "HashTable"]
    ;; ["Hi" "ImmutableHashTable"]
    ;; ["Hm" "MutableHashTable"]
    ;; ["Qn" "Negative-Exact-Rational"]
    ;; ["Qn0" "Nonpositive-Exact-Rational"]
    ;; ["Q" "Exact-Rational"]
    ;; ["Qp0" "Nonnegative-Exact-Rational"]
    ;; ["Qp" "Positive-Exact-Rational"]
    ;; ["L^" "Non-Empty-List"]
    ;; ["JS" "JSExpr"]
    ;; ["JH" "JSHash"]

		))



(xah-math-input--add-to-hash
 [
  ["deg" "°"]
  ["micro" "µ"]
  ["mdot" "·"]
  ["1/4" "¼"]
  ["1/2" "½"]
  ["3/4" "¾"]

  ["Theta" "Θ"] ["Lambda" "Λ"] ["Xi" "Ξ"] ["Phi" "Φ"] ["Psi" "Ψ"] ["Omega" "Ω"]

  ["beta" "β"] ["delta" "δ"] ["epsilon" "ε"] ["zeta" "ζ"] ["eta" "η"] ["theta" "θ"] ["mu" "μ"] ["xi" "ξ"] ["sigmaf" "ς"] ["tau" "τ"] ["phi" "φ"] ["psi" "ψ"] ["theta2" "ϑ"] ["upsih" "ϒ"] ["piv" "ϖ"]

  ["ndash" "–"] ["mdash" "—"]

  ["times" "×"] ["divide" "÷"] ["minus" "−"] ["lowast" "∗"] ["radic" "√"]
  ["oplus" "⊕"] ["otimes" "⊗"]
  ["oslash" "ø"]
  ["fnof" "ƒ"]

  ["nabla" "∇"]
  ;; ["part" "∂"]
  ["dp" "∂"]

	["ang" "∠"]

  ;; ["sub" "⊂"] ["sup" "⊃"] ["nsub" "⊄"] ["sube" "⊆"] ["supe" "⊇"]

  ["perp" "⊥"] ["sdot" "⋅"]

  ["lceil" "⌈"] ["rceil" "⌉"] ["lfloor" "⌊"] ["rfloor" "⌋"]

  ["lang" "〈"] ["rang" "〉"]

  ]
 )


(xah-math-input--add-to-hash
 [
  ;; misc non-math symbols
  ["tm" "™"]
  ["3/4" "¾"]
  ["1/2" "½"]
  ["1/4" "¼"]
  ["..." "…"]
  ["fdash" "‒"]
  ["wdash" "〜"]
  ["--" "—"]
  ;; ["??" "⁇"]
  ;; ["?!" "⁈"]
  ;; ["!?" "⁉"]
  ;; ["!!" "‼"]

  ;;
  ]

 )

(xah-math-input--add-to-hash
 [
  ["m2" "㎡"]
  ["cm" "㎝"]
  ["cm2" "㎠"]
  ["cm3" "㎤"]
  ] )


(xah-math-input--add-to-hash
 [
  ;; superscripts
  ["^0" "⁰"]
  ["^1" "¹"]
  ["^2" "²"]
  ["^3" "³"]
  ["^4" "⁴"]
  ["^5" "⁵"]
  ["^6" "⁶"]
  ["^7" "⁷"]
  ["^8" "⁸"]
  ["^9" "⁹"]
  ["^+" "⁺"]
  ["^-" "⁻"]
  ["^=" "⁼"]
  ["^(" "⁽"]
  ["^)" "⁾"]
  ["^n" "ⁿ"]
  ["^i" "ⁱ"]

  ;; subscripts
  ["_(" "₍"]
  ["_)" "₎"]
  ["_+" "₊"]
  ["_-" "₋"]
  ["_0" "₀"]
  ["_1" "₁"]
  ["_2" "₂"]
  ["_3" "₃"]
  ["_4" "₄"]
  ["_5" "₅"]
  ["_6" "₆"]
  ["_7" "₇"]
  ["_8" "₈"]
  ["_9" "₉"]
  ["_=" "₌"]
  ["_a" "ₐ"]
  ["_e" "ₑ"]

  ["_h" "ₕ"]
  ["_i" "ᵢ"]
  ["_j" "ⱼ"]
  ["_k" "ₖ"]
  ["_l" "ₗ"]
  ["_m" "ₘ"]
  ["_n" "ₙ"]
  ["_o" "ₒ"]
  ["_p" "ₚ"]
  ["_r" "ᵣ"]
  ["_s" "ₛ"]
  ["_t" "ₜ"]
  ["_u" "ᵤ"]
  ["_v" "ᵥ"]
  ["_x" "ₓ"]
  ["_schwa" "ₔ"]

  ])

(xah-math-input--add-to-hash
'(  ["empty" "∅"] ["+-" "±"] ["-+" "∓"]))

(xah-math-input--add-to-hash
 '(

   ;; ["flr" "⌊⌋"]
   ;; ["ceil" "⌈⌉"]
   ;; ["floor" "⌊⌋"]
   ;; ["ceiling" "⌈⌉"]

   ;; ["\"" "“”"] ;curly quote
   ;; ["\"\"" "“”"]

   ;; ["cb" "「」"] ; corner bracket
   ;; ["[" "「」"]

   ;; ["[(" "【】"] ; LEFT BLACK LENTICULAR BRACKET

  ;;   ["tb" "〔〕"] ; TORTOISE SHELL BRACKET
  ;; ["(" "〔〕"]

   ))


(xah-math-input--add-to-hash
 '(
   ;; letter-like forms
   ["R2" "ℝ²"]
   ["R3" "ℝ³"]
   ["r2" "ℝ²"]
   ["r3" "ℝ³"]
   ["fn" "ƒ"]))

(xah-math-input--add-to-hash
 '(
   ;; ["<" "≺"]
   ;; [">" "≻"]

   ["<=" "≤"]
   [">=" "≥"]
   ;; ["!el" "∉"]
   ;; ["el" "∈"]
   ;; ["in" "∈"]
   ["&&" "∧"]
   ["||" "∨"]
   ;; ["not" "¬"]
   ;; ["===" "≡"]
   ;; ["eq" "≡"]
   ;; ["xor" "⊻"]
   ;; ["nand" "⊼"]
   ;; ["nor" "⊽"]

   ["~" "≈"]
   [":=" "≔"]
   ["=:" "≕"]
   ["!=" "≠"]
   ["/=" "≠"]
   ["ne" "≠"]

   ;; ["fa" "∀"] ["forall" "∀"]
   ;; ["ex" "∃"]
   ["|-" "⊢"]
   ["-|" "⊣"]

))

(xah-math-input--add-to-hash
 '(

   ["<-" "←"] ["->" "→"] ["<->" "↔"] ["!<-" "↚"] ["!->" "↛"] ["!<->" "↮"]
   ["≤" "⇐"] ["=>" "⇒"]
["to" "⇒"]
 ["<=>" "⇔"] ["!<=" "⇍"] ["!=>" "⇏"] ["!<=>" "⇎"]
   ["<==" "⟸"] ["==>" "⟹"] ["<==>" "⟺"]
   ["<-|" "↤"] ["|->" "↦"]
   ["<--" "⟵"] ["-->" "⟶"] ["<-->" "⟷"]
   ["al" "←"]
   ["ar" "→"]
   ["au" "↑"]
   ["ad" "↓"]
   ["-<" "←"]
   ["->" "→"]
   ["-!" "↑"]
   ["-v" "↓"]

   ))

(xah-math-input--add-to-hash
 '(

   ;; operators
   ;; ["cp" "⊕"] ; circle plus
   ;; ["ct" "⊗"] ; circle times
   ;; ["cm" "⊖"] ; circle minus
   ;; ["cd" "⊘"] ; circle divide
   ["'" "′"]  ; prime
   ["''" "″"] ; double prime
   ["'''" "‴"]
   ["." "·"]
   ["sqrt" "√"]
   ["rt" "√"]
   ["rtt" "∛"]
   ["crt" "∛"] ; cube root
   ["rt3" "∛"]
   ["rtf" "∜"]
   ["frt" "∜"] ; fourth root
   ["rt4" "∜"]
   ["del" "∇"]

   ["pd" "∂"] ; partial derivative
   ["cross" "⨯"]
   ;; ["cint" "∮"] ; contour integral
   ;; ["ccint" "∲"]
   ;; ["cccint" "∳"]
   ["union" "∪"]
   ["intersection" "∩"]))

(xah-math-input--add-to-hash
 '(
   ["/_" "∠"] ;ANGLE
   ["rightangle" "⦜"]
   ["|_" "⦜"]
   ))



(defun xah-math-input--hash-to-list (hashtable)
  "Return a list that represent the HASHTABLE."
  (let (mylist)
    (maphash (lambda (kk vv) (setq mylist (cons (list vv kk) mylist))) hashtable)
    mylist
    ))

(defun xah-math-input-list-math-symbols ()
  "Print a list of math symbols and their input abbreviations.
See `xah-math-input-mode'."
  (interactive)
  (with-output-to-temp-buffer "*xah-math-input output*"
    (mapc (lambda (tt)
            (princ (concat (car tt) " " (car (cdr tt)) "\n")))
          (sort
           (xah-math-input--hash-to-list xah-math-input-abrvs)
           (lambda
             (a b)
             (string< (car a) (car b)))))))

(defvar xah-math-input-keymap nil "Keymap for xah-math-input mode.")

(progn
  (setq xah-math-input-keymap (make-sparse-keymap))
  (define-key xah-math-input-keymap (kbd "S-SPC") 'xah-math-input-change-to-symbol))

(defun xah-math-input--abbr-to-symbol (@inputStr)
  "Returns a char corresponding to @inputStr.
If none found, return nil.
Version 2018-02-16"
  (let ($resultChar $charByNameResult)
    (setq $resultChar (gethash @inputStr xah-math-input-abrvs))
    (cond
     ($resultChar $resultChar)
     ;; begin with u+
     ((string-match "\\`u\\+\\([0-9a-fA-F]+\\)\\'" @inputStr) (char-to-string (string-to-number (match-string 1 @inputStr) 16)))
     ;; decimal. 「945」 or 「#945」
     ((string-match "\\`#?\\([0-9]+\\)\\'" @inputStr) (char-to-string (string-to-number (match-string 1 @inputStr))))
     ;; e.g. decimal with html entity markup. 「&#945;」
     ((string-match "\\`&#\\([0-9]+\\);\\'" @inputStr) (char-to-string (string-to-number (match-string 1 @inputStr))))
     ;; hex number. e.g. 「x3b1」 or 「#x3b1」
     ((string-match "\\`#?x\\([0-9a-fA-F]+\\)\\'" @inputStr) (char-to-string (string-to-number (match-string 1 @inputStr) 16)))
     ;; html entity hex number. e.g. 「&#x3b1;」
     ((string-match "\\`&#x\\([0-9a-fA-F]+\\);\\'" @inputStr) (char-to-string (string-to-number (match-string 1 @inputStr) 16)))
     ;; unicode full name. e.g. 「GREEK SMALL LETTER ALPHA」
     ((and (string-match "\\`\\([- a-zA-Z0-9]+\\)\\'" @inputStr)
           (setq $charByNameResult (xah-math-input--name-to-codepoint @inputStr)))
      (char-to-string $charByNameResult))
     (t nil))))

(defun xah-math-input--name-to-codepoint (@name)
  "Returns integer that's the codepoint of Unicode char named @name (string).
Version 2018-07-09"
  (interactive)
  (if (version<= "26" emacs-version)
      (gethash @name (ucs-names))
    (assoc-string @name (ucs-names) t)))

(defun xah-math-input-change-to-symbol (&optional print-message-when-no-match)
  "Change text selection or word to the left of cursor into a Unicode character.

A valid input can be any abbreviation listed by the command `xah-math-input-list-math-symbols', or, any of the following form:

 945     ← decimal
 #945    ← decimal with prefix #
 &#945;  ← XML entity syntax

 x3b1    ← hexadimal with prefix x
 U+3B1   ← hexadimal with prefix U+ (lower case ok.)
 #x3b1   ← hexadimal with prefix #x
 &#x3b1; ← XML entity syntax

Full Unicode name can also be used, e.g. 「greek small letter alpha」.

If preceded by `universal-argument', print error message when no valid abbrev found.

See also: `xah-math-input-mode'.
Version 2018-07-09"
  (interactive "P")
  (let ($p1 $p2 $inputStr $resultChar)
    (if (region-active-p)
        (progn
          (setq $p1 (region-beginning))
          (setq $p2 (region-end))
          (setq $inputStr (buffer-substring-no-properties $p1 $p2))
          (setq $resultChar (xah-math-input--abbr-to-symbol $inputStr))
          (when $resultChar (progn (delete-region $p1 $p2) (insert $resultChar))))
      ;; if there's no text selection, grab all chars to the left of cursor point up to whitespace, try each string until there a valid abbrev found or none char left.
      (progn
        (setq $p2 (point))
        (skip-chars-backward "^ \t\n" -20)
        (setq $p1 (point))
        (while (and (not $resultChar) (>= (- $p2 $p1) 1))
          (setq $inputStr (buffer-substring-no-properties $p1 $p2))
          (setq $resultChar (xah-math-input--abbr-to-symbol $inputStr))
          (when $resultChar (progn (goto-char $p2) (delete-region $p1 $p2) (insert $resultChar)))
          (setq $p1 (1+ $p1)))))
    (when (not $resultChar)
      (when print-message-when-no-match
        (xah-math-input-list-math-symbols)
        (user-error "「%s」 no match found for that abbrev/input. M-x `xah-math-input-list-math-symbols' for a list. Or use a decimal e.g. 「945」 or hexadecimal e.g. 「x3b1」, or full Unicode name e.g. 「greek small letter alpha」."  $inputStr)))))

;;;###autoload
(define-globalized-minor-mode global-xah-math-input-mode xah-math-input-mode xah-math-input-mode-on)

;;;###autoload
(defun xah-math-input-mode-on ()
  "Turn on `xah-math-input-mode' in current buffer."
  (interactive)
  (xah-math-input-mode 1))

;;;###autoload
(defun xah-math-input-mode-off ()
  "Turn off `xah-math-input-mode' in current buffer."
  (interactive)
  (xah-math-input-mode 0))

;;;###autoload
(define-minor-mode xah-math-input-mode
  "Toggle xah-math-input minor mode.

A mode for inputting a math and Unicode symbols.

Type “inf”, then press \\[xah-math-input-change-to-symbol] (or M-x `xah-math-input-change-to-symbol'), then it becomes “∞”.

Other examples:
 a → α
 p → π
 /= → ≠ or ne
 >= → ≥ or ge
 -> → → or rarr
 and → ∧
etc.

If you have a text selection, then selected word will be taken as input. For example, type 「extraterrestrial alien」, select the phrase, then press \\[xah-math-input-change-to-symbol], then it becomse 👽.

For the complete list of abbrevs, call `xah-math-input-list-math-symbols'.

Decimal and hexadecimal can also be used. Example:

 945     ← decimal
 #945    ← decimal with prefix #
 &#945;  ← XML entity syntax

 x3b1    ← hexadimal with prefix x
 #x3b1   ← hexadimal with prefix #x
 &#x3b1; ← XML entity syntax

Full Unicode name can also be used, e.g. 「greek small letter alpha」.

If you wish to enter a symbor by full unicode name but do not know the full name, M-x `insert'. Asterisk “*” can be used as a wildcard to find the char. For example, M-x `insert' , then type 「*arrow」 then Tab, then emacs will list all unicode char names that has “arrow” in it. (this feature is part of Emacs 23)

Home page at: URL `http://ergoemacs.org/emacs/xah-math-input-math-symbols-input.html'"
  nil
  :global nil
  :lighter " ∑α"
  :keymap xah-math-input-keymap
  )

(provide 'xah-math-input)

;;; xah-math-input.el ends here
