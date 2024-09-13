
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

	["AA" "𝔸"] ["BB" "𝔹"] ["CD" "ℂ"] ["CCC" "ℂ"] ["DD" "𝔻"] ["EE" "𝔼"] ["FF" "𝔽"] ["GG" "𝔾"] ["HH" "ℍ"] ["ID" "𝕀"] ["III" "𝕀"] ["JJ" "𝕁"] ["KK" "𝕂"] ["LL" "𝕃"] ["MM" "𝕄"] ["NN" "ℕ"] ["OD" "𝕆"] ["OOO" "𝕆"] ["PP" "ℙ"] ["QQ" "ℚ"] ["RR" "ℝ"] ["SD" "𝕊"] ["SSS" "𝕊"] ["TT" "𝕋"] ["UD" "𝕌"] ["UUU" "𝕌"] ["VD" "𝕍"] ["VVVV" "𝕍"] ["WW" "𝕎"] ["XX" "𝕏"] ["YY" "𝕐"] ["ZZ" "ℤ"]


	["AAIS" "𝑨"] ["BBIS" "𝑩"] ["CCIS" "𝑪"] ["DDIS" "𝑫"] ["EEIS" "𝑬"] ["FFIS" "𝑭"] ["GGIS" "𝑮"] ["HHIS" "𝑯"] ["IIIS" "𝑰"] ["JJIS" "𝑱"] ["KKIS" "𝑲"] ["LLIS" "𝑳"] ["MMIS" "𝑴"] ["NNIS" "𝑵"] ["OOIS" "𝑶"] ["PPIS" "𝑷"] ["QQIS" "𝑸"] ["RRIS" "𝑹"] ["SSIS" "𝑺"] ["TTIS" "𝑻"] ["UUIS" "𝑼"] ["VVIS" "𝑽"] ["WWIS" "𝑾"] ["XXIS" "𝑿"] ["YYIS" "𝒀"] ["ZZIS" "𝒁"]

	["AASI" "𝑨"] ["BBSI" "𝑩"] ["CCSI" "𝑪"] ["DDSI" "𝑫"] ["EESI" "𝑬"] ["FFSI" "𝑭"] ["GGSI" "𝑮"] ["HHSI" "𝑯"] ["IISI" "𝑰"] ["JJSI" "𝑱"] ["KKSI" "𝑲"] ["LLSI" "𝑳"] ["MMSI" "𝑴"] ["NNSI" "𝑵"] ["OOSI" "𝑶"] ["PPSI" "𝑷"] ["QQSI" "𝑸"] ["RRSI" "𝑹"] ["SSSI" "𝑺"] ["TTSI" "𝑻"] ["UUSI" "𝑼"] ["VVSI" "𝑽"] ["WWSI" "𝑾"] ["XXSI" "𝑿"] ["YYSI" "𝒀"] ["ZZSI" "𝒁"]


	["AAS" "𝐀"] ["BBS" "𝐁"] ["CCS" "𝐂"] ["DDS" "𝐃"] ["EES" "𝐄"] ["FFS" "𝐅"] ["GGS" "𝐆"] ["HHS" "𝐇"] ["IIS" "𝐈"] ["JJS" "𝐉"] ["KKS" "𝐊"] ["LLS" "𝐋"] ["MMS" "𝐌"] ["NNS" "𝐍"] ["OOS" "𝐎"] ["PPS" "𝐏"] ["QQS" "𝐐"] ["RRS" "𝐑"] ["SSS" "𝐒"] ["TTS" "𝐓"] ["UUS" "𝐔"] ["VVS" "𝐕"] ["WWS" "𝐖"] ["XXS" "𝐗"] ["YYS" "𝐘"] ["ZZS" "𝐙"]

	["dd" "ⅆ"] ["eee" "ⅇ"] ["id" "ⅈ"] ["jj" "ⅉ"] ["j" "ⅉ"]

  ;; TODO: add Greek mathematical capital bold letters

  ;; TODO: add Greek mathematical small bold letters

  ;; TODO: add Latin (English) mathematical small bold letters

	])




(xah-math-input--add-to-hash
 '(

	 ;; AsciiDoc
	 ["??" "* ??? \n\n"]; personal notation for questions
	 ["fill" "     -----------------------------------------------------"] ; replacement for empty lines in output to avoid blank lines, for multi-line navigation purposes
	 ["t" "    "] ; 4 spaces
	 ["src" "[source]\n"] ; 4 spaces
	 ["dl" "----"]; source code (adoc)
	 ["dll" "----\n\n----"]; source code (adoc)
	 ["DL" "------------------------------------"]
	 ["hh" "== "] ; header — adoc
	 ["hj" "=== "] ; header — adoc
	 ["hk" "==== "] ; header — adoc
	 ["hl" "===== "]; header — adoc
	 ["pb" "++++"] ; passthrough block -> adoc use
	 ["pass" "++++"] ; passthrough block -> adoc use
	 ["lb" "...."] ; literal block -> adoc use
	 ["lit" "...."] ; literal block -> adoc use
	 ["esc" "+++"] ; escaped -> adoc use
	 ["ppp" "+++"] ; escaped -> adoc use
	 ["PPP" "+++ +++"] ; escaped -> adoc use
	 ["ESC" "+++ +++"] ; escaped -> adoc use
	 ["note" "NOTE: "]


   ;; misc.
   ["NL" "\\n"] ; newline
   ["CR" "
"] ; carriage return

   ;; digits
   ["ze" "0"]
   ["on" "1"]
   ["tw" "2"]
   ["th" "3"]
   ["fo" "4"]
   ["fi" "5"]
   ["si" "6"]
   ["se" "7"]
   ["ei" "8"]
   ["ni" "9"]
   ["te" "10"]
   ["fif" "15"]; time
   ["thi" "30"]; time
   ["fofi" "45"]; time
   ["fof" "45"]; time
   ["nn" "99"]; APL rank operator

	 ["s" "[]"] ; square
	 ["c" "{}"] ; curly
	 ["dol" "${}"]
	 ["ti" "~"]
	 ["ex" "!"]
	 ["at" "@"]
	 ["h" "#"]
	 ["ha" "#"]
	 ["do" "$"] ; dollar sign
	 ["SS" "$"] ; dollar sign
	 ["pe" "%"] ; percent
	 ["ca" "^"] ; carrot
	 ["up" "^"] ; carrot
	 ["u" "^"] ; carrot
	 ["am" "&"]
	 ["andd" "&&"]
	 ["and'" "&&"]
	 ["orr" "||"]
	 ["or'" "||"]
	 ["as" "*"] ; asterick
	 ["st" "*"]
	 ["mi" "-"]
	 ["m" "-"]
	 ["un" "_"]
	 ["pl" "+"]
	 ["p" "+"]
	 ["eq" "="]
 	 ["e" "="]
	 ["bs" "\\"] ; single backslash
	 ["mc" "/**/"] ; multi-line comment

	 ;; personal
	 ["cam" "Cameron"]
	 ["cac" "Cameron Chandoke"]
	 ["scameron" "⊂∧m∊r⍥n"] ; stylized
	 ["scameron'" "⊂∧m∊r⌽n"] ; stylized
	 ["scammy" "⊂∧mm𝛄"] ; stylized
	 ["scam" "⊂∧m"] ; stylized


	 ;; phrase abbrev
	 ["afaik" "as far as i know"]
	 ["afik" "as far as i know"]
	 ["atm" "at the moment"]
	 ["dfb" "difference between"]
	 ["dbt" "difference between"]
	 ["dfbt" "difference between"]
	 ["ty" "thank you"]
	 ["ui" "user interface"]
	 ["ur" "your"]
	 ["ya" "you are"]
	 ["ru" "are you"]
	 ["vv" "vice versa"]
	 ["byway" "by the way"]
	 ["bywa" "by the way"]


	 ;; english word abbrev
	 ["ie," "i.e., "]
	 ["Ie," "I.e., "]
	 ["IE," "I.e., "]
	 ["ie" "i.e. "]
	 ["Ie" "I.e. "]
	 ["IE" "I.e. "]
	 ["eg," "e.g., "]
	 ["Eg," "E.g., "]
	 ["EG," "E.g., "]
	 ["eg" "e.g. "]
	 ["Eg" "E.g. "]
	 ["EG" "E.g. "]
	 ["cf" "c.f. "]
	 ["Cf" "C.f. "]
	 ["CF" "C.f. "]

   ; contractions
	 ["cant" "can't"]
	 ["cnt" "can't"]
	 ["wont" "won't"]
	 ["wnt" "won't"]
	 ["iv" "I've"]
	 ["ive" "I've"]
	 ["ivnt" "I haven't"]
	 ["ivent" "I haven't"]
	 ["idnt" "I didn't"]
	 ["idntv" "I wouldn't have"]
	 ["idntve" "I wouldn't have"]
	 ["idve" "I would've"]
	 ["theydntve" "they wouldn't have"]
	 ["wedntve" "we wouldn't have"]
	 ["youdntve" "you wouldn't have"]
	 ["didnt" "didn't"]
	 ["ddnt" "didn't"]
	 ["dont" "don't"]
	 ["wouldnt" "wouldn't"]
	 ["wdnt" "wouldn't"]
	 ["aint" "ain't"]
	 ["doesnt" "doesn't"]
	 ["dsnt" "doesn't"]
	 ["dsnt" "doesn't"]
	 ["couldnt" "couldn't"]
	 ["cdnt" "couldn't"]
	 ["wevnt" "we haven't"]
	 ["theyvnt" "they haven't"]
	 ["youre" "you're"]
	 ["ann" "announcement"]
	 ["arg" "argument"]
	 ["args" "arguments"]
	 ["auto" "automatic"]
	 ["bc" "because "]
	 ;; ["bg" "background"]
	 ["bt" "between"]
	 ["btn" "between"]
	 ["math" "mathematical"]
	 ["maths" "mathematics"]
	 ["prop" "proposition"]
	 ["bywa" "by the way "]
	 ["wo'" "without"]
	 ["q" "question"]
	 ["num" "number"]
	 ["instr" "instruction"]
	 ["instrs" "instructions"]
	 ["tut" "tutorial"]

	 ;; computing
	 ["fcc" "first-class citizens"]
	 ["fccs" "first-class citizens"]
	 ["gen" "generate"]
	 ["gena" "generative"]; adjective
	 ["primrec" "primitive recursion"]
	 ["linrec" "linear recursion"]
	 ["binrec" "binary recursion"]
	 ["genrec" "generative recursion"]
	 ["imp" "implication"]
	 ["fnal" "functional"]
	 ["cpt" "compute"]
	 ["cmpt" "compute"]
	 ["ans" "answer"]
	 ["elg" "elegant"]
	 ["alang" "array language"]
	 ["slang" "stack language"]
	 ["arrl" "array language"]
	 ["stackl" "stack language"]
	 ["depts" "dependent types"]
	 ["deptp" "dependent typing"]; present tense
	 ["depting" "dependent typing"]; present tense
	 ["ahk" "AutoHotkey"]
	 ["cfg" "context-free grammar"]
	 ["cj" "Clojure"]
	 ["cs" "computer science"]
	 ["CS" "Computer Science"]
	 ["oss" "open-source software"]
	 ["osrc" "open-source"]
	 ["bin" "binary"]
	 ["cdg" "coding"]
	 ["cdng" "coding"]
	 ["logi" "logical"]
	 ["cnss" "consistent"]
	 ["cons" "consistent"]
	 ["consti" "constituent"]
	 ["cnstn" "constituent"]
	 ["constt" "constitutes"]
	 ["cnstt" "constitutes"]
	 ["defi" "definition"]
	 ["nota" "notation"]
	 ["notal" "notational"]
	 ["notl" "notational"]
	 ["stmt" "statement"]
	 ["expr" "expression"]
	 ["exprs" "expressions"]
	 ["exps" "expressions"]
	 ["subexpr" "subexpression"]
	 ["subexp" "subexpression"]
	 ["subex" "subexpression"]
	 ["subexprs" "subexpressions"]
	 ["subexps" "subexpressions"]
	 ["subexs" "subexpressions"]
	 ["expo" "exponent"]
	 ["char" "character"]
	 ["chr" "character"]
	 ["autho" "authorization"]
	 ["authe" "authentication"]
	 ["init" "initialize"]
	 ["initn" "initialization"]
	 ["conj" "conjunction"]
	 ["cjn" "conjunction"]
	 ["op" "operator"]
	 ["opr" "operation"]
	 ["fn" "function"]
	 ["fns" "functions"]
	 ["par" "parentheses"]
	 ["paren" "parentheses"]
	 ["pard" "parenthesized"]
	 ["parend" "parenthesized"]
	 ["ppar" "pair of parentheses"]
	 ["epar" "enclosing parentheses"]
	 ["ebrac" "enclosing braces"]
	 ["ebrak" "enclosing brackets"]
	 ["encbrac" "enclosing braces"]
	 ["encbrak" "enclosing brackets"]
	 ["outm" "outermost"]
	 ["inm" "innermost"]
	 ["intdep" "interdependent"]
	 ["interd" "interdependent"]
	 ["interc" "interconnected"]
	 ["inde" "independent"]
	 ["arb" "independent"]
	 ["orth" "orthogonal"]
	 ["ortho" "orthogonal"]
	 ["orthn" "orthogonalization"]; noun
	 ["orthon" "orthogonalization"]; noun
	 ["ialo" "inner→outer"]
	 ["iglo" "inner→outer"]
	 ["oali" "outer→inner"]
	 ["ogli" "outer→inner"]
	 ["hashm" "hashmap"];
	 ["hasht" "hashtable"];
	 ["dic" "dictionary"];
	 ["adj" "adjective"];
	 ["homoa" "homoiconic"]; adjective
	 ["homon" "homomorphism"]; noun
	 ["homoc" "homomorphic"]
	 ["isom" "isomorphism"]
	 ["isomc" "isomorphic"]
	 ["epia" "epimorphic"]; adjective
	 ["epin" "epimorphism"]; noun
	 ["surn" "surjection"]; noun
	 ["sura" "surjective"]; adjective
	 ["injn" "injection"]; noun
	 ["inja" "injective"]; adjective
	 ["bijn" "bijection"]; noun
	 ["bija" "bijective"]; adjective
	 ["trad" "traditional"]
	 ["elem" "element"]
	 ["elems" "elements"]
	 ["metap" "metaprogramming"]
	 ["metac" "metacognition"]
	 ["hask" "Haskell"]
	 ["wrt" "with respect to "]
	 ["stdml" "Standard ML"]
	 ["ml" "machine learning"]
	 ["ai" "artificial intelligence"]
	 ["exd" "Extended Dyalog"]
	 ["edy" "Extended Dyalog"]
	 ["edya" "Extended Dyalog APL"]
	 ["exda" "Extended Dyalog APL"]
	 ["dza" "dzaima/APL"]
	 ["dapl" "dzaima/APL"]
	 ["ngn" "ngn/APL"]
	 ["apl" "APL"]
	 ["nars" "NARS"]
	 ["gnua" "GNU APL"]
	 ["gnuapl" "GNU APL"]
	 ["col" "column"]
	 ["cols" "columns"]

	 ;; tech company
	 ["gc" "Google Chrome"]
	 ["chrome" "Google Chrome"]
	 ["ff" "Firefox"]
	 ["mf" "Mozilla Firefox"]
	 ["mz" "Mozilla"]
	 ["gm" "Google Maps"]
	 ["macos" "Mac OS"]
	 ["mwin" "Microsoft Windows"]
	 ["mw" "Microsoft Windows"]
	 ["win" "Windows"]

	 ;; programing
	 ["evar" "environment variable"]
	 ["env" "environment"]
	 ["ipa" "IP address"]
	 ["jvm" "Java Virtual Machine"]
	 ["rsi" "repetitive-strain injury"]
	 ["dir" "directory"]
	 ["dirs" "directories"]
	 ["subdir" "sub-directory"]
	 ["sdir" "sub-directory"]
	 ["subdirs" "sub-directories"]
	 ["sdirs" "sub-directories"]
	 ["subf" "sub-folder"]
	 ["attr" "attribute"]
	 ["wd" "web development"]
	 ["db" "database"]
	 ["dbs" "databases"]
	 ["gui" "GUI"]
	 ["gnu" "GNU"]
	 ["gui'" "graphical user interface"]
	 ["oopf" "object oriented programing"]
	 ["oop" "OOP"]
	 ["OS" "operating system"]

	 ;; programming
	 ["eqe" "=="]
	 ["ret" "return"]
	 ["utf8" "-*- coding: utf-8 -*-"]

	 ;; catch typos
	 ["pwoer" "power"]
	 ["lsit" "list"]
	 ["csae" "case"]

))



(xah-math-input--add-to-hash
 [

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

])


;; (xah-math-input--add-to-hash
;;  [
;; 	;; ["ydl" "youtube-dl -x --no-playlist -o "~/Music/youtube-dl/%(title)s.%(ext)s" --audio-format mp3'"]
;; 	;; ["ydl" "'youtube-dl -x --no-playlist -o "~/Music/youtube-dl/%(title)s.%(ext)s" --audio-format mp3'"]
;; ])



;; (xah-math-input--add-to-hash
;;  [
;; 	;; ["def" "define "]
;; 	;; ["deft" "define-type "]
;; 	;; ["fn" "define "]
;; ])




;; Mathematics
(xah-math-input--add-to-hash
 '(

   ; Predicate logic
	["bim" "⟷ "]; bi-implication
	["bimp" "⟷ "]; bi-implication
	["same" "⟷ "]
	["⟷ " "<--> "]; for Wunderlist
	["All" "∀"]
  ["all" "∀"]
  ["for" "∀"]; for x in X --> ∀x∈X
	["exi" "∃"]
	["any" "∃"]
	["some" "∃"]
	["nex" "∄"]
	["none" "∄"]
	["an" "∧"]
	["and" "∧"]
  ["or" "∨"]
	["nor'" "⊽"]; original reserved for APL version
	["nand'" "⊼"]; original reserved for APL version
	["nan'" "⊼"]; original reserved for APL version
	["xor" "⊻"]
	["OR" "⋁"]
	["AND" "⋀"]
	["not" "¬"] ; not-sign
	["fnot" "⌙"] ; (vertically) flipped not-sign
	["fno" "⌙"] ; (vertically) flipped not-sign
	["rnot" "⌐"] ; reversed not-sign
	["rno" "⌐"] ; reversed not-sign
	["let" "∴ "]
	["wher" "∵ "]
	["where" "∵ "]
	["qed" "∎"]
	["eop" "∎"]; "end of proof"

	["proh" "🛇"]
	["prohibit" "🛇"]
	["prohibited" "🛇"]
	["cannot" "🛇"]
	["nallow" "🛇"]


  ; Set logic
	["UU" "∪"]
	["II" "∩"]
 	["∪" "⋃"]; n-ary
	["∩" "⋂"]; n-ary
 	["UU'" "⋃"]; n-ary ; UU'
	["II'" "⋂"]; n-ary ; II'
	["⋃" "∪"]; n-ary to binary
	["⋂" "∩"]; n-ary to binary
 	["(" "⊂"]
 	["⊂" "⊆"]
 	["⊆" "⊄"]
 	["⊄" "⊈"]
 	["⊈" "⊂"]
 	[")" "⊃"]
 	["⊃" "⊇"]
 	["⊇" "⊅"]
 	["⊅" "⊉"]
 	["⊉" "⊃"]
	["in'" "∈"] ; "in" reserved for small-elem-of (APL)
 	["CC" "⊆"]

  ["ele" "∈"] ; small element of; enlist
	["nin" "∉"]
	["con" "∋"]
	["cont" "∋"]
	["nco" "∌"]
	["ncon" "∌"]
	["sc" "∁"] ; set complement
  ["comp" "∁"]
	["scom" "∁"]
	["scomp" "∁"]
	["pset" "℘"] ; powerset
	["ps" "℘"] ; powerset
	["\\'" "∖"] ; double backslash; each escaped here by another backslash
	["smi" "∖"] ; set minus
	["set-" "∖"] ; set minus
	["s-" "∖"] ; set minus
	["emp" "∅"]
	["bagh" "⟅"]
	["bagl" "⟆"]
	["inbag" "⋿"]
	["inb" "⋿"]; in bag

	["Z-" "ℤ⁻ "]
	["Z-0" "ℤ⁰⁻ "]
	["Z+0" "ℤ⁰⁺ "]
	["Z+" "ℤ⁺ "]
	["R-" "ℝ⁻ "]
	["R-0" "ℝ⁰⁻ "]
	["R+0" "ℝ⁰⁺ "]
	["R+" "ℝ⁺ "]
	["Q-" "ℚ⁻ "]
	["Q-0" "ℚ⁰⁻ "]
	["Q+0" "ℚ⁰⁺ "]
	["Q+" "ℚ⁺ "]
	["Z-" "ℤ⁻ "]
	["Z0-" "ℤ⁰⁻ "]
	["Z0+" "ℤ⁰⁺ "]
	["Z+" "ℤ⁺ "]
	["R-" "ℝ⁻ "]
	["R0-" "ℝ⁰⁻ "]
	["R0+" "ℝ⁰⁺ "]
	["R+" "ℝ⁺ "]
	["Q-" "ℚ⁻ "]
	["Q0-" "ℚ⁰⁻ "]
	["Q0+" "ℚ⁰⁺ "]
	["Q+" "ℚ⁺ "]
	["ZN" "ℤ⁻ "]
	["ZN0" "ℤ⁰⁻ "]
	["ZP0" "ℤ⁰⁺ "]
	["ZP" "ℤ⁺ "]
	["RN" "ℝ⁻ "]
	["RN0" "ℝ⁰⁻ "]
	["RP0" "ℝ⁰⁺ "]
	["RP" "ℝ⁺ "]
	["QN" "ℚ⁻ "]
	["QN0" "ℚ⁰⁻ "]
	["QP0" "ℚ⁰⁺ "]
	["QP" "ℚ⁺ "]




	["." "∘"]
	["of" "∘"]; composition
	["map" "↦"]
	["maps" "↦"]
	["app" "↥"] ; apply


	["ne" "≠"]
	["le" "≤"]
	["<" "≤"]
	["<u" "≤"]
	["ge" "≥"]
	[">" "≥"]
	[">u" "≥"]
	["le'" "⩽"]
	["<u''" "⩽"]
	["ge'" "⩾"]
	[">u'" "⩾"]
	["abs" "∣"]
	["ab" "∣"]
	["|" "∣"]


	["sum" "∑"]
	["prod" "∏"]
	["cop" "∐"]
	["acop" "⨿"]


	["cro" "⨯"]
  ["cross" "⨯"]
  ["delt" "∆"]
  ["Delta" "∆"]
	["del" "∇"]
	["nab" "∇"] ; nabla


	["G" "𝚪"]; gamma function
  ["lam" "λ"]; lambda
  ["la" "λ"]; lambda
	["pi" "π"]
	["pib" "𝛑"] ; pi bold
	["eul" "ℯ"]; euler
	["eul'" "ℇ"]; euler
	["pla" "ℎ"]; planck
	["gam" "𝛾"]; gamma
	["inf" "∞"]; infinity


	["est" "℮"]
	["pred" "≺"]
	["succ" "≻"]
	["prer" "⊰"] ; precedes under relation
	["sucr" "⊱"] ; succeeds under relation
	[":::" "∷"]; "::" taken by APL 'each' (¨); just having ":" leads to easy mistakes why typing all-caps word followed by colon
	["prop" "∝"]
	["pro" "∝"]
	["O+" "⊕"]
	["O--" "⊖"]
	["O*" "⊛"]
	["Od" "⨸"]
	["O/" "⊘"]
	["cir+" "⊕"]
	["cir-" "⊖"]
	["cirx" "⊗"]
	["cir*" "⊛"]
	["cird" "⨸"]
	["cir/" "⊘"]
	["cirm" "⊖"]
	["cirp" "⊕"]
	["cirs" "⊛"]

  ;; operators
  ;; ["cp" "⊕ "] ; circle plus
  ;; ["ct" "⊗ "] ; circle times
  ;; ["cm" "⊖ "] ; circle minus
  ;; ["cd" "⊘ "] ; circle divide
  ["p'" "′"]  ; prime -- save ' for back-tick (grave accent) ` (adoc, etc)
  ["⁗" "′"]  ; prime -- cycle from quad prime
  ["p''" "″"] ; double prime
  ["′" "″"] ; double prime
  ["p'''" "‴"] ; triple prime
  ["″" "‴"] ; triple prime
  ["p''''" "⁗"] ; quad prime
  ["‴" "⁗"] ; quad prime
  [".m" "·"]


  ["+-" "±"]
  ["-+" "∓"]
  ["pm" "±"]
  ["mp" "∓"]


  ["rt" "√"]
  ["rtt" "∛ "]
  ["rtc" "∛ "]
  ["crt" "∛ "] ; cube root
  ["trt" "∛ "] ; third root
  ["rt3" "∛ "]
  ["rtf" "∜ "]
  ["frt" "∜ "] ; fourth root
  ["rt4" "∜ "]



  ["inv'" "¯¹"]
  ["d" "∂"]
  ["int" "∫"]
  ["ii" "∬"]
  ["iii" "∭"]
  ["iiii" "⨌"]
  ["io" "∮"]
  ["iio" "∯"]
  ["iiio" "∰"]
  ["icl" "∱"]
  ["cli" "∱"]
  ["iac" "⨑"]
  ["aci" "⨑"]
  ["aci" "⨑"]
  ["iocl" "∲"]
  ["cio" "∲"]
  ["clio" "∲"]
  ["ioac" "∳"]
  ["acio" "∳"]
  ["intf" "⨍"]
  ["ioap" "⨕"] ; integral around a point
  ["int=" "⨎"] ; integral with double stroke
  ["intu" "⨚"] ; integral with union
  ["inti" "⨙"] ; integral with intersection
  ["fourier" "ℱ"]
  ["fft" "ℱ"]

  ["{{" "⦃"]
  ["}}" "⦄"]
  ["{{}}" "⦃⦄"]
  ["[[" "〚"]
  ["]]" "〛"]
  ["[[]]" "〚〛"]

  ["o-" "⟜"]
  ["aft" "⟜"]; BQN's "after"
  ["-o" "⊸"]
  ["bef" "⊸"]; BQN's "before"
  ["o-o" "⧟ "]
  ["o-." "⊶"]
  [".-o" "⊷"]
  ;; ["cur" "⫶ "] ; curry


  ))


;; general programming
(xah-math-input--add-to-hash
 [

  ["re" "require ''"]
  ["im" "import "]

 ])



;; APL primitives
(xah-math-input--add-to-hash
 [

  ["l" "{}"]; lambda
  ["ah" "←"]
  ["al" "→"]
  ["ta" "↑"] ; take
  ["mix" "↑"]
  ["au" "↑"]
  ["ak" "↑"]
  ["ad" "↓"]
  ["aj" "↓"]
  ["dr" "↓"] ; drop
  ["sp" "↓"] ; split
  ["spl" "↓"] ; split
  ["is" "←"]
  ["def" "←"]
  ["df" "←"]
  [")" "⊃"]
  ["dis" "⊃"]; disclose
  ["unb" "⊃"]; unbox
  ["pic" "⊃"]; pick
  ["x" "×"]; times
  ;; ["vh" "⍅"] ; left vane
  ;; ["vl" "⍆"] ; right vane
  ;; ["vk" "⍏"] ; upward vane
  ;; ["vj" "⍖"] ; downward vane
  ;; ["vu" "⍏"] ; upward vane
  ;; ["vd" "⍖"] ; downward vane
  ["div" "÷"]; divide
  ["di" "÷"]; divide
  ["li" "‿"]; link
  ["ln" "‿"]; link

  ["ta'" "↑"]; take
  ["dr'" "↓"]; drop
  ["x'" "×"]
  ["-" "¯"] ; high minus
  ;; ["_" "¯"] ; high minus
  ["n" "¯"] ; high minus
  ["non" "¯1"] ; "negative one"
  ["lo" "⍟"]
  ["log" "⍟"]
  ["lg" "⍟"]
  ["ee" "="]
  ["ma" "≡"]; match
  ["mat" "≡"]; match
  ["dep" "≡"]; depth
  ["tal" "≢"]; tally
  ["co" "≢"]; count
  ["cou" "tal"]; "count"
  ["nat" "≢"]; not match "natch"
  ["na" "≢"]; not match "natch"
  ["nm" "≢"]; not match
  ["een" "≢"]
  ["nee" "≢"]
  ["ee/" "≢"]
  ["lcm" "∧"] ; lowest common multiple
  ["gcd" "∨"] ; lowest common multiple
  ["r" "⍴"]
  ["rr" "⍴⍴"]
  ["i" "⍳"]
  ["iu" "⍸"]
	["wh" "⍸"]; where
	["whe" "⍸"]; where
  ["w" "⍵"]
  ["ww" "⍵⍵"]
  ["wwu" "⍹"]
  ["a" "⍺"]
  ["aw" "⍺⍵"]
  ["wa" "⍵⍺"]
  ["aa" "⍺⍺"]
  ["aau" "⍶"]
  ["in" "∊"] ; small element of; enlist
  ["enl" "∊"] ; small element of; enlist
  ["ele" "∊"] ; small element of; enlist
  ["e'" "∊"] ; to avoid issue with e.g., "Xe"→'
  ["eu" "⍷"]
  ["fin" "⍷"]; find
  ["to" "*"]; exponent
  ["flr" "⌊"] ; floor
  ["min" "⌊"]
  ["cei" "⌈"] ; ceiling
  ["max" "⌈"]
  ["mod" "∣"]
  ["mo" "∣"]
  ["inc'" "∆"] ; increment
  ["enc" "⊤"] ; encode -- enc already taken by enclose (more common)
  ["To" "⊤"] ; encode -- enc already taken by enclose (more common)
  ["TO" "⊤"] ; encode -- enc already taken by enclose (more common)
  ["ba" "⊤"] ; "base" -- encode -- enc already taken by enclose (more common)
  ["bas" "⊤"] ; "base" -- encode -- enc already taken by enclose (more common)
  ["dec" "⊥"] ; decode
  ["fr" "⊥"] ; "from" -- decode
  ["fro" "⊥"] ; "from" -- decode
  ["ant" "⊥"] ; "anti-base" -- decode
  ["lef" "⊣"] ; left
  ["lf" "⊣"] ; left
  ["ri" "⊢"] ; right
  ["rig" "⊢"] ; right
  ;; ["th" "⊣"] ; tack left (vim keys) TODO: activate after mapping kmonad
  ;; ["tl" "⊢"] ; tack right (vim keys) TODO: activate after mapping kmonad
  ["mad" "⌹"] ; matrix divide
  ["mdi" "⌹"] ; matrix divide
  ["mdiv" "⌹"] ; matrix divide
  ["minv" "⌹"] ; matrix inverse
  ["ci" "○"] ; pi-times, circular fns
  ["cir" "○"] ; pi-times, circular fns
  ["OO" "○"] ; pi-times, circular fns
  ["o;" "○"] ; pi-times, circular fns
  ["o." "∘."] ; outer product
  ["out" "⍜"] ; outer product
  ["out'" "∘."] ; table (outer product)
  ["tbl" "⍜"] ; table (outer product)
  ["!!" "‼"]
  ["ds" "‼"] ; double shriek
  ["xx" "‼"] ; exclamation exclamation
  ["ro" "⌽"]
  ["rot" "⌽"]
  ["rof" "⊖"] ; rotate first
  ["rf" "⊖"] ; rotate first
  ["fl" "⊖"] ; flip
  ["tr" "⍉"] ; transpose
  ["tra" "⍉"] ; transpose
  ["\\" "⍀"] ; backslash bar; single backslash; escaped here
  ["\\b" "⍀"] ; backslash bar; single backslash; escaped here
  ["⍀" "\\"] ; single backslash; escaped here
  ["exp" "⍀"] ; expand
  ["expa" "⍀"] ; expand
  ["/" "⌿"] ; slash bar
  ["⌿" "/"]
  ["/b" "⌿"] ; slash bar
  ["rp" "⌿"] ; replicate
  ["rep" "⌿"] ; replicate
  ["ref" "⌿"] ; replicate first
  ["gu" "⍋"]
  ["ug" "⍋"]
  ["gd" "⍒"]
  ["dg" "⍒"]
  ["gru" "⍋"]
  ["grd" "⍒"]
  ["ugr" "⍋"]
  ["dgr" "⍒"]
  ["zil" "⍬"] ; zilde
  ["zi" "⍬"]
  ["0v" "⍬"]
  ["," "⍪"] ; comma bar
  [",b" "⍪"] ; comma bar
  [",u" "⍪"]
  ;; [";" "⍮"]; taken by ⋄
  [";b" "⍮"]
  [";u" "⍮"]
  ["fmt" "⍕"]
  ["fm" "⍕"]
  ["ft" "⍕"]
  ["exc" "⍎"]
  ["xc" "⍎"]
  ["ev" "⍎"]
  ["eva" "⍎"]
  ["IB" "⌶"]
  ["ib" "⌶"]
  ["::" "¨"] ; each
  ["ea" "¨"] ; each
  ["nan" "⍲"]
  ["nor" "⍱"]
  ["pow" "⍣"]
  ["pwr" "⍣"]
  ["pw" "⍣"]
  ["po" "⍣"]
  ["sel" "⍨"] ; selfie TODO: change back to "se" once I'm using kmonad for home-row numbers
  ["sese" "⍨⍨"]
  ;; ["ra" "⍤"] ; rank
  ["rk" "⍤"] ; rank
  ["rnk" "⍤"] ; rank
  ["dia" "⋄ "]
  ["sep" "⋄ "] ; separator
  ["no" "~"]
  ["wo" "~"]
  ["ov" "⍥"] ; Over
  ["ove" "⍥"] ; Over
  ["dp" "⍥"] ; Depth
  ["de" "⍥"] ; Depth
  ["dep" "⍥"] ; Depth
  ["o" "∘"]
  ;; ["o_" "⍛"]
  ["oo" "∘"]
  ["ou" "⍛"]
  ;; ["._" "⍛"]
  ;; [".u" "⍛"]
  ;; [".r" "⍛"]
  ["rh" "⍛"] ; reverse hook
  ["rc" "⍛"] ; reverse compose
  ;; ["rwi" "⍛"] ; reverse withe
  ;; ["rw" "⍛"] ; reverse withe
  ;; ["r." "⍛"]
  ["wi" "⍩"]
  ["wit" "⍩"] ; withe
  [">:" "⍩"]
  ["el" "ᑈ"]; each-right (vim keys)
  ["eachr" "ᑈ"]; each-right (vim keys)
  ["<:" "ᑈ"]
  ["eh" "ᐵ"]; each-left (vim keys)
  ["eachl" "ᐵ"]; each-left (vim keys)
  ["b:" "⍠"] ; variant
  ["b;" "⍠"] ; variant
  ["bh" "⍄"] ; "box h"
  ["bl" "⍃"] ; "box l"
  ["va" "⍠"] ; variant
  ["var" "⍠"] ; variant
  ["sys" "⎕"]
  ["bo" "⎕"] ; box
  ["inp" "⎕"] ; input
  ["ato" "⍤"]
  ["atop" "⍤"]
  ["ind" "⌷"]; index
  ["mate" "⌷"]; materialize
  ["sho" "⌷"]; materialize
  ["show" "⌷"]; materialize
  ["disp" "⌷"]; materialize
  ["key" "⌸"]
  ["ke" "⌸"]
  ["b'" "⍞"];
  ["ste" "⌺"]
  ["stn" "⌺"]
  ["sten" "⌺"]
  ["ms" "⍦"]; multi-set in NARS2000
  ["symd" "§"]; NARS2000 symmetric set difference ("Section" symbol)
  ["SY" "§"]; NARS2000 symmetric set difference ("Section" symbol)
  ["syd" "§"]; NARS2000 symmetric set difference ("Section" symbol)
  ["sym" "§"]; NARS2000 symmetric set difference ("Section" symbol)
  ["sd" "§"]; NARS2000 symmetric set difference ("Section" symbol)
  ["mer" "⍈"]; merge in NARS2000 APL
  ["me" "⍈"]; merge in dzaima/APL
  ["mrg" "⍈"]; merge in dzaima/APL
  ["mr" "⍈"]; merge in dzaima/APL
  ["mg" "⍈"]; merge in dzaima/APL
  ["cin" "⍧"]; count-in in dzaima/APL
  ["ci" "⍧"]; count-in in dzaima/APL
  ;; ["um" "≠"]; unique mask in Dyalog APL
  ["ns" "≠"]; nub sieve in Dyalog APL
  ["us" "≠"]; unique sieve in Dyalog APL
  ["ns'" "⍧"]; nub sieve in dzaima/APL
  ;; ["um'" "⍧"]; unique mask in dzaima/APL
  ["us'" "⍧"]; unique sieve in dzaima/APL
  ["pri" "⍭"]; primes in Extended Dyalog
  ["pr" "⍭"]; primes in Extended Dyalog
  ["und" "⍢"]; under
  ["T:" "⍡"]; cumulative repeat
  ["t:" "⍡"]; cumulative repeat
  ["cu" "⍡"]; cumulative repeat
  ["cur" "⍡"]; cumulative repeat
  ["tran" "⍑"]; transform
  ["tb" "⍑"]; T bar
  ["Tb" "⍑"]; T bar
  ["ob" "⍫"]; obverse
  ["obv" "⍫"]; obverse
  ["inv" "⍣¯1"]; inverse
  ["rec" "∇"];
  ["VV" "∇"]
  ["VVV" "∇∇"]
  ["rec'" "∇∇"]
  ["ac" "   ⍝ "] ; APL comment
  ["ac'" "⍝ "] ; APL comment
  ["aco" "   ⍝ "] ; APL comment
  ["aco'" "⍝ "] ; APL comment
  ["ninf" "¯∞"]; neg. infinity
  ["dfns" "⌂"]; dfns workspace
  ["ho" "⌂"]
  ["hou" "⌂"]
  ["'b" "⍘"]
  ["wor" "⍘"] ; words
  ["b/" "⍁"]
  ["diag" "⍁"]
  ["obl" "⍁"] ; "oblique (diagonals)" (J vb name)
  ["b\\" "⍂"]
  ["ce" "⍤¯1"] ; Cells
  ["ce'" "˘"] ; Cells
  ["val" "⊘"] ; Valences
  ["md" "⊘"] ; monad-dyad
  ["fc" "⊏"] ; First Cell
  ;; ["bhe" "⊑"] ; BQN Head
  ;; ["bpic" "⊑"] ; BQN Pick
  ["bsel" "⊏"] ; BQN Select
  ["cla" "⊐"] ; Classify (BQN)
  ["bi" "⊐"] ; BQN Index Of
  ["bio" "⊐"] ; BQN Index Of
  ["pin" "⊒"] ; Progressive Index Of——BQN
  ["pio" "⊒"] ; Progressive Index Of——BQN
  ["oc" "⊒"] ; Occurrence Count——BQN
  ["occ" "⊒"] ; Occurrence Count——BQN
  ["gr" "⊔"] ; Group
  ["grp" "⊔"] ; Group
  ["inv''" "⁼"] ; inverse
  ["heq" "⁼"] ; high equals
  ["he" "⁼"] ; high equals


  ["off" "⎕OFF"]
  ["nums" "⎕AVU"]
  ["numvec" "⎕AVU"]
  ["suits" "♠♡♢♣"]
  ["cards" "♠♡♢♣"]
  ["spade" "♠"]
  ["heart" "♡"]
  ["diamond" "♢"]
  ["club" "♣"]
  ["suitsw" "♤♡♢♧"]
  ["cardsw" "♤♡♢♧"]
  ["suitsb" "♠♥♦♣"]
  ["cardsb" "♠♥♦♣"]

  ["IO" "⎕IO"]
  ["IO0" "⎕IO←0"]
  ["IO1" "⎕IO←1"]
  ["adl" "⍝--------------------"] ; for separating lines of code which would be incorrectly formatted by adoc if adjacent
  ["apld" "⎕IO←0\n]box on -style=max\n]rows on\n"]
  ["aplsetup" "⎕IO←0\n]box on -style=max\n]rows on\n"]
  ["boxoff" "]box off"]
  ["boxnon" "]box on -style=non"]
  ["box0" "]box on -style=non"]
  ["boxmin" "]box on -style=min"]
  ["box1" "]box on -style=min"]
  ["boxmid" "]box on -style=mid"]
  ["box2" "]box on -style=mid"]
  ["boxmax" "]box on -style=max"]
  ["box3" "]box on -style=max"]
  ["ss" "      "]; six spaces

  ;; ---- idioms ------
  ["ce" "⍤¯1"]; Cells
  ["cr" "⍤99 ¯1"]; Cells right
  ["cl" "⍤¯1 99"]; Cells left
  ["er" "⍤99 0"]; Each right
  ["el" "⍤0 99"]; Each left
  ["ax" "⍳≢⍴"]; axes
  ["ax'" "⍳∘≢∘⍴⍤"]; axes ;⍳∘≢∘⍴⍤⊢ ;⍳∘≢∘⍴⍤⊣
  ["tai" "1↓"]; tail (Haskell semantic)
  ["tl" "1↓"]; tail (Haskell semantic)
  ["tai'" "(1∘↓)"]; tail (Haskell semantic)
  ["tl'" "(1∘↓)"]; tail (Haskell semantic)
  ["beh" "1↓"]; behead
  ["be" "1↓"]; behead
  ["beh'" "(1∘↓)"]; behead
  ["be'" "(1∘↓)"]; behead
  ["last" "¯1↑"]; last
  ["las" "¯1↑"]; last
  ["la" "¯1↑"]; last
  ["last'" "(¯1∘↑)"]; last
  ["las'" "(¯1∘↑)"]; last
  ["la'" "(¯1∘↑)"]; last
  ["end" "¯1↑"]; last
  ["curt" "¯1↓"]; curtail
  ["drla" "¯1↓"]; "drop last"
  ["sq" "*2"]; square
  ["sq'" "*∘2"]; square
  ["root" "*0.5"]
  ["roo" "*0.5"]
  ["root'" "(*∘0.5)"]
  ["roo'" "(*∘0.5)"]
  ["dou" "×2"]; double
  ["dou'" "(×∘2)"]; double
  ["inc" "+1"]; increment
  ["inc'" "(+∘1)"]; increment
  ["decr" "-1"]; decrement
  ["decr'" "(-∘1)"]; decrement
  ["halve" "÷2"]; halve
  ["hal" "÷2"]; halve
  ["halve'" "(÷∘2)"]; halve
  ["hal'" "(÷∘2)"]; halve
  ["beh'" "(1∘↓)"]; behead
  ["be'" "(1∘↓)"]; behead
  ["end'" "(¯1∘↑)"]; last
  ["curt'" "(¯1∘↓)"]; curtail
  ["cur'" "(¯1∘↓)"]; curtail
  ["drla'" "(¯1∘↓)"]; "drop last"
  ["sq'" "(*∘2)"]; square
  ["root'" "(*∘0.5)"]
  ["roo'" "(*∘0.5)"]
  ["dou'" "(×∘2)"]; double
  ["incr'" "(+∘1)"]; increment
  ["inc'" "(+∘1)"]; increment
  ["decr'" "(-∘1)"]; decrement
  ["halve'" "(÷∘2)"]; halve
  ["hal'" "(÷∘2)"]; halve
  ["avg" "(+/÷≢)"]
  ["mean" "(+/÷≢)"]
  ["mea" "(+/÷≢)"]
  ["pai" "{⍺⍵}"]; pair
  ["pa" "{⍺⍵}"]; pair
  ["wor" "(≠⊆⊢)"]; words
  ;; ["words" "{1↓¨(1+' '=⍵)⊂⍵}"]
  ;; ["wor" "{1↓¨(1+' '=⍵)⊂⍵}"]
  ;; ["⍘" "{1↓¨(1+' '=⍵)⊂⍵}"]
  ;; ["words'" "(1↓¨(1∘+)∘(' '∘=)⊂⊢)"] ; tacit (monadic)
  ;; ["wor'" "(1↓¨(1∘+)∘(' '∘=)⊂⊢)"] ; tacit (monadic)
  ;; ["⍘'" "{1↓¨(1∘+)∘(' '∘=)⍛⊂}"] ; tacit (monadic)
  ["ra" "≢⍴"]; rank of A
  ["ra'" "≢∘⍴"]; rank of A


])


(xah-math-input--add-to-hash
 '(
   ;; ["<" "≺ "]
   ;; [">" "≻ "]

   ["<=" "≤ "]
   [">=" "≥ "]
   ;; ["!el" "∉ "]
   ;; ["el" "∈ "]
   ;; ["in" "∈ "]
   ["&&" "∧ "]
   ["||" "∨ "]
   ;; ["not" "¬ "]
   ;; ["===" "≡ "]
   ;; ["eq" "≡ "]
   ;; ["xor" "⊻ "]
   ;; ["nand" "⊼ "]
   ;; ["nor" "⊽ "]

   ["~" "≈"]
   [":=" "≔"]
   ["=:" "≕"]
   ["!=" "≠"]
   ["/=" "≠"]
   ["ne" "≠"]

   ;; ["fa" "∀ "] ["forall" "∀ "]
   ;; ["ex" "∃ "]

   ))


(xah-math-input--add-to-hash
 '(
   ["/_" "∠ "] ;ANGLE
   ["rightangle" "⦜ "]
   ["|_" "⦜ "]
   ))


(xah-math-input--add-to-hash
 [
 	;; misc. unicode
	["em" "—"]
	["emda" "—"]
	["en" "–"]
	["enda" "–"]
	["mm" "——"]; double em-dash
	["line" "———————————————"]
	["bul" "• "]
	["alb" "↤"]
	["from" "↤"]
	["adb" "↧"]
  ["'" "`"] ; single back-tick
  [";;" "``"] ; back-tick pair



])



(xah-math-input--add-to-hash
 [

  ;; J language

  ;; ["ex." "!."]
  ;; ["ex:" "!:"]
  ;; ["at:" "@:"]
  ;; ["h." "#."]
  ;; ["h:" "#:"]
  ;; ["ha." "#."]
  ;; ["ha:" "#:"]
  ;; ["do." "$."]
  ;; ["do:" "$:"]
  ;; ["S." "$."]
  ;; ["S:" "$:"]
  ;; ["pe." "%."]
  ;; ["pe:" "%:"]

  ;; ["/" "%"]

  ["jd" "%"] ; J divide

  ;; ["jd." "%."] ; J divide
  ;; ["jd:" "%:"] ; J divide

  ;; ["/." "%."]
  ;; ["/:" "%:"]

  ;; ["ca." "^."]
  ;; ["ca:" "^:"]
  ;; ["u." "^."]
  ;; ["u:" "^:"]
  ;; ["up." "^."]
  ;; ["up:" "^:"]
  ;; ["am." "&."]
  ;; ["am:" "&:"]
  ;; ["am.:" "&.:"]
  ;; ["as." "*."]
  ;; ["as:" "*:"]
  ;; ["st." "*."]
  ;; ["st:" "*:"]
  ;; ["m." "-."]
  ;; ["m:" "-:"]
  ;; ["mi." "-."]
  ;; ["mi:" "-:"]
  ;; ["p." "+."]
  ;; ["p:" "+:"]
  ;; ["pl." "+."]
  ;; ["pl:" "+:"]
  ;; ["e." "=."]
  ;; ["e:" "=:"]
  ;; ["ch." "{."]
  ;; ["ch:" "{:"]
  ;; ["cl." "}."]
  ;; ["cl:" "}:"]
  ;; ["sh." "[."]
  ;; ["sh:" "[:"]
  ;; ["sl." "]."]
  ;; ["sl:" "]:"]

  ;; ["bh" "["] ; bracket
  ;; ["bl" "]"]

  ["sh" "["] ; square bracket
  ["sl" "]"]
  ["ch" "{"] ; curly
  ["cl" "}"] ; curly
  ["jl" "{{}}"] ; lambda
  ["is'" "=:"] ; J comment
  ["is''" "=."] ; J comment
  ["jis" "=:"] ; J comment
  ["jisl" "=."] ; J comment
  ["jc" "   NB. "] ; J comment
  ["jc'" "NB. "] ; J comment
  ["nb" "   NB. "] ; J comment
  ["nb'" "NB. "] ; J comment

])




(xah-math-input--add-to-hash
 [

	;; Racket
	;; ["ht" "#t"] ; t is taken by "    " for tabbing
	;; ["f" "#f"]
	;; ["emp" "'()"]
	;; ["em" "empty"]
	;; ["emp" "empty"]
	["csae" "case"] ; catch typo
	["casea" "case->"]
	["csaea" "case->"] ; catch typo
	["casel" "case-λ"]
	["caseL" "case-λ"]
	["csael" "case-λ"] ; catch typo
	["csaeL" "case-λ"] ; catch typo
	;; ["les" "<= "]
	;; ["gre" ">= "]
	;; ["hr" "hash-ref "]
	;; ["lr" "list-ref "]
	;; ["pick" "list-ref "]
	;; ["not" "not "]
	["neg" "negative? "]
	["pos" "positive? "]
	;; ["and" "and "]
	;; ["or" "or "]
	;; ["nor" "nor "]
	;; ["nand" "nand "]
	;; ["xor" "xor "]
	;; ["abs" "abs "]
	;; ["rem" "remove "]
	;; ["rems" "remove* "]
	;; ["rem*" "remove* "]
	["fll" "foldl "]
	["frr" "foldr "]
	;; ["eq" "equal? "]
	;; ["neq" "not-equal? "]
	;; ["p" "length "] ; rho
	;; ["i" "build-list "] ; iota
	;; ["j" "compose1 "]
	;; ["j_" "compose "]
	;; ["flip" "flip "]
	;; ["rev" "reverse "]
	;; ["rot" "reverse "]
	;; ["fmt" "format "]
	;; ["all" "all "]
	;; ["exi" "findf "]
	;; ["any" "findf "]
	;; ["some" "findf "]
	;; ["nex" "none "] ; "not exists"
	;; ["none" "none "]
	;; ["in" "member "]
	;; ["mem" "member "]
	;; ["fm" "filter-map "] ; filter-map
	;; ["mf" "map-filter "] ; map-filter
	["eflr" "exact-floor "] ; result is Integer
	["eflo" "exact-floor "] ; result is Integer
	["ecei" "exact-ceiling "] ; result is Integer
	["flrr" "floor "] ; result is Real
	["ceir" "ceiling "] ; result is Real
	;; ["fil" "filter "]
	;; ["fno" "filter-not "]
	;; ["repl" "replicate "]
	;; ["cat" "concat "]
	;; ["ap" "append "]
	;; ["zipw" "zip-with "]
	;; ["zw" "zip-with "]
	;; ["ir" "in-range? "]
	["lsit" "list "] ; catch typo
	["list" "list "] ; catch typo
	;; ["let" "let* "]

])


(xah-math-input--add-to-hash
 [

		;; --- Types ---
		;; If it's not parameterized (Integer, Natural, Boolean, etc), it has a double-struck letter.
		;; If it is parameterized, it has a bold letter.
		;;     Normal parameterized types (Listof, Vectorof, Pairof, HashTable, etc) are slanted.
		;;     Fixed parameterized types, akin to tuples (List, Vector, Pair) are upright
		;; ADT constructors, (Just, True, etc) are slanted but not bold
		;; The exception is Symbol, which is bold/italic, while String is double-struck, in order to differentiate the two.

	;; ["JS" "JSExpr "]
	;; ["JH" "JSHash "]
	;; ["O" "𝑴 "]
	;; ["M" "𝑴 "]
	;; ["JU" "𝐽 "]
	;; ["NO" "𝑁 "]
	;; ["EI" "𝑬 "]
	;; ["RI" "𝑅 "]
	;; ["LE" "𝐿 "]
	;; ["T" "𝑻 "]
	;; ["F" "𝑭 "]
	;; ["B" "𝔹 "]
	;; ["N" "ℕ "]
	;; ["IN" "𝕀 "]
	;; ["NU" "ℂ "]
	;; ["CO" "ℂ "]
	;; ["COM" "ℂ "]
	;; ["Z-" "ℤ⁻ "]
	;; ["Z-0" "ℤ⁰⁻ "]
	;; ["Z" "ℤ "]
	;; ["Z+0" "ℤ⁰⁺ "]
	;; ["Z+" "ℤ⁺ "]
	;; ["R-" "ℝ⁻ "]
	;; ["R-0" "ℝ⁰⁻ "]
	;; ["R" "ℝ "]
	;; ["R+0" "ℝ⁰⁺ "]
	;; ["R+" "ℝ⁺ "]
	;; ["Q-" "ℚ⁻ "]
	;; ["Q-0" "ℚ⁰⁻ "]
	;; ["Q" "ℚ "]
	;; ["Q+0" "ℚ⁰⁺ "]
	;; ["Q+" "ℚ⁺ "]
	;; ["Fl-" "Fl⁻ "]
	;; ["Fl-0" "Fl⁰⁻ "]
	;; ["Fl" "Fl "]
	;; ["Fl+0" "Fl⁰⁺ "]
	;; ["Fl+" "Fl⁺ "]
	;; ["L" "𝑳 "]
	;; ["L'" "𝗟 "]
	;; ["LF" "𝗟 "]
	;; ["V" "𝑽 "]
	;; ["V'" "𝗩 "]
	;; ["VF" "𝗩 "]
	;; ["PA" "𝐏 "]
	;; ["STR" "𝕊 "]
	;; ["SY" "𝑺 "]
	;; ["H" "𝑯 "]
	;; ["HI" "𝑯i "]
	;; ["HM" "𝑯m "]
	;; ["A" "𝔸 "]
	;; ["L^" "𝑳^ "]
	;; ["Z-" "ℤ⁻ "]
	;; ["Z0-" "ℤ⁰⁻ "]
	;; ["Z" "ℤ "]
	;; ["Z0+" "ℤ⁰⁺ "]
	;; ["Z+" "ℤ⁺ "]
	;; ["R-" "ℝ⁻ "]
	;; ["R0-" "ℝ⁰⁻ "]
	;; ["R" "ℝ "]
	;; ["R0+" "ℝ⁰⁺ "]
	;; ["R+" "ℝ⁺ "]
	;; ["Q-" "ℚ⁻ "]
	;; ["Q0-" "ℚ⁰⁻ "]
	;; ["Q" "ℚ "]
	;; ["Q0+" "ℚ⁰⁺ "]
	;; ["Q+" "ℚ⁺ "]
	;; ["Fl-" "Fl⁻ "]
	;; ["Fl0-" "Fl⁰⁻ "]
	;; ["Fl" "Fl "]
	;; ["Fl0+" "Fl⁰⁺ "]
	;; ["Fl+" "Fl⁺ "]
	;; ["ZN" "ℤ⁻ "]
	;; ["ZN0" "ℤ⁰⁻ "]
	;; ["Z" "ℤ "]
	;; ["ZP0" "ℤ⁰⁺ "]
	;; ["ZP" "ℤ⁺ "]
	;; ["RN" "ℝ⁻ "]
	;; ["RN0" "ℝ⁰⁻ "]
	;; ["R" "ℝ "]
	;; ["RP0" "ℝ⁰⁺ "]
	;; ["RP" "ℝ⁺ "]
	;; ["QN" "ℚ⁻ "]
	;; ["QN0" "ℚ⁰⁻ "]
	;; ["Q" "ℚ "]
	;; ["QP0" "ℚ⁰⁺ "]
	;; ["QP" "ℚ⁺ "]
	;; ["FLN" "Fl⁻ "]
	;; ["FLN0" "Fl⁰⁻ "]
	;; ["FL" "Fl "]
	;; ["FLP0" "Fl⁰⁺ "]
	;; ["FLP" "Fl⁺ "]

	;; ;; overrides
	;; ["OP" "Option "]
	;; ["M" "Maybe "]
	;; ["JU" "Just "]
	;; ["NO" "Nothing "]
	;; ["EI" "Either "]
	;; ["RI" "Right "]
	;; ["LE" "Left "]
	;; ["A" "Any "]
	;; ["T" "True "]
	;; ["F" "False "]
	;; ["B" "Boolean "]
	;; ["N" "Natural "]
	;; ["IN" "Index "]
	;; ["NU" "Number "]
	;; ["CO" "Complex "]
	;; ["COM" "Complex "]
	;; ["Z" "Integer "]
	;; ["R" "Real "]
	;; ["Q" "Exact-Rational "]
	;; ["FL" "Float "]
	;; ["L" "Listof "]
	;; ["L'" "List "]
	;; ["VE" "Vectorof "]
	;; ["VE'" "Vector "]
	;; ["PA" "Pair "]
	;; ["PS" "Path-String "]
	;; ["ST" "String "]
	;; ["SY" "Symbol "]
	;; ["HT" "HashTable "]
	;; ["HA" "HashTable "]
	;; ["HI" "ImmutableHashTable "]
	;; ["HM" "MutableHashTable "]
	;; ["L^" "Non-Empty-List "]
	;; ["JS" "JSExpr "]
	;; ["JH" "JSHash "]


		;; ----------------------------------------------

	;; ["M" "𝑴 "]
	;; ["JU" "𝐽 "]
	;; ["NO" "𝑁 "]
	;; ["EI" "𝑬 "]
	;; ["RI" "𝑅 "]
	;; ["LE" "𝐿 "]
	;; ["T" "𝑻 "]
	;; ["F" "𝑭 "]
	;; ["B" "𝐁 "]
	;; ["O" "𝑴 "]
	;; ["N" "𝐍 "]
	;; ["IN" "𝐈 "]
	;; ["NU" "ℂ "]
	;; ["CO" "ℂ "]
	;; ["COM" "ℂ "]
	;; ["ZN" "𝐙⁻ "]
	;; ["ZN0" "𝐙⁰⁻ "]
	;; ["Z" "𝐙 "]
	;; ["ZP0" "𝐙⁰⁺ "]
	;; ["ZP" "𝐙⁺ "]
	;; ["L" "𝑳 "]
	;; ["L'" "𝗟 "]
	;; ["V" "𝑽 "]
	;; ["V'" "𝗩 "]
	;; ["PA" "Pair "]
	;; ["RN" "𝐑⁻ "]
	;; ["RN0" "𝐑⁰⁻ "]
	;; ["R" "𝐑 "]
	;; ["RP0" "𝐑⁰⁺ "]
	;; ["RP" "𝐑⁺ "]
	;; ["FLN" "Fl⁻ "]
	;; ["FLN0" "Fl⁰⁻ "]
	;; ["FL" "Fl "]
	;; ["FLP0" "Fl⁰⁺ "]
	;; ["FLP" "Fl⁺ "]
	;; ["ST" "𝕊 "]
	;; ["SY" "𝑺 "]
	;; ["H" "𝑯 "]
	;; ["HI" "𝑯i "]
	;; ["HM" "𝑯m "]
	;; ["A" "𝐀 "]
	;; ["QN" "𝐐⁻ "]
	;; ["QN0" "𝐐⁰⁻ "]
	;; ["Q" "𝐐 "]
	;; ["QP0" "𝐐⁰⁺ "]
	;; ["QP" "𝐐⁺ "]
	;; ["L^" "𝑳^ "]
	;; ["JS" "JSExpr "]
	;; ["JH" "JSHash "]

	;; ----------------------------------------------


	;; ["M" "Maybe "]
	;; ["JU" "Just "]
	;; ["NO" "Nothing "]
	;; ["EI" "Either "]
	;; ["RI" "Right "]
	;; ["LE" "Left "]
	;; ["T" "True "]
	;; ["F" "False "]
	;; ["B" "Boolean "]
	;; ["OP" "Option "]
	;; ["N" "Natural "]
	;; ["IN" "Index "]
	;; ["In" "Index "]
	;; ["NU" "Number "]
	;; ["CO" "Complex "]
	;; ["COM" "Complex "]
	;; ["Z-" "Negative-Integer "]
	;; ["Z0-" "Nonpositive-Integer "]
	;; ["Z" "Integer "]
	;; ["Z0+" "Nonnegative-Integer "]
	;; ["Z+" "Positive-Integer "]
	;; ["R-" "Negative-Real "]
	;; ["R0-" "Nonpositive-Real "]
	;; ["R" "Real "]
	;; ["R0+" "Nonnegative-Real "]
	;; ["R+" "Positive-Real "]
	;; ["FL-" "Negative-Float "]
	;; ["FL0-" "Nonpositive-Float "]
	;; ["FL" "Float "]
	;; ["FL0+" "Nonnegative-Float "]
	;; ["FL+" "Positive-Float "]
	;; ["Q-" "Negative-Exact-Rational "]
	;; ["Q0-" "Nonpositive-Exact-Rational "]
	;; ["Q" "Exact-Rational "]
	;; ["Q0+" "Nonnegative-Exact-Rational "]
	;; ["Q+" "Positive-Exact-Rational "]
	;; ["L" "Listof "]
	;; ["L'" "List "]
	;; ["VE" "Vectorof "]
	;; ["VE'" "Vector "]
	;; ["PA" "Pair "]
	;; ["ST" "String "]
	;; ["SY" "Symbol "]
	;; ["HT" "HashTable "]
	;; ["HI" "ImmutableHashTable "]
	;; ["HM" "MutableHashTable "]
	;; ["L^" "Non-Empty-List "]
	;; ["JS" "JSExpr "]
	;; ["JH" "JSHash "]
	;; ["Z-" "Negative-Integer "]
	;; ["Z-0" "Nonpositive-Integer "]
	;; ["Z" "Integer "]
	;; ["Z+0" "Nonnegative-Integer "]
	;; ["Z+" "Positive-Integer "]
	;; ["R-" "Negative-Real "]
	;; ["R-0" "Nonpositive-Real "]
	;; ["R" "Real "]
	;; ["R+0" "Nonnegative-Real "]
	;; ["R+" "Positive-Real "]
	;; ["FL-" "Negative-Float "]
	;; ["FL-0" "Nonpositive-Float "]
	;; ["FL" "Float "]
	;; ["FL+0" "Nonnegative-Float "]
	;; ["FL+" "Positive-Float "]
	;; ["Q-" "Negative-Exact-Rational "]
	;; ["Q-0" "Nonpositive-Exact-Rational "]
	;; ["Q" "Exact-Rational "]
	;; ["Q+0" "Nonnegative-Exact-Rational "]
	;; ["Q+" "Positive-Exact-Rational "]
	;; ["ZN" "Negative-Integer "]
	;; ["ZN0" "Nonpositive-Integer "]
	;; ["Z" "Integer "]
	;; ["ZP0" "Nonnegative-Integer "]
	;; ["ZP" "Positive-Integer "]
	;; ["RN" "Negative-Real "]
	;; ["RN0" "Nonpositive-Real "]
	;; ["R" "Real "]
	;; ["RP0" "Nonnegative-Real "]
	;; ["RP" "Positive-Real "]
	;; ["FLN" "Negative-Float "]
	;; ["FLN0" "Nonpositive-Float "]
	;; ["FL" "Float "]
	;; ["FLP0" "Nonnegative-Float "]
	;; ["FLP" "Positive-Float "]
	;; ["QN" "Negative-Exact-Rational "]
	;; ["QN0" "Nonpositive-Exact-Rational "]
	;; ["Q" "Exact-Rational "]
	;; ["QP0" "Nonnegative-Exact-Rational "]
	;; ["QP" "Positive-Exact-Rational "]

	])



(xah-math-input--add-to-hash
 [
	["deg" "°"]
  ["micro" "µ"]
  ["mdot" "·"]
  ["1/4" "¼"]
  ["1/2" "½"]
  ["3/4" "¾"]

  ["Theta" "Θ"] ["Lambda" "Λ"] ["Xi" "Ξ"] ["Phi" "Φ"] ["Psi" "Ψ"] ["Omega" "Ω"]

  ["beta" "β"] ["delta" "δ"] ["epsilon" "ε"] ["zeta" "ζ"] ["eta" "η"] ["theta" "θ"] ["mu" "μ"] ["xi" "ξ"] ["sigmaf" "ς"] ["tau" "τ"] ["phi" "φ"] ["psi" "ψ"] ["theta'" "ϑ"] ["upsih" "ϒ"] ["piv" "ϖ"]

  ["ndash" "–"] ["mdash" "—"]

  ["times" "×"] ["divide" "÷"] ["minus" "− "] ["lowast" "∗ "] ["radic" "√"]
  ["oplus" "⊕"] ["otimes" "⊗"] ["ox" "⊗"]
  ["oslash" "ø"]
  ["fnof" "ƒ"]

  ;; ["dp" "∂"] ;; already mapped to 'd'

	["ang" "∠"]

  ["perp" "⊥"] ["bot" "⊥"] ["top" "⊤"] ["dop" "⋅"] ; dot operator

  ["ceih" "⌈"] ["ceil" "⌉"] ["flrh" "⌊"] ["flrl" "⌋"]

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
  [".." "…"] ; ellipsis
  ["..." "…"] ; ellipsis
  ["ell" "…"] ; ellipsis
  ["ran" "…"]
  ["rng" "…"]
  ["rn" "…"]
  ["rg" "…"]
  ["fdash" "‒"]
  ["wdash" "〜"]
  ["----" "——"] ; double m-dash
  ["---" "—"] ; em-dash
  ["--" "–"] ; en-dash
  ;; ["??" "⁇ "]
  ;; ["?!" "⁈ "]
  ;; ["!?" "⁉ "]
  ;; ["!!" "‼ "]

  ;;
  ]

 )

(xah-math-input--add-to-hash
 [
  ["m2" "㎡"]
  ["cm'" "㎝"]
  ["cm2" "㎠"]
  ["cm3" "㎤"]
  ] )


(xah-math-input--add-to-hash
 [
  ;; superscripts


  ["0" "⁰"]
  ["1" "¹"]
  ["2" "²"]
  ["3" "³"]
  ["4" "⁴"]
  ["5" "⁵"]
  ["6" "⁶"]
  ["7" "⁷"]
  ["8" "⁸"]
  ["9" "⁹"]

  ["-1" "⁻¹"]
  ["-2" "⁻²"]
  ["-3" "⁻³"]
  ["-4" "⁻⁴"]
  ["-5" "⁻⁵"]
  ["-6" "⁻⁶"]
  ["-7" "⁻⁷"]
  ["-8" "⁻⁸"]
  ["-9" "⁻⁹"]

  ["m1" "⁻¹"]
  ["m2" "⁻²"]
  ["m3" "⁻³"]
  ["m4" "⁻⁴"]
  ["m5" "⁻⁵"]
  ["m6" "⁻⁶"]
  ["m7" "⁻⁷"]
  ["m8" "⁻⁸"]
  ["m9" "⁻⁹"]

  ["+" "⁺"]
  ["-" "¯"] ; macron, not actual superscript minus, since it looks better anyway and this stops us from making booboos in APL
  ["=" "⁼"]
  ["(up" "⁽"]
  [")up" "⁾"]
  ["nup" "ⁿ"]
  ["iup" "ⁱ"]





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
  ["_sch" "ₔ"]; schwa

  ["(_" "₍"]
  [")_" "₎"]
  ["+_" "₊"]
  ["-_" "₋"]
  ["0_" "₀"]
  ["1_" "₁"]
  ["2_" "₂"]
  ["3_" "₃"]
  ["4_" "₄"]
  ["5_" "₅"]
  ["6_" "₆"]
  ["7_" "₇"]
  ["8_" "₈"]
  ["9_" "₉"]
  ["=_" "₌"]
  ["a_" "ₐ"]
  ["e_" "ₑ"]
  ["h_" "ₕ"]
  ["i_" "ᵢ"]
  ["j_" "ⱼ"]
  ["k_" "ₖ"]
  ["l_" "ₗ"]
  ["m_" "ₘ"]
  ["n_" "ₙ"]
  ["o_" "ₒ"]
  ["p_" "ₚ"]
  ["r_" "ᵣ"]
  ["s_" "ₛ"]
  ["t_" "ₜ"]
  ["u_" "ᵤ"]
  ["v_" "ᵥ"]
  ["x_" "ₓ"]
  ["sch_" "ₔ"]; schwa

  ["(d" "₍"] ; "down"
  [")d" "₎"]
  ["+d" "₊"]
  ["-d" "₋"]
  ["0d" "₀"]
  ["1d" "₁"]
  ["2d" "₂"]
  ["3d" "₃"]
  ["4d" "₄"]
  ["5d" "₅"]
  ["6d" "₆"]
  ["7d" "₇"]
  ["8d" "₈"]
  ["9d" "₉"]
  ["=d" "₌"]

  ;; ["(do" "₍"] ; messes with doing "($"; use "(d"
  ;; [")do" "₎"] ; messes with doing ")$"; use ")d"
  ["+do" "₊"]
  ["-do" "₋"]
  ["0do" "₀"]
  ["1do" "₁"]
  ["2do" "₂"]
  ["3do" "₃"]
  ["4do" "₄"]
  ["5do" "₅"]
  ["6do" "₆"]
  ["7do" "₇"]
  ["8do" "₈"]
  ["9do" "₉"]
  ["=do" "₌"]
  ["ado" "ₐ"]
  ["edo" "ₑ"]
  ["hdo" "ₕ"]
  ["ido" "ᵢ"]
  ["jdo" "ⱼ"]
  ["kdo" "ₖ"]
  ["ldo" "ₗ"]
  ["mdo" "ₘ"]
  ["ndo" "ₙ"]
  ["odo" "ₒ"]
  ["pdo" "ₚ"]
  ["rdo" "ᵣ"]
  ["sdo" "ₛ "]
  ["td" "ₜ"]
  ["udo" "ᵤ"]
  ["vdo" "ᵥ"]
  ["xdo" "ₓ"]
  ["schdo" "ₔ"]; schwa

  ["⁰" "₀"]
  ["¹" "₁"]
  ["²" "₂"]
  ["³" "₃"]
  ["⁴" "₄"]
  ["⁵" "₅"]
  ["⁶" "₆"]
  ["⁷" "₇"]
  ["⁸" "₈"]
  ["⁹" "₉"]

  ["⁻¹" "₋₁"]
  ["⁻²" "₋₂"]
  ["⁻³" "₋₃"]
  ["⁻⁴" "₋₄"]
  ["⁻⁵" "₋₅"]
  ["⁻⁶" "₋₆"]
  ["⁻⁷" "₋₇"]
  ["⁻⁸" "₋₈"]
  ["⁻⁹" "₋₉"]

  ["⁺" "₊"]
  ["¯" "₋"] ; macron, not actual superscript minus, since it looks better anyway and this stops us from making booboos in APL
  ["⁼" "₌"]
  ["⁽" "₍"]
  ["⁾" "₎"]
  ])

(xah-math-input--add-to-hash
 '(

   ;; ["flr" "⌊⌋ "]
   ;; ["ceil" "⌈⌉ "]
   ;; ["floor" "⌊⌋ "]
   ;; ["ceiling" "⌈⌉ "]

   ;; ["\"" "“” "] ;curly quote
   ;; ["\"\"" "“” "]

   ;; ["cb" "「」 "] ; corner bracket
   ;; ["[" "「」 "]

   ;; ["[(" "【】 "] ; LEFT BLACK LENTICULAR BRACKET

  ;;   ["tb" "〔〕 "] ; TORTOISE SHELL BRACKET
  ;; ["(" "〔〕 "]

   ))


(xah-math-input--add-to-hash
 '(
   ;; letter-like forms
   ["R2" "ℝ²"]
   ["R3" "ℝ³"]
   ["r2" "ℝ²"]
   ["r3" "ℝ³"]
   ;; ["fn" "ƒ "]
   ))

(xah-math-input--add-to-hash
 '(

   ["<-" "←"] ["->" "→"] ["<->" "↔"] ["!<-" "↚"] ["!->" "↛"] ["!<->" "↮"]
   ;; ["<=" "⇐"] ["=>" "⇒"]
    ["<=>" "⇔ "] ["!<=" "⇍ "] ["!=>" "⇏ "] ["!<=>" "⇎ "]
   ["<==" "⟸ "] ["==>" "⟹ "] ["<==>" "⟺ "]
   ["<-|" "↤"] ["|->" "↦"]
   ["<--" "⟵ "] ["-->" "⟶ "] ["<-->" "⟷ "]
   ;; ["al" "← "]
   ;; ["ar" "→ "]
   ;; ["au" "↑ "]
   ;; ["ad" "↓ "]
   ["-!" "↑ "]
   ["-^" "↑ "]
   ["-v" "↓ "]

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

