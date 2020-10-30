
#lang aful typed/racket
(provide (all-defined-out))
(require (only-in typed/racket
                  [filter-map orig:filter-map] [hash-ref orig:hash-ref]
                  [U ⋃] [∩ ⋂] [Symbol 𝑺] [HashTable 𝑯] [Any 𝔸] [Boolean 𝔹] [Option 𝑴]
                  [Listof 𝑳] [List 𝗟] [True 𝑻] [False 𝑭] [Vectorof 𝑽] [Vector 𝗩] [String 𝕊]
                  [Natural ℕ] [Number ℂ] [Index 𝕀] [Negative-Integer ℤ⁰⁻] [Nonpositive-Integer ℤ⁻] [Integer ℤ] [Nonnegative-Integer ℤ⁰⁺] [Positive-Integer ℤ⁺]
                  [Negative-Float Fl⁻] [Nonpositive-Float Fl⁰⁻] [Float Fl] [Nonnegative-Float Fl⁰⁺] [Positive-Float Fl⁺]
                  [Negative-Exact-Rational ℚ⁻] [Nonnegative-Exact-Rational ℚ⁰⁻] [Negative-Exact-Rational ℚ]
                  [Nonnegative-Exact-Rational ℚ⁰⁺] [Negative-Exact-Rational ℚ⁺]
                  [Negative-Real ℝ⁻] [Nonpositive-Float ℝ⁰⁻] [Real ℝ] [Nonnegative-Real ℝ⁰⁺] [Positive-Real ℝ⁺]
                  [let* ∴] [if ?] [case-lambda case-λ] [and ∧] [or ∨] [nor ⊽] [nand ⊼]
                  [for ∀:] [for* ∀*:] [for/list ∀:l] [for*/list ∀*:l] [for/hash ∀:h] [for*/hash ∀*:h]
                  [for/vector ∀:v] [for*/vector ∀*:v] [for/sum ∀:∑] [for*/sum ∀*:∑]
                  [for/last ∀:last] [for*/last ∀*:last] [for/set ∀:s] [for*/set ∀*:s]
                  [for/hasheq ∀:hq] [for*/hasheq ∀*:hq] [for/hasheqv ∀:hv] [for*/hasheqv ∀*:hv]
                  [for/or ∀:or] [for*/or ∀*:or] [for/and ∀:and] [for*/and ∀*:and]
                  [for/product ∀:∏] [for*/product ∀*:∏] [for/first ∀:1st] [for*/first ∀*:1st]
                  [for/lists ∀:lists] [for*/lists ∀*:lists] [for/fold ∀:fold] [for*/fold ∀*:fold]))
(require cond-strict)
(require (only-in typed-map [map map] [foldl infer:foldl] [foldr infer:foldr]))
;; (require
;;     srfi/87 ;; "=>" in case clauses
;;   srfi/71 ;; extended 'let' syntax for defining multiple names
;;   srfi/61 ;; more general cond clause
;;   srfi/26 ;; pseudo-curry
;;   srfi/31 ;; rec
;;   srfi/2 ;; and-let*
;;   )
;; (require/typed (only-in srfi/1) ) ;; list library


;; (require (only-in typed/racket
;;                   [filter-map orig:filter-map]
;;                   [U ⋃] [∩ ⋂] [Symbol 𝑺] [HashTable 𝑯] [Any 𝔸] [Boolean 𝔹] [Option 𝑴]
;;                   [Listof 𝑳] [List 𝗟] [True 𝑻] [False 𝑭] [Vectorof 𝑽] [Vector 𝗩] [String 𝕊]
;;                   [Natural ℕ] [Number ℂ] [Index 𝕀] [Negative-Integer ℤ⁰⁻] [Nonpositive-Integer ℤ⁻] [Integer ℤ] [Nonnegative-Integer ℤ⁰⁺] [Positive-Integer ℤ⁺]
;;                   [Negative-Float Fl⁻] [Nonpositive-Float Fl⁰⁻] [Float Fl] [Nonnegative-Float Fl⁰⁺] [Positive-Float Fl⁺]
;;                   [Negative-Exact-Rational ℚ⁻] [Nonnegative-Exact-Rational ℚ⁰⁻] [Negative-Exact-Rational ℚ]
;;                   [Nonnegative-Exact-Rational ℚ⁰⁺] [Negative-Exact-Rational ℚ⁺]
;;                   [Negative-Real ℝ⁻] [Nonpositive-Float ℝ⁰⁻] [Real ℝ] [Nonnegative-Real ℝ⁰⁺] [Positive-Real ℝ⁺]
;;                   [let* ∴] [if ?] [case-lambda case-λ] [and ∧] [or ∨] [nor ⊽] [nand ⊼]
;;                   [list-ref ‼] [first head] [rest tail] [cons :] [empty ∅] [length ρ] [build-list ι]
;;                   [sort ⍋] [reverse ⌽] [and ∧] [or ∨] [not ¬] [negate ⌙] [xor ⊻] [nor ⊽] [nand ⊼]
;;                   [append <>] [append* <>^] [string-append ++] [string-append* ++^] [append* concat]
;;                   [map <$>] [foldl /.] [foldr /:] [map →] [filter ⊇] [filter-not ⊉]
;;                   [curry ⫶] [compose1 ∘] [compose1 <<<] [compose ∘^] [identity id]
;;                   [+ ∑] [* ∏] [/ ÷] [sqrt √] [modulo %] [<= ≤] [>= ≥]
;;                   [member ∈] [findf ∃] [take ↑] [drop ↓] [make-list replicate]
;;                   [append-map concat-map] [remove rem-1st] [remove* \\]
;;                   [for ∀:] [for* ∀*:] [for/list ∀:l] [for*/list ∀*:l] [for/hash ∀:h] [for*/hash ∀*:h]
;;                   [for/vector ∀:v] [for*/vector ∀*:v] [for/sum ∀:∑] [for*/sum ∀*:∑]
;;                   [for/last ∀:last] [for*/last ∀*:last] [for/set ∀:s] [for*/set ∀*:s]
;;                   [for/hasheq ∀:hq] [for*/hasheq ∀*:hq] [for/hasheqv ∀:hv] [for*/hasheqv ∀*:hv]
;;                   [for/or ∀:or] [for*/or ∀*:or] [for/and ∀:and] [for*/and ∀*:and]
;;                   [for/product ∀:∏] [for*/product ∀*:∏] [for/first ∀:1st] [for*/first ∀*:1st]
;;                   [for/lists ∀:lists] [for*/lists ∀*:lists] [for/fold ∀:fold] [for*/fold ∀*:fold]
;;                   [first 1st] [second 2nd] [third 3rd] [fourth 4th] [fifth 5th]
;;                   [sixth 6th] [seventh 7th] [eighth 8th] [ninth 9th] [tenth 10th]))


(define-type (Non-Empty-List a) (Pairof a (Listof a)))
(define-type (NEList a) (Non-Empty-List a))
(define-type (𝑳^ a) (Non-Empty-List a))
(define id identity)
(define ≡ equal?)
(define head first)
(define tail rest)
(define ∅ empty)
(define ¬ not)
(define ⊻ xor)
(define ⌈ exact-ceiling)
(define ⌊ exact-floor)
(define ∘ compose1)
(define ∑ +)
(define ∏ *)
(define √ sqrt)
(define % modulo)
(define ∣ abs)
(define ⩽ <=)
(define ⩾ >=)
(define ∈ member)
;; (define ∃ findf) ; replaced by "some"
(define replicate make-list)
(define concat-map append-map)
(define remove-1st remove)
(define 1st first)
(define 2nd second)
(define 3rd third)
(define 4th fourth)
(define 5th fifth)
(define 6th sixth)
(define 7th seventh)
(define 8th eighth)
(define 9th ninth)
(define 10th tenth)
;; -----------------------------------------------------------------------------------
;; ------- plausible-readability-to-others separater -------
;; -----------------------------------------------------------------------------------
(define ⍛ compose)
(define ⌉ ceiling)
(define ⌋ floor)
(define ↑ take)
(define ↓ drop)
(define ⌙ negate)
(define \\ remove*)
(define ↑. first)
(define ↓. rest)
(define ⍴ length)
(define ⌽ reverse)
(define ⍳ build-list)
(define lookup assoc)
(define <> append)
(define <>. append*)
(define ++ string-append)
(define ++. string-append*)
(define ⍋ sort)
(define <$> map)
(define ¨ map)
(define ⊇ filter)
(define ⊉ filter-not)
(define ⍕ format)
(define ⍎ eval)
(define ⫶ curry)
(define <<< compose1)


;; TODO: write ∃ (more different findf) that takes an (𝔸 -> 𝔸) rather than (𝔸 -> 𝔹)
;;       -- this way we can give it e.g. (λ v. ∧ (≡ v (hash-ref h k)) v)

;; TODO: refactor "any-val-assoc" function using the ∃ (more different findf) function

;; TODO: write safe-hash-ref (actually shadow hash-ref) -- automatically use (hash-ref h k #false)

;; TODO: find out how to define these (multi-variadic)
;; (define-type Tuple (∀ (a b ...) (List a b ... b)))
;; (define-type Tuple. (∀ (a b ...) (Vector a b ... b)))


(: pair : ∀ (a b) a b -> (Pair a b))
(define (pair a b) `(,a . ,b))


;; intercalate


;; intersperse


(: >>> : ∀ (a b c) (a -> b) (b -> c) -> (a -> c))
(define (>>> f g) (<<< g f))


;; "More-different findf" that takes an (a -> 𝔸) rather than (a -> 𝔹)
;;    -- this way we can give it e.g. (λ v. ∧ (≡ v (hash-ref h k)) v)
(: some : ∀ (a) (a -> 𝔸) (Listof a) -> (Option a))
(define (some pred xs) (ormap (λ ([x : a]) : (Option a)
                                 (and (pred x) x)) xs))
(define ∃ some)


(: not-member (∀ (a b) (->* (b (Listof a)) ((b a -> Any)) Boolean)))
(define (not-member x xs [eqv-rel equal?]) (¬ (∈ x xs eqv-rel)))
(define ∉ not-member)


(: none : ∀ (a) (a -> Boolean) (Listof a) -> Boolean)
(define (none pred xs) (¬ (∃ pred xs)))
(define ∄ none)


(: ≠ : Number Number -> Boolean)
(define (≠ x y) (¬ (= x y)))
(define /= ≠)

(: not-equal? : Any Any -> Boolean)
(define (not-equal? x y) (¬ (≡ x y)))
(define ≢ not-equal?)


;; all
(: all : ∀ (a) (a -> Boolean) (Listof a) -> Boolean)
(define (all pred xs) (andmap (λ ([x : a]) (pred x)) xs))


(: flip : ∀ (a b c) (a b -> c) -> (b a -> c))
(define (flip f) (λ (b a) (f a b)))
(define ⍨ flip)


;; move 1st param to 3rd
(: flip1 : ∀ (a b c d) (a b c -> d) -> (b c a -> d))
(define (flip1 f) (λ (b c a) (f a b c)))
(define ⍨. flip1)


;; move 2nd param to 3rd
(: flip2 : ∀ (a b c d) (a b c -> d) -> (a c b -> d))
(define (flip2 f)  (λ (a c b) (f a b c)))
(define ⍨: flip2)


;; (: ‼ : ∀ (a) Integer (Listof a) -> a)
;; (define ‼ (flip list-ref))
;; (define !! ‼)

(: !! : ∀ (a) Integer (Listof a) -> a)
(define (!! i xs) (list-ref xs i))
(define ‼ !!)


(: snoc : ∀ (a) [Listof a] a -> [𝑳^ a])
(define (snoc xs x)
  (? (empty? xs)
     [list x]
     [cons (head xs) (snoc (tail xs) x)]))












;; ;; TEST
;; ;; foldl with break
;; (: foldl  (∀ (a b) (->* ((a b -> b) b (Listof a)) (#:break (a -> 𝔸)) b)))
;; (define (foldl f a xs #:break [bool (λ (x : a) #f)])
;;   ( : go : (Listof a) b -> b )
;;   (define (go xs acc)
;;     (cond [(∨ (empty? xs) bool) acc]
;;           [else (go (tail xs) (f (head xs) acc))]))
;;   (go xs a))


;; ;; FIXME: convert from case-λ to ->*
;; ;; foldl with break
;; ;; can specify predicate on accumulator or on each element
;; (: foldl : ∀ (a b) ->* (a b -> b) b (Listof a) [#:break (a -> Any)] [#:break-acc (b -> Any)] b)
;; (define (foldl f a xs #:break [pred (λ ([x : a]) #f)] #:break-acc [pred-acc (λ ([acc : b]) #f)])
;;   )
;; ;;       [(f a xs) (infer:foldl f a xs)]
;; ;;       [(f a xs #:break pred)
;; ;;        (: go : (Listof a) b -> b)
;; ;;        (define (go xs acc)
;; ;;          (cond [(or (empty? xs) (pred (head xs))) acc]
;; ;;                [else (go (tail xs) (cons (f (head xs) acc)))]))
;; ;;        (go xs a)]
;; ;;       [(f a xs #:break pred)
;; ;;        (: go : (Listof a) b -> b)
;; ;;        (define (go xs acc)
;; ;;          (cond [(∨ (empty? xs) (pred acc)) acc]
;; ;;                [else (go (tail xs) (cons (f (head xs) acc)))]))
;; ;;        (go xs a)]
;; ;;       ))




;; ;; foldl1 with break
;; ;; can specify predicate on accumulator or on each element
;; (: ⮲. (∀ (a) (case→
;;               [(a a -> a) (List^ a) -> a]
;;               [(a a -> a) (List^ a) (#:break (a -> Any)) -> a]
;;               [(a a -> a) (List^ a) #:break-acc (a -> Any) -> a])))
;; (define ⮲. (case-λ
;;     [(f xs) (infer:foldl f (head xs) (tail xs))]
;;     [(f xs pred)
;;      (: go : (Listof a) a -> a)
;;      (define (go xs acc)
;;        (cond [(∨ (empty? xs) (pred (head xs))) acc]
;;              [else (go (⍠ (f (head xs) (tail xs))))]))
;;      (go xs empty)]
;;     [(f xs pred)
;;      (: go : (Listof a) -> a)
;;      (define (go xs acc)
;;        (cond [(∨ (empty? xs) (pred acc)) acc]
;;              [else (go (tail xs) (cons (f (head xs) acc)))]))
;;      (go xs empty)]))


;; ;; foldr with break
;; (: ⮳ (∀ (a b) (case→
;;                [(a b -> b) b (Listof a) -> b]
;;                [(a b -> b) b (Listof a) (#:break (a -> Any)) -> b])))
;; (define ⮳ (case-λ
;;     [(f a xs) (infer:foldr f a xs)]
;;     [(f a xs pred)
;;      (: go : b (Listof a) -> b)
;;      (define (go xs)
;;        (cond [(∨ (empty? xs) (pred (head xs))) empty]
;;              [else (go (⍠ (f (head xs) (tail xs))))]))
;;      (go (cons a xs))]))



;; ;; foldr1 with break
;; (: ⮳. (∀ (a b) (case→
;;                [(a b -> b) b (Listof a) -> b]
;;                [(a a -> a) (List^ a) -> a]
;;                [(a a -> a) (List^ a) (#:break (a -> Any)) -> a])))
;; (define ⮳. (case-λ
;;     [(f a xs) (infer:foldr f a xs)]
;;     [(f a xs pred)
;;      (: go : b (Listof a) -> b)
;;      (define (go xs)
;;        (cond [(∨ (empty? xs) (pred (head xs))) empty]
;;              [else (go (⍠ (f (head xs) (tail xs))))]))
;;      (go (cons a xs))]))




;; (: zip (∀ (a b c) (case->
;;                    [(Listof a) (Listof b) -> (Listof (Pair a b))]
;;                    [(Listof a) (Listof b) (Listof c) -> (Listof (List a b c))])))
;; (define zip (case-λ
;;         [(xs ys)
;;          (∴ ([lys (length ys)]
;;              [lxs (length xs)])
;;             (cond [(= lxs lys)   (map (λ (x y) (pair x y)) xs ys)]
;;                   [(> lxs lys)   (map (λ (x y) (pair x y)) (↑ xs lys) ys)]
;;                   ;; lxs < lys
;;                   [else (map (λ (x y) (pair x y)) xs (↑ ys lxs))]))]
;;         [(xs ys zs)
;;          (∴ ([lxs (length xs)]
;;              [lys (length ys)]
;;              [lzs (length zs)])
;;             (for/list : (Listof (List a b c))
;;                 ([x xs]
;;                  [y ys]
;;                  [z zs])
;;               #:break (∃ (λ ([as : (Listof Any)]) (empty? (tail as))) '[xs ys zs])
;;               ;; #:break (∃ (∘ empty? tail) '[xs ys zs])
;;               (list x y z)))]))


;; (: zip-with : ∀ (a b c) (a b -> c) (Listof a) (Listof b) -> (Listof c))
;; (define (zip-with f xs ys) (<$> f xs ys))

















(: zip-with (∀ (a b c d) (case->
                          [(a b -> c) (Listof a) (Listof b) -> (Listof c)]
                          [(a b c -> d) (Listof a) (Listof b) (Listof c) -> (Listof d)])))
(define zip-with (case-λ
             [(f xs ys) (map f xs ys)]
             [(f xs ys zs) (map f xs ys zs)]))


(: unzip : ∀ (a b) (Listof (Pair a b)) -> (Pair (Listof a) (Listof b)))
(define (unzip ps) (foldl (λ ([p : (Pair a b)] [acc : (Pair (Listof a) (Listof b))])
                   (pair (cons (car p) (car acc)) (cons (cdr p) (cdr acc))))
                 (ann (pair '[] '[]) (Pair (Listof a) (Listof b)))
                 ps))


(: in-range? : Real Real Real -> (Option Real))
(define (in-range? n lower upper)
  (? (>= lower upper)
     (error "in-range?: lower bound was not < upper bound")
     (∧ (>= n lower) (<= n upper) n)))


;; equivalent to ((map f) >>> (filter pred)), except more efficient,
;;    since it avoids building the intermediate list.
(: map-filter : ∀ (a b) (a -> b) (b -> Any) (Listof a) -> (Listof b))
(define (map-filter f pred xs)
  (orig:filter-map (λ ([x : a]) (let* ([res (f x)]) (∧ (pred res) res))) xs))
(define ⊇∘¨ map-filter)


;; equivalent to ((filter pred) >>> (map f)), except more efficient,
;;    since it avoids building the intermediate list.
(: filter-map : ∀ (a b) (a -> Any) (a -> b) (Listof a) -> (Listof b))
(define (filter-map pred f xs)
  (orig:filter-map (λ ([x : a]) (∧ (pred x) (f x))) xs))
(define ¨∘⊇ filter-map)


;; extract a list of vals from list of hashes, given a single key
(: select (∀ (a b) (case→
                 ((Listof (HashTable a b)) a -> (Listof b))
                 ((Listof (HashTable a b)) a False -> (Listof (Option b))))))
(define select (case-λ
        [(hs key) (map (λ ([h : (HashTable a b)]) ((inst hash-ref a b) h key)) hs)]
        [(hs key false) (map (λ ([h : (HashTable a b)]) ((inst hash-ref a b #f) h key #f)) hs)]))
(define σ select)


;; filter hs for those hashes for which k is associated with one of the values in vs
(: filter-hashes : ∀ (a b) a (Listof b) (Listof (HashTable a b)) -> (Listof (HashTable a b)))
(define (filter-hashes k vs hs)
  (filter (λ ([h : (HashTable a b)])
            (and (findf (λ ([v : b]) (equal?  v (hash-ref h k #f))) vs)
               h))
          hs))
(define σ. filter-hashes)


;; ;; if ∃ v' ∈ vs | v' == (hash-ref h k), then give (the 1st matching) v', else #f
;; ;; checks whether key k in hash h is associated with any v among vs. If so, give the v, else give #false.
;; (: any-val-assoc : ∀ (a b) (HashTable a b) a (Listof b) -> (Option b))
;; (define (any-val-assoc h k vs)
;;   (ormap (λ ([v : b]) (∧ (equal? v ((inst hash-ref a b) h k #f)) v)) vs))


;; ;; FIXME
;; ;; if ∃ v' ∈ vs | v' == (hash-ref h k), then give (the 1st matching) v', else #f
;; ;; given a single hash, checks whether k is associated with any v among vs. If so, give the v, else give #false.
;; ;; given list of hashes, checks the above for each hash, and gives the corresponding list of (Maybe v)'s.
;; (: any-val-assoc (∀ (a b) (case->
;;                              [(HashTable a b) a (Listof b) -> (Option b)]
;;                              [(Listof (HashTable a b)) a (Listof b) -> (Listof (Option b))])))
;; (define any-val-assoc (case-λ
;;                     [(h k vs) (ormap (λ ([v : b])
;;                                        (and (equal? v ((inst hash-ref a b) h k #f)) v)) vs)]
;;                     [(hs k vs) (map (λ ([h : (HashTable a b)])
;;                                     (ormap (λ ([v : b])
;;                                              (and (equal? v ((inst hash-ref a b) h k #f)) v))
;;                                            vs))
;;                                   hs)]))











































;; TODO: refactor "any-val-assoc" function using the ∃ (more different findf) function

;; TODO: write safe-hash-ref (actually shadow hash-ref) -- automatically use (hash-ref h k #false)


;; (: hash-ref (∀ (a b c) (case->
;;                           ((HashTable a b) a -> (Option b))
;;                           ((HashTable a b) a (-> c) -> (U b c))
;;                           (->* (HashTableTop a) (False) Any) ; matches orig:hash-ref
;;                           (HashTableTop a (-> c) -> Any))))
;; (define hash-ref (case-λ
;;                [(h k) (orig:hash-ref h k #f)]
;;                [(h k thu) (orig:hash-ref h k thu)]
;;                [(h bool) (orig:hash-ref h bool)] ; matches orig:hash-ref
;;                [(h k thu) (orig:hash-ref h k thu)]))

;; (: hash-ref (∀ (a b c) (case->
;;                         []
;;                         []
;;                         []
;;                         [])))



;; ;; if ∃ v' ∈ vs | v' == (hash-ref h k), then give (the 1st matching) v', else #f
;; ;; given a single hash, checks whether k is associated with any v among vs. If so, give the v, else give #false.
;; ;; given list of hashes, checks the above for each hash, and gives the corresponding list of (Maybe v)'s.
;; (: any-val-assoc (∀ (a b) (case->
;;                            [(HashTable a b) a (Listof b) -> (Option b)]
;;                            [(Listof (HashTable a b) a (Listof b) -> (Listof (Option b)))])))
;; (define any-val-assoc (case-λ
;;                   [(h k vs) (ormap (λ ([v : b]) : (Option b) (and (equal? v (hash-ref)) )) vs)]
;;                   [(hs k vs)]))






























































;; ;; variadic versions of map-filter (TODO: (learning exercise) implement for map-filter and filter-map)
;; (: map-filter (∀ (c a b ...) (-> (-> a b ... b (U c False)) (c -> Any) (Listof a) (Listof b) ... b (Listof c))))
;; (define (map-filter f pred xs . rss)
;;   (apply (curry filter-map (λ #:∀ (b ...) [xs : (List* a b ... b)] (let ([res (apply f xs)]) (and (pred res) res)))) (cons xs rss)))

;; ;; version of map-filter that utilizes the type info gained from the predicate, just like filter
;; (: map-filter (∀ (a b) (case->
;;                         (-> a b) (-> b Any #:+ c) (Listof a) -> (Listof c)
;;                         (-> a b) (-> b Any) (Listof a) -> (Listof b))))
;; (define (map-filter f pred xs)
;;   (orig:filter-map (λ ([x : a]) (let ([res (f x)]) (cond [(pred res) res]
;;                                                        [else #f]))) xs)
;;   (orig:filter-map (λ ([x : a]) (let ([res (f x)]) (and (pred res) res))) xs))



;; (define-simple-macro (hash-ref-c hash:id key:id type:id) (λ (cast (hash-ref key) type)))


;; (: ∧ (∀ (a b) (->* () () #:rest Any Any)))
;; (define (∧ . rst) (⮲ (λ (x acc) (and x acc))
;;                 #t rst))



;; write andmap', which is the same as orig:andmap, except:
;;  either (pick one of these constraints -- probably #2):
;;      1. it gives #f on an empty list instead of #t, thus eliminating the problem
;;          of the result type being (U Boolean a), given (Listof a).
;;      2. it specifies in a case-λ that if given an (NEList z), returns z (rather than (Union True z)).



