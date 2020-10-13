
#lang aful typed/racket
(provide (all-defined-out))
(require (only-in typed/racket
                  [filter-map orig:filter-map]
                  [else ε] [U ⋃] [∩ ⋂]
                  [let* ∴] [if ?] [case-lambda case-λ] [and ∧] [or ∨] [nor ⊽] [nand ⊼]
                  [for ∀:] [for* ∀*:] [for/list ∀:l] [for*/list ∀*:l] [for/hash ∀:h] [for*/hash ∀*:h]
                  [for/vector ∀:v] [for*/vector ∀*:v] [for/sum ∀:∑] [for*/sum ∀*:∑]
                  [for/last ∀:last] [for*/last ∀*:last] [for/set ∀:s] [for*/set ∀*:s]
                  [for/hasheq ∀:hq] [for*/hasheq ∀*:hq] [for/hasheqv ∀:hv] [for*/hasheqv ∀*:hv]
                  [for/or ∀:or] [for*/or ∀*:or] [for/and ∀:and] [for*/and ∀*:and]
                  [for/product ∀:∏] [for*/product ∀*:∏] [for/first ∀:1st] [for*/first ∀*:1st]
                  [for/lists ∀:lists] [for*/lists ∀*:lists] [for/fold ∀:⮲] [for*/fold ∀*:⮲]))
(require cond-strict)
(require (only-in typed-map [foldl infer:foldl] [foldr infer:foldr]))
;; (require/typed
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
;;                   [U ⋃] [∩ ⋂] [identity id] [equal? =?] [assoc lookup]
;;                   [list-ref ‼] [first head] [rest tail] [cons ⍠] [null ∅]
;;                   [let* ∴] [if ?] [case-lambda case-λ]
;;                   [and ∧] [or ∨] [not ￢] [xor ⊻] [nor ⊽] [nand ⊼] [negate ￢^]
;;                   [append <>] [append* <>^] [string-append ++] [string-append* ++^] [append* concat]
;;                   [map <$>] [foldl ⮲] [foldr ⮳] [map ⮊] [apply ⮉] [filter ⮋] [filter-not ⮋￢]
;;                   [curry ⫶] [compose1 ∘] [compose1 <<<] [compose ∘^]
;;                   [+ ∑] [* ∏] [/ ÷] [sqrt √] [modulo %] [<= ≤] [>= ≥]
;;                   [member ∈] [findf ∃] [memf ∃s][take ↑] [drop ↓] [make-list replicate]
;;                   [append-map concat-map] [remove remove-1st] [remove* \\]
;;                   [for ∀:] [for* ∀*:] [for/list ∀:l] [for*/list ∀*:l] [for/hash ∀:h] [for*/hash ∀*:h]
;;                   [for/vector ∀:v] [for*/vector ∀*:v] [for/sum ∀:∑] [for*/sum ∀*:∑]
;;                   [for/last ∀:last] [for*/last ∀*:last] [for/set ∀:s] [for*/set ∀*:s]
;;                   [for/hasheq ∀:hq] [for*/hasheq ∀*:hq] [for/hasheqv ∀:hv] [for*/hasheqv ∀*:hv]
;;                   [for/or ∀:or] [for*/or ∀*:or] [for/and ∀:and] [for*/and ∀*:and]
;;                   [for/product ∀:∏] [for*/product ∀*:∏] [for/first ∀:1st] [for*/first ∀*:1st]
;;                   [for/lists ∀:lists] [for*/lists ∀*:lists] [for/fold ∀:⮲] [for*/fold ∀*:⮲]
;;                   [first 1st] [second 2nd] [third 3rd] [fourth 4th] [fifth 5th]
;;                   [sixth 6th] [seventh 7th] [eighth 8th] [ninth 9th] [tenth 10th]))


(define id identity)
(define =? equal?)
(define lookup assoc)
(define head first)
(define θ first)
(define tail rest)
(define τ rest)
(define ρ length)
(define ⊖ reverse)
(define ι build-list)
(define ⍠ cons)
(define ∅ empty)
(define ￢ not)
(define ～ not)
(define ⊻ xor)
(define ￢. negate)
(define ⌈ exact-ceiling)
(define ⌊ exact-floor)
(define ⌉ ceiling)
(define ⌋ floor)
(define <> append)
(define <>. append*)
(define ++ string-append)
(define ++. string-append*)
(define concat append*)
(define ∆ sort) ; ⍋
(define σ sort)
(define <$> map)
;; (define ⮲ foldl)
;; (define ⮳ foldr)
(define ⮊ map)
(define ⮉ apply)
(define ⮋ filter)
(define ⮋￢ filter-not)
(define ⫶ curry)
(define <<< compose1)
(define ∘ compose1)
(define ∘^ compose)
(define ∑ +)
(define ∏ *)
(define √ sqrt)
(define % modulo)
(define ∣ abs)
(define ≤ <=)
(define ≥ >=)
(define ∈ member)
(define ∃ findf)
(define ∃s memf)
(define ↑ take)
(define ↓ drop)
(define replicate make-list)
(define concat-map append-map)
(define remove-1st remove)
(define \\ remove*)
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
(define-type (List^ a) (Pairof a (Listof a))) ; Non-empty List

;; TODO: find out how to define these (multi-variadic)
;; (define-type Tuple (∀ (a b ...) (List a b ... b)))
;; (define-type Tuple. (∀ (a b ...) (Vector a b ... b)))

;; ℤ   ℕ   ∅   ⋙  ⨌   ⩽  ⩾

(: pair : ∀ (a b) a b -> (Pair a b))
(define (pair a b) `(,a . ,b))
(define ⌻ pair)


;; intercalate


;; intersperse


(: >>> : ∀ (a b c) (a -> b) (b -> c) -> (a -> c))
(define (>>> f g) (<<< g f))


(: ∉ (∀ (a b) (->* (b (Listof a)) ((b a -> Any)) Boolean)))
(define (∉ x xs [eqv-rel equal?]) (￢ (∈ x xs eqv-rel)))


(: ∄ : ∀ (a) (a -> Boolean) (Listof a) -> Boolean)
(define (∄ pred xs) (￢ (∃ pred xs)))


(: ≠ : Any Any -> Boolean)
(define (≠ x y) (￢ (=? x y)))
(define /= ≠)

;; all
(: all : ∀ (a) (a -> Boolean) (Listof a) -> Boolean)
(define (all pred xs) (andmap (λ ([x : a]) (pred x)) xs))


(: flip : ∀ (a b c) (a b -> c) -> (b a -> c))
(define (flip f) (λ (b a) (f a b)))


;; move 1st param to 3rd
(: flip1 : ∀ (a b c d) (a b c -> d) -> (b c a -> d))
(define (flip1 f) (λ (b c a) (f a b c)))


;; move 2nd param to 3rd
(: flip2 : ∀ (a b c d) (a b c -> d) -> (a c b -> d))
(define (flip2 f)  (λ (a c b) (f a b c)))


;; (: ‼ : ∀ (a) Integer (Listof a) -> a)
;; (define ‼ (flip list-ref))
;; (define !! ‼)

(: ‼ : ∀ (a) Integer (Listof a) -> a)
(define (‼ i xs) (list-ref xs i))
(define !! ‼)




(: snoc : ∀ (a) [Listof a] a -> [List^ a])
(define (snoc xs x)
  (? (empty? xs)
     [list x]
     [⍠ (head xs) (snoc (tail xs) x)]))



;; ;; TEST
;; ;; foldl with break
;; ;; can specify predicate on accumulator or on each element
;; (: ⮲ : ∀ (a b) (a b ->  b) b (Listof a) #:break (a -> Any) -> b)
;; (define (⮲ f a xs pred)
;;   ( : go : (Listof a) b -> b )
;;   (define (go xs acc)
;;     (cond [(∨ (empty? xs) (pred (head xs))) acc]
;;           [else (go (cons (f (head xs) (tail xs))))]))
;;   (go xs a))
;; (define foldl ⮲)




;; ;; foldl with break
;; ;; can specify predicate on accumulator or on each element
;; (: ⮲ (∀ (a b) (case→
;;                [(a b ->  b) b (Listof a) -> b]
;;                [(a b ->  b) b (Listof a) #:break (a -> Any) -> b]
;;                [(a b ->  b) b (Listof a) #:break-acc (b -> Any) -> b])))
;; (define ⮲ (case-λ
;;       [(f a xs) (infer:foldl f a xs)]
;;       [(f a xs pred)
;;        (begin
;;          ( : go : (Listof a) b -> b )
;;          (define (go xs acc)
;;            (cond [(∨ (empty? xs) (pred (head xs))) acc]
;;                  [else (go (cons (f (head xs) (tail xs))))]))
;;          (go xs a))]
;;       [(f a xs pred)
;;        (begin
;;          (: go : (Listof a) b -> b)
;;          (define (go xs acc)
;;            (cond [(∨ (empty? xs) (pred acc)) acc]
;;                  [else (go (tail xs) (cons (f (head xs) acc)))]))
;;          (go xs a))]))
;; (define foldl ⮲)


;; ;; foldl1 with break
;; ;; can specify predicate on accumulator or on each element
;; (: ⮲. (∀ (a) (case→
;;               [(a a ->  a) (List^ a) -> a]
;;               [(a a ->  a) (List^ a) #:break (a -> Any) -> a]
;;               [(a a ->  a) (List^ a) #:break-acc (a -> Any) -> a])))
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
;; (define foldl. ⮲.)


;; ;; foldr with break
;; (: ⮳ (∀ (a b) (case→
;;                [(a b ->  b) b (Listof a) -> b]
;;                [(a b ->  b) b (Listof a) #:break (a -> Any) -> b])))
;; (define ⮳ (case-λ
;;     [(f a xs) (infer:foldr f a xs)]
;;     [(f a xs pred)
;;      (: go : b (Listof a) -> b)
;;      (define (go xs)
;;        (cond [(∨ (empty? xs) (pred (head xs))) empty]
;;              [else (go (⍠ (f (head xs) (tail xs))))]))
;;      (go (cons a xs))]))
;; (define foldr ⮳)



;; ;; foldr1 with break
;; (: ⮳. (∀ (a b) (case→
;;                [(a b ->  b) b (Listof a) -> b]
;;                [(a a ->  a) (List^ a) -> a]
;;                [(a a ->  a) (List^ a) #:break (a -> Any) -> a])))
;; (define ⮳. (case-λ
;;     [(f a xs) (infer:foldr f a xs)]
;;     [(f a xs pred)
;;      (: go : b (Listof a) -> b)
;;      (define (go xs)
;;        (cond [(∨ (empty? xs) (pred (head xs))) empty]
;;              [else (go (⍠ (f (head xs) (tail xs))))]))
;;      (go (cons a xs))]))
;; (define foldr. ⮳.)





(: zip (∀ (a b c) (case→
                   [(Listof a) (Listof b) -> (Listof (Pair a b))]
                   [(Listof a) (Listof b) (Listof c) -> (Listof (List a b c))])))
(define zip (case-λ
        [(xs ys)
         (∴ ([lys (length ys)]
             [lxs (length xs)])
            (cond [(= lxs lys)   (map (λ (x y) (pair x y)) xs ys)]
                  [(> lxs lys)   (map (λ (x y) (pair x y)) (↑ xs lys) ys)]
                  ;; lxs < lys
                  [else (map (λ (x y) (pair x y)) xs (↑ ys lxs))]))]
        [(xs ys zs)
         (∴ ([lxs (length xs)]
             [lys (length ys)]
             [lzs (length zs)])
            (for/list : (Listof (List a b c))
                ([x xs]
                 [y ys]
                 [z zs])
              #:break (∨ (null? (tail xs)) (null? (tail ys)) (null? (tail zs)))
              (list x y z)))]))


;; (: zip-with : ∀ (a b c) (a b -> c) (Listof a) (Listof b) -> (Listof c))
;; (define (zip-with f xs ys) (<$> f xs ys))

(: zip-with (∀ (a b c d) (case→
                        ((a b -> c) (Listof a) (Listof b) -> (Listof c))
                        ((a b c -> d) (Listof a) (Listof b) (Listof c) -> (Listof d)))))
(define zip-with (case-λ [(f xs ys) (map f xs ys)]
                    [(f xs ys zs) (map f xs ys zs)]))


(: unzip : ∀ (a b) (Listof (Pair a b)) -> (Pair (Listof a) (Listof b)))
(define (unzip ps) (⮲ (λ ([p : (Pair a b)] [acc : (Pair (Listof a) (Listof b))])
                   (pair (⍠ (car p) (car acc)) (⍠ (cdr p) (cdr acc))))
                 (ann (pair '[] '[]) (Pair (Listof a) (Listof b)))
                 ps))


(: in-range? : Real Real Real -> (Option Real))
(define (in-range? n lower upper)
  (? (>= lower upper)
     (error "in-range?: lower bound was not < upper bound")
     (∧ (>= n lower) (<= n upper) n)))


;; extract a list of vals from list of hashes, given a single key
(: select (All (a b) (case→
                    ((Listof (HashTable a b)) a -> (Listof b))
                    ((Listof (HashTable a b)) a False -> (Listof (Option b))))))
(define select (case-λ
            [(hs key) (map (λ ([h : (HashTable a b)]) ((inst hash-ref a b) h key)) hs)]
            [(hs key false) (map (λ ([h : (HashTable a b)]) ((inst hash-ref a b #f) h key #f)) hs)]))


;; equivalent to ((map f) >>> (filter pred)), except more efficient,
;;    since it avoids building the intermediate list.
(: map-filter : ∀ (a b) (a -> b) (b -> Any) (Listof a) -> (Listof b))
(define (map-filter f pred xs)
  (orig:filter-map (λ ([x : a]) (let* ([res (f x)]) (∧ (pred res) res))) xs))
(define ⮊⮋ map-filter)


;; equivalent to ((filter pred) >>> (map f)), except more efficient,
;;    since it avoids building the intermediate list.
(: filter-map : ∀ (a b) (a -> Any) (a -> b) (Listof a) -> (Listof b))
(define (filter-map pred f xs)
  (orig:filter-map (λ ([x : a]) (∧ (pred x) (f x))) xs))
(define ⮋⮊ filter-map)


;; checks whether (hash-ref h k) == v for any v in vs. If so, give the v, else give #false.
(: hash-match-vals : ∀ (a b) (HashTable a b) a (Listof b) -> (Option b))
(define (hash-match-vals h k vs)
  (ormap (λ ([v : b]) (∧ (equal? v ((inst hash-ref a b) h k #f)) v)) vs))


;; filter hs for those hashes for which k is associated with one of the values in vs
(: filter-hash : ∀ (a b) (Listof (HashTable a b)) a (Listof b) -> (Listof (HashTable a b)))
(define (filter-hash hs k vs)
  (filter (λ ([h : (HashTable a b)])
            (∧ (∃ (λ ([v : b]) (=? v (hash-ref h k #f))) vs)
               h))
          hs))

;; ;; given a single hash, checks whether k is associated with any v among vs. If so, give the v, else give #false.
;; ;; given list of hashes, checks the above for each hash, and gives the corresponding list of (U v #false).
;; (: hash-match-vals (∀ (a b) (case->
;;                           ((HashTable a b) a (Listof b) -> (U b False))
;;                           ((Listof (HashTable a b)) a (Listof b) -> (Listof (U b False))))))
;; (define hash-match-vals (case-lambda
;;                      [(h k vs) (ormap (λ ([v : b])
;;                                     (and (equal? v ((inst hash-ref a b) h k #f)) v)) vs)]
;;                      [(hs k vs) (map (λ ([h : (HashTable a b)])
;;                                      (ormap (λ ([v : b])
;;                                           (and (equal? v ((inst hash-ref a b) h k #f)) v))
;;                                         vs))
;;                                    hs)]))


;; ;; variadic versions of map-filter (TODO: (learning exercise) implement for map-filter and filter-map)
;; (: map-filter (∀ (c a b ...) (-> (-> a b ... b (U False c)) (c -> Any) (Listof a) (Listof b) ... b (Listof c))))
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



