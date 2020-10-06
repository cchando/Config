
#lang aful typed/racket
(provide (all-defined-out))
(require typed-map)
(require (only-in typed/racket
                  [filter-map orig:filter-map]
                  [identity id]
                  [list-ref !!]
                  [first head] [rest tail]
                  [and ∧] [or ∨] [not ¬] [xor ⊻] [nor ⊽] [nand ⊼]
                  [letrec ∴] [if ?]
                  [cons ⍠]
                  [append <>] [append* <>^]
                  [string-append ++] [string-append* ++^]
                  ;; [append* concat]
                  [map <$>] [foldl ⮲] [foldr ⮳]
                  [map ⮊]
                  [apply ⮉]
                  [filter ⮋]
                  [curry ⫶] [compose1 ∘]
                  [+ ∑] [* ∏]
                  [member ∈] [findf ∃]
                  [take ↑] [drop ↓]
                  [make-list replicate]
                  [append-map concat-map]
                  [remove remove-1st] [remove* \\]
                  ;; [for ∀] [for* ∀*] [for/list ∀/list] [for*/list ∀*/list] [for/hash ∀/hash] [for*/hash ∀*/hash]
                  [for* ∀*] [for/list ∀→l] [for*/list ∀*→l] [for/hash ∀→h] [for*/hash ∀*→h]
                  ;; [for ∀] [for* ∀*] [for/list ∀/l] [for*/list ∀*/l] [for/hash ∀/h] [for*/hash ∀*/h]
                  [for/vector ∀→v] [for*/vector ∀*→v] [for/sum ∀→∑] [for*/sum ∀*→∑] [for/product ∀→∏] [for*/product ∀*→∏]
                  ;; [for/vector ∀/v] [for*/vector ∀*/v] [for/sum ∀/∑] [for*/sum ∀*/∑]
                  [for/last ∀→last] [for*/last ∀*→last] [for/set ∀→s] [for*/set ∀*→s]
                  [for/hasheq ∀→hq] [for*/hasheq ∀*→hq] [for/hasheqv ∀→hv] [for*/hasheqv ∀*→hv]
                  [for/or ∀→∨] [for*/or ∀*→∨] [for/and ∀→∧] [for*/and ∀*→∧] [for/first ∀→fst] [for*/first ∀*→fst]
                  [for/lists ∀→lists] [for*/lists ∀*→lists] [for/fold ∀→⮲] [for*/fold ∀*→⮲]
                  [first 1st] [second 2nd] [third 3rd] [fourth 4th] [fifth 5th]
                  [sixth 6th] [seventh 7th] [eighth 8th] [ninth 9th] [tenth 10th]))





(: ∉ (∀ (a b) (->* (b (Listof a)) ((-> b a Any)) Boolean)))
(define (∉ x xs (eqv-rel equal?)) (if (member x xs eqv-rel) #t #f))


(: ≠ : Any Any -> Boolean)
(define (≠ x y) (¬ (equal? x y)))


;; all
(: all : ∀ (a) (a -> Boolean) (Listof a) -> Boolean)
(define (all pred xs) (andmap (λ ([x : a]) (pred x)) xs))


(: zip : All (a b) (Listof a) (Listof b) -> (Listof (Pairof a b)))
(define (zip xs ys) (let ([lys (length ys)]
                   [lxs (length xs)])
                 (cond [(= lxs lys) (map (λ (x y) `(,x . ,y)) xs ys)]
                       [(> lxs lys) (map (λ (x y) `(,x . ,y)) (take xs lys) ys)]
                       ;; < lxs lys
                       [else (map (λ (x y) `(,x . ,y)) xs (take ys lxs))])))


;; (: unzip : ∀ (a b) (Listof (Pairof a b) -> (Values (Listof a) (Listof b))))
;; (define unzip : ())


;; zipWith

;; intercalate

;; intersperse


(: in-range? : Real Real Real -> (Option Real))
(define (in-range? n lower upper)
  (if (>= lower upper)
     (error "in-range?: lower bound was not < upper bound")
     (and (>= n lower) (<= n upper) n)))


;; extract a list of vals from list of hash, given single key
(: select (All (a b) (case→
                    (-> (Listof (HashTable a b)) a (Listof b))
                    (-> (Listof (HashTable a b)) a False (Listof (Option b))))))
(define select (case-lambda
            [(hs key) (map (λ ([h : (HashTable a b)]) ((inst hash-ref a b) h key)) hs)]
            [(hs key false) (map (λ ([h : (HashTable a b)]) ((inst hash-ref a b #f) h key #f)) hs)]))


;; map f to xs, then filter result list using pred
(: map-filter : All (a b) (-> a b) (-> b Any) (Listof a) -> (Listof b))
(define (map-filter f pred xs)
  (orig:filter-map (λ ([x : a]) (let ([res (f x)]) (and (pred res) res))) xs))


;; filter xs using pred, then map f to result list
(: filter-map : All (a b) (-> a Any) (-> a b) (Listof a) -> (Listof b))
(define (filter-map pred f xs)
  (orig:filter-map (λ ([x : a]) (and (pred x) (f x))) xs))


;; checks whether (hash-ref h k) == v for any v in vs. If so, give the v, else give #false.
(: hash-match-vals : All (a b) (HashTable a b) a (Listof b) -> (Option b))
(define (hash-match-vals h k vs)
  (ormap (λ ([v : b]) (and (equal? v ((inst hash-ref a b) h k #f)) v)) vs))


;; filter hs for those hashes for which k is associated with one of the values in vs
(: filter-hash : All (a b) (Listof (HashTable a b)) a (Listof b) -> (Listof (HashTable a b)))
(define (filter-hash hs k vs)
  (filter (λ ([h : (HashTable a b)])
            (and
             ;; find a v among vs that's associated with k in h, if one exists.
             (findf (λ ([v : b])
                      (equal? v (hash-ref h k #f)))
                    vs)
             ;; if we found that h has such a v, include h in filtered list
             h))
          hs))

;; ;; given a single hash, checks whether k is associated with any v among vs. If so, give the v, else give #false.
;; ;; given list of hashes, checks the above for each hash, and gives the corresponding list of (U v #false).
;; (: hash-match-vals (All (a b) (case->
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
;; (: map-filter (All (c a b ...) (-> (-> a b ... b (U False c)) (c -> Any) (Listof a) (Listof b) ... b (Listof c))))
;; (define (map-filter f pred xs . rss)
;;   (apply (curry filter-map (λ #:∀ (b ...) [xs : (List* a b ... b)] (let ([res (apply f xs)]) (and (pred res) res)))) (cons xs rss)))

;; ;; version of map-filter that utilizes the type info gained from the predicate, just like filter
;; (: map-filter (All (a b) (case->
;;                         (-> a b) (-> b Any #:+ c) (Listof a) -> (Listof c)
;;                         (-> a b) (-> b Any) (Listof a) -> (Listof b))))
;; (define (map-filter f pred xs)
;;   (orig:filter-map (λ ([x : a]) (let ([res (f x)]) (cond [(pred res) res]
;;                                                        [else #f]))) xs)
;;   (orig:filter-map (λ ([x : a]) (let ([res (f x)]) (and (pred res) res))) xs))





