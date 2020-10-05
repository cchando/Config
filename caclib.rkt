
#lang aful typed/racket
(provide (all-defined-out))
(require typed-map)
(require (only-in typed/racket
                  [filter-map orig:filter-map]
                  [identity id]
                  [list-ref !!]
                  [first head]
                  [rest tail]
                  [first 1st]
                  [second 2nd]
                  [third 3rd]
                  [fourth 4th]
                  [fifth 5th]
                  [sixth 6th]
                  [seventh 7th]
                  [eighth 8th]
                  [ninth 9th]
                  [tenth 10th]))

;; useful for easier range-based filtering
(: in-range? : Real Real Real -> (Option Real))
(define (in-range? n lower upper)
  (if (>= lower upper)
     (error "in-range?: lower bound was not < upper bound")
     (and (>= n lower) (<= n upper) n)))


;; extract a list of vals from list of hash, given single key
(: select (All (a b) (case->
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
  (filter (λ ([h : (HashTable a b)]) (and (ormap (λ ([v : b]) (and (equal? v (hash-ref h k #f)) v)) vs) h)) hs))

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





