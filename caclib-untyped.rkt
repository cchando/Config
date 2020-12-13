#lang typed/racket

(provide (all-defined-out))

(define-type Word (Listof Symbol))


;; gives a list of the permutations of word w
(: arrangements : Word -> (Listof Word))
(define (arrangements w)
  (cond [(empty? w) (list empty)] ; dictacted by helper-function insert-everywhere-in-all-words
        [else (insert-everywhere-in-all-words (first w) (arrangements (rest w)))]))


(: insert-everywhere-in-all-words : Char (Listof Word) -> (Listof Word))
(define (insert-everywhere-in-all-words c ws)
  (cond [(empty? ws) empty]
        [else (append (insert-everywhere c (first ws)) (insert-everywhere-in-all-words c (rest ws)))]))


(: insert-everywhere : Char Word -> (Listof Word))
(define (insert-everywhere c w)
  (cond [(empty? w) (list (list c))]
        [else (cons (cons c w)
                    (prepend-to-each (first w) (insert-everywhere c (rest w))))]))


(: prepend-to-each : Char -> (Listof Word) -> (Listof Word))
(define (prepend-to-each c ws)
  (cond [(empty? ws) empty]
        [else (cons (cons c (first ws)) (prepend-to-each c (rest ws)))]))


; gives a list of all permutations of the word xs
(: rearrangements : (Listof Char) -> (Listof Char))
(define (rearrangements xs)
  (map (lambda (x)
         ((letrec ([xs-without-x (remove x xs)])
            map (lambda (x)
                  (cons x (rearrangements (xs-without-x))))
            (xs-without-x))))
       xs))


; creates a list of all rearrangements of the items in w
(: arrangements : All (a) (Listof a) -> (Listof (Listof a)))
(define (arrangements w)
  (cond
    [(empty? w) '(())]
    [else (for*/list : (Listof (Listof a)) ([item w]
                                            [arrangement-without-item
                                             (arrangements (remove item w))])
            (cons item arrangement-without-item))]))


; creates a list of all rearrangements of the items in w
(: arrangements : All (a) (Listof a) -> (Listof (Listof a)))
(define (arrangements w)
  (cond
    [(empty? w) '(())]
    [else
     (foldr (λ (item others)
              (define arr-without-item
                (arrangements (remove item w)))
              (define add-item-to-front
                (map (lambda (a) (cons item a))
                     arr-without-item))
              (append add-item-to-front others))
            empty
            w)]))


(: relative->absolute : All (a) (Listof a) -> (Listof a))
(define (relative->absolute xs)
  (reverse (foldr (lambda (x acc) (cons (+ x (first acc) acc)))
                  (list (first xs))
                  (reverse (rest xs)))))


(define (identityM [n : Natural]) : (Listof (Listof Natural))
  ; Creates a list of n zero-filled lists, each of length m
  (: mlist-builder : Natural Natural -> (Listof (Listof Natural)))
  (define (mlist-builder n m)
    (if (= 0 n)
        null
        (cons (make-list m 0) (mlist-builder (- n 1) m))))
  ; replace the Nth element of xs with 1
  (: insert-1-at-Nth : Number (Listof Number) (Listof Number))
  (define (insert-1-at-Nth n xs)
    (if (= 1 n)
        (cons 1 (cdr xs))
        (cons (car xs) (insert-1-at-Nth (- n 1) (cdr xs)))))
  ; Starting from end of xs, runs [f with y] on current element, where y starts as the length of xs and decrements.
  ; Given:   ((dec-map insert-1-at-Nth (list (list 0 0 0) (list 0 0 0) (list 0 0 0)))
  ; Expect:  (list (list 1 0 0) (list 0 1 0) (list 0 0 1))
  (: dec-map : All (a b) (a b -> b) (Listof a) -> (Listof b))
  (define (dec-map f xss)
    (define length (length xss))
    (define i 0)  ; recursive variable for dec-map. Starts at 1, goes up to length of xs as passed in initial call
    (define (helper f x xss)  ; i should be passed in as 1, and it increments to (length xs)
      (set! i (+ i 1))
      (if (> i x)
          null
          (cons (f i (car xss)) (helper f x (cdr xss)))))
    (helper f length xss))
  (dec-map insert-1-at-Nth (mlist-builder n n)))
; In the rec call, x is being set to (+ recvar 1) == 2 every time, so the function never stops recursing.
; Thus, recvar must either be set to (+ recvar 1) as a global variable in each function call, or it must
; become a function parameter, with an implicit agreement that it will be passed in as the value 1.


;; [X]  ->  X  -> Boolean
(: contains? (All (a) (Listof a) a -> Boolean))
(define (contains? xs z)
  (foldr (lambda (x acc)
           (or (equal? x z) acc))
         #false xs))



;; if M_ys == false -> xs should not contain z
;; else:   (M_ys is a list)
;; -> xs should contain z
;; -> ys should start with z
;; -> xs should be (append head ys) for some string head
;; -> head should not contain z
(: found? : (All (a) (Listof a) (Option (Listof a)) a -> Boolean))
(define (found? xs M_ys z)
  (let ([xs-contains-z (ormap ((curry eq?) z) xs)])
    (if (eq? M_ys #false)
        (not xs-contains-z)
        ; M_ys is a list
        (letrec ([head (take xs (- (length xs) (length M_ys)))]
                 [tail (drop xs (length head))]
                 [head-contains-z (ormap ((curry eq?) z) head)]
                 [tail-matches-M_ys (foldr (lambda (x y acc)
                                             (and (equal? x y) acc))
                                           null tail M_ys)])
          (if (>= (length xs) (length M_ys))  ; verify head exists before evaluating it
              (and xs-contains-z (equal? (car M_ys) z) tail-matches-M_ys (not head-contains-z))
              #false)))))


(: found? : (All (a) (Listof a) (Option (Listof a)) a -> Boolean))
(define (found? xs m_ys z)
  (local ((define head (take xs (- (length xs) (length M_ys)))) ; can only use head verifying that (length xs) >= (length ys)
          (define tail (drop xs (length head)))
          (define xs-contains-z (ormap ((curry eq?) z) xs))
          (define head-contains-z (ormap ((curry eq?) z) head))
          (define tail-matches-M_ys (foldr (lambda (x y acc)
                                             (and (equal? x y) acc))
                                           null tail M_ys)))
    (if (eq? M_ys #false)
        (not (xs-contains-z))
        ; M_ys is list
        (if (>= (length xs) (length M_ys))  ; verify head exists before evaluating it
            (and xs-contains-z (equal? (car M_ys) z) tail-matches-M_ys (not head-contains-z))
            (#false)))))


(: found? : (All (a) (Listof a) (Option (Listof a)) a -> Boolean))
(define (found? xs M_ys z)
  (let ([M_tail (member z xs)])
    (if (equal? M_tail #false)
        ; xs does not contain z
        (if (eq? M_ys #false)
            #true    ; M_ys is #f
            #false)  ; M_ys is a list
        ; xs does contain z
        (and (not (eq? M_ys #false)) ; M_ys is a list
             (if (= (length M_tail) (length M_ys))
                 (foldr (lambda (x y acc)
                          (and (equal? x y) acc))
                        #t M_tail M_ys)
                 #false)))))


;; gives Cartesian product of xs and ys
(: cross : All (a) (Listof a) (Listof a) -> (Listof (Listof a)))
(define (cross xs ys)
  (foldr (lambda ([x : a] [qss : (Listof (Listof a))])
           (append (map (curry list x) ys) qss))
         null
         xs))


(: cross : All (a) (Listof a) (Listof a) -> (Listof (Listof a)))
(define (cross xs ys)
  (foldr append
         null
         (map (lambda (x)
                (map (curry list x) ys))
              xs)))


(struct NONE ())

(struct node ([ssn : Integer] [name : String] [left : Node] [right : Node]))

(define-type Tree BST) ; (Union BST ...)

(define-type BST (Union Node NONE))

; takes a binary tree t and gives a list of its ssn values in order
(: bst->list : BST -> (Listof (Listof (Union Integer String))))
(define (bst->list t)
  (if (empty? t)
      empty
      (append (bst->list (node-left t)) (cons (list (node-ssn t) (node-name t)) (bst->list (node-right t))))))


; gives the name field of the Node whose ssn field is n, if such a Node exists.
; gives NONE otherwise.
(: search-bst : BST -> (Union Integer NONE))
(define (search-bst t n)
  (if (empty? t)
      (NONE)
      ; t exists and has a number and symbol
      (if (eqv? (node-ssn t) n)
          (node-name t)
          (letrec ([lval (search-bst (node-left t))])
            (if (and (number? lval) (eqv? lval n))
                lval
                (search-bst (node-right t)))))))


; is the given tree a leaf
(: leaf? : Tree -> Boolean)
(define (leaf? t)  (and (node? t) (NONE? (node-left t)) (NONE? (node-right t))))


; gives a binary search tree just like t except with the addition of the
;   Node formed from number n and symbol s
(: create-bst : BST Integer Symbol -> BST)
(define (create-bst t num sym)
  (define (err-eq) (error (format "Node with ssn ~a already present in the given BST" num)))
  (cond [(NONE? t) (Node num sym (NONE) (NONE))]
        [(= num (node-ssn t))  (err-eq)]
        [(leaf? t) (cond [(< num (node-ssn t))
                          ; insert Node as left child
                          (Node (node-ssn t) (node-name t)
                                (Node num sym (NONE) (NONE))
                                ; should already be (NONE)
                                (node-right t))]
                         ; n > (ssn t), so insert as right child
                         [else
                          (Node (node-ssn t) (node-name t)
                                ; should already be (NONE)
                                (node-left t)
                                (Node num sym (NONE) (NONE)))])]
        ; general case
        [(< num (node-ssn t))
         ; insert into left subtree
         (Node (node-ssn t) (node-name t)
               (create-bst (node-left t) num sym)
               (node-right t))]
        ; insert into right subtree
        [else
         (Node (node-ssn t) (node-name t)
               (node-left t)
               (create-bst (node-right t) num sym))]))



;; takes a list of (number, name) pairs and gives a BST by repeatedly applying create-bst
(: create-bst-from-list : (Listof (List Integer Symbol)) -> BST)
(define (create-bst-from-list xss)
  (cond [(empty? xss) NONE]
        [else (create-bst (create-bst-from-list (cdr xss)) (caar xss) (cadar xss))]))


(: list->bst : (Listof (List Number Symbol)) -> BST)
(define (list->bst xss)
  (foldr (lambda (xs t)
           (create-bst t (car xs) (cadr xs)))
         (NONE) xss))


(define ls '((99 o) (77 l) (24 i) (10 h) (95 g) (15 d) (89 c) (29 b) (63 a)))

(define tt (list->bst ls))


; prints the structure of the binary tree t
(: print-by-levels : BST -> Void)
(define (print-by-levels t)
  ;; (: print-node : -> Void)
  (define (print-node t)
    (cond [(NONE? t) (void)]
          [(and (node? (node-left t)) (node? (node-right t)))
           (printf "[~a ~a]  L: [~a ~a]  R: [~a ~a] ||| "
                   (node-ssn t) (node-name t)
                   (node-ssn (node-left t)) (node-name (node-left t))
                   (node-ssn (node-right t)) (node-name (node-right t)))]
          [(node? (node-left t))
           (printf "[~a ~a]  L: [~a ~a] ||| "
                   (node-ssn t) (node-name t)
                   (node-ssn (node-left t)) (node-name (node-left t)))]
          [(node? (node-right t))
           (printf "[~a ~a]  R: [~a ~a] ||| "
                   (node-ssn t) (node-name t)
                   (node-ssn (node-right t)) (node-name (node-right t)))]
          [else (printf "[~a ~a] ||| " (node-ssn t) (node-name t))]))
  (: print-level : -> Void)
  (define (print-level xs)
    (begin (map print-node xs)
           (printf "\n")))
  (: list-children : Node -> (Listof Node))
  (define (list-children x)
    (cond [(NONE? x) null]
          [(and (node? (node-left x)) (node? (node-left x)))
           (list (node-left x) (node-right x))]
          [(node? (node-left x)) (list (node-left x))]
          [else (list (node-right x))]))
  (: list-next-level : (Listof Node) -> (Listof Node))
  (define (list-next-level xs)
    (flatten (map list-children xs)))
  (define (rec xs)
    (cond [(empty? xs) (void)]
          [else (begin (print-level xs)
                       (rec (list-next-level xs)))]))
  (rec (list t)))


(struct File [[name : String] [size : Integer] [content : String]])

(struct Dir [[name : String] [dirs : (Listof Dir)] [files : (Listof File)]])

(define d-a (Dir "dir6"
                (list (Dir "dir7"
                           null
                           (list (File "e" 9874 ""))))
                null))

(define d-b (Dir "dir7"
                null
                (list (File "e" 9874 ""))))

(define d-c (Dir "dir1"
               (list (Dir "dir2"
                          null
                          null)
                     (Dir "dir3"
                          (list
                           (Dir "dir8"
                                null
                                (list (File "e" 9345 ""))))
                          (list (File "e" 9345 "") (File "f" 923 "")))
                     (Dir "dir4"
                          (list
                           (Dir "dir5" null (list (File "e" 39 "") (File "f" 1425 "")))
                           (Dir "dir6"
                                (list (Dir "dir7" null (list (File "e" 65 ""))))
                                null))
                          (list (File "e" 9874 ""))))
               (list (File "a" 10 "") (File "e" 23 "") (File "b" 34 ""))))

(define d-d (Dir "dir1"
                 (list (Dir "dir2"
                            (list (Dir "dir3"
                                  (list (Dir "dir4"
                                             (list (Dir "dir5"
                                                        (list (Dir "dir6"
                                                                   (list (Dir "dir7"
                                                                              null
                                                                              (list (File "e" 9874 ""))))
                                                                   null))
                                                        null))
                                             null))
                                  null))
                       null))
                 null))

(define d-e (Dir "dir1"
                 (list (Dir "dir2"
                            null
                            null)
                       (Dir "dir3"
                            null
                            (list (File "e" 9345 "") (File "f" 45 "") (File "g" 9 ""))))
               (list (File "a" 10 "") (File "b" 34 ""))))

(define d-f (Dir "dir1"
                 (list (Dir "dir2"
                            null
                            null)
                       (Dir "dir3"
                            null
                            (list (File "e" 9345 "") (File "f" 45 "") (File "g" 9 "")))
                       (Dir "dir4"
                            null
                            (list (File "e" 9345 "") (File "f" 45 "") (File "g" 9 ""))))
                 (list (File "a" 10 "") (File "b" 34 "") (File "e" 23 ""))))

(define d-g (Dir "dir1"
                 (list (Dir "dir2"
                            (list (Dir "dir7"
                                       null
                                       (list (File "e" 9874 ""))))
                            null))
                 null))


;; ; determines whether a File with name fname occurs in the directory tree rooted at d
;; ; NOTE -- this function looks very confusing, but it works and does not contain redundancy.
;; ; Become familiar with reading folds that use explicit recursion (like this one),
;; ; since they solve problems elegantly.
;; ; Here, the 'or' in the fold is NOT 'or'ing across various hierarchical levels; it's only 'or'ing across
;; ; the list of dirs in a particular level. Remember, a fold is iterative, not recursive!
;; ; The if-clause is where the propagation happens. If the File is here, the #true value gets
;; ; returned as the value of the (exists? (Dir f)) in the previous fold_ expression.
;; ; This #true is not truly being "propagated" outward, though; it's only going up ONE level to the previous
;; ; fold_ expression. However, a #true resulting from a fold_ evaluation several levels below d DOES
;; ; propagate upward to d, since that's where a recursive call happens. The propagation happens
;; ; because of the recursive call, not because there's a fold being used. Again, fold_ is ITERATIVE.
;; ; In this case, fold_ just happens to combine the accumulator with a recursive call.
;; ; So it's just 'or'ing across (Dir-dirs root), and probing downward in each x in this list until it finds
;; ; a result, either by hitting a 'found', or by reaching a leaf directory. This result comes back
;; ; up to the top level, where it's 'or'ed with the results of the other directories in (Dir-dirs root)
;; ;   -- 'or'ing iteratively across the results of processing each of the dirs in (Dir-dirs root).
;; ;      This is short-circuited when one of these dirs returns #true.
;; ;   -- when processing each of these dirs in (Dir-dirs d), we only search deeper as needed.
;; ;      As with any recursion, the result propages upward to each Dir in (Dir-dirs root).
;; ;      If the File is here, we say so and stop searching any deeper.
;; ; Of course, this is a horrible way of doing it, compared to using ormap below.
;; ; But the point here is to become proficient in quickly reading what's going on in an arbitrary fold.
;; ; Dir -> String -> Boolean
;; (define (exists? d fname)
;;   (define here? (member fname (map File-name (Dir-files d))))
;;   (if here?
;;       #true
;;       ; search the next level
;;       (foldr (lambda (Dir bool)
;;                (or  bool  (exists? (Dir f))))
;;              #false (Dir-dirs d))))

(: exists? : Dir String -> Boolean)
(define (exists? d fname)
  (define here? (member fname (map File-name (Dir-files d))))
  (if here?
      #true
      ; search the next level
      (ormap  (lambda (Dir) (exists? Dir fname))  (Dir-dirs d))))


; gives list of all paths in the directory tree, rooted at d, that lead to files with name f, or else #false.
; return type is actually (Maybe (NEListOf (NEListOf String)))
; Dir -> String -> (Maybe [[String]])
(: find : Dir String -> (Option (Listof (Listof String))))
(define (find d fname)
  (define M_rss (letrec ([vsss (filter-map (lambda (Dir) (find Dir fname))
                                           (Dir-dirs d))]
                         [vss  (foldr append null vsss)])
                  (if (empty? vss) ; no successful paths
                      #false
                      vss)))
  (define here? (member fname (map File-name (Dir-files d))))
  ;; (: prepend-to-all : Dir (Listof (Listof Dir)) -> (Listof (Listof Dir)))
  (define (prepend-to-all d xss) (map (curry cons (Dir-name d))  xss))
  (cond [(or  (empty? (Dir-dirs d))  (false? M_rss))
         ; will not be combining with recursive result
         (if here?
             `((,(Dir-name d) ,fname))
             #false)]
     ; subtree contains file...
        [(not here?)  (prepend-to-all d M_rss)]
     ; File is here...
        [else  (prepend-to-all d (cons (list fname) M_rss))]))


(define (if-exists-then-find d f)
  (foldr (lambda (Dir M_qs)
           (cond [(false? M_qs)
                  (if (member f (Dir-files Dir))
                      (list Dir #false]
                 [(member f (Dir-files Dir))  (cons Dir M_qs)))]
         null (Dir-dirs d)))


; determines how many files are in the directory tree rooted at d
; Dir -> Integer
(: how-many-files : Dir -> Natural)
(define (how-many-files d)
  (+ (length (Dir-files d))
     (foldr (lambda (Dir q)
              (+ q (how-many-files Dir)))
            0 (Dir-dirs d))))


; determines how many files are in the directory tree rooted at d
(: how-many-files : Dir -> Natural)
(define (how-many-files d)
  (foldr (lambda (x acc)
           (cond [(dir? x)  (+ acc (how-many-files x))]
                 [else      (+ 1 acc)]))
         0 (dir-content d)))


;; (define (how-many-files d)
;;   (match d
;;     [null  0]
;;     [(cons head tail)
;;      (+ (how-many-files tail)
;;         (match head
;;           [(Dir name content)  (how-many-files content)]
;;           [other  1]))]))


; produces an expression like ex with all occurrences of x replaced by v
(: subst : BSL-var-expr Symbol Number -> BSL-var-expr)
(define (subst ex x v)
  (cond [(number? ex) ex]
        [(symbol? ex) v]
        [(add? ex) (add (subst (add-left ex)) (subst (add-right ex)))]
        [(mul? ex) (mul (subst (mul-left ex)) (subst (mul-right ex)))]))


(: eval-var* : BSL-var-expr AssocList -> Number)
(define (eval-var* ex da)
  (eval (foldl (λ (pair acc)
                       (subst (car pair) (cadr pair)))
               ex da)))


(: eval-var-lookup : BSL-var-expr AssocList -> Number)
(define (eval-var-lookup e da)
  (cond [(number? e) e]
        [(symbol? e) (cadr (assq e da))]
        [(add? e) (add (eval-var-lookup (add-left e)) (eval-var-lookup (add-right e)))]
        [(mul? e) (mul (eval-var-lookup (mul-left e)) (eval-var-lookup (mul-right e)))]))



(: get-index : (Listof Symbol) Natural -> Symbol)
(define (get-index ss n)
  (: go : (Listof Symbol) Natural Natural -> Symbol)
  (define (go ss n acc)
    (cond [(null? ss) (error "error: index out of bounds")]
          [(= acc n) (car ss)]
          [else (go (cdr ss) n (add1 acc))]))
  (go ss n 0))


; extracts the nth symbol from l;
; signals an error if there is no such symbol
(: list-pick : (Listof Symbol) Natural -> Symbol)
(define (list-pick l n)
  (cond
    [(empty? l) (error 'list-pick "list too short")]
    [(= n 0) (car l)]
    [(> n 0) (list-pick (cdr l) (sub1 n))]))


(: bundle : ((Listof String) Exact-Positive-Integer -> (Listof String)))
; given a list of 1Strings
(define (bundle xs n)
  (: go : (Listof String) Exact-Positive-Integer String -> (Listof String))
  (define (go xs n ts) ; ts builds up each chunk
    (cond [(null? xs)  (list ts)]
          [(= (string-length ts) n) ; cons ts onto result and reset ts for next iteration
           (cons ts (go (cdr xs) n (car xs) 1))]
          ; (< (string-length ts) n)
          [else  (go (cdr xs) n (string-append ts (car xs)))]))
    (if (= n 0)
        xs
        (go xs n "")))





(: list->chunks : All (a) (Listof a) Exact-Positive-Integer -> (Listof (Listof a)))
(define (list->chunks xs n)
  (cond [(= n 0)  xs]
        [(null? xs)  null]
        [else  (cons (take xs n) (list->chunks (drop xs n) n))]))



; produces a sorted version of xs
; Preserves duplicate elements
(: quick-sort< : (Listof Number) -> (Listof Number))
(define (quick-sort< xs)
  (define (largers xs n) (filter (curry < n) xs))
  (define (smallers xs n) (filter (curry >= n) xs))
  (cond [(null? xs)  null]
        [else (let ([pivot (first xs)])
                (append (quick-sort< (smallers (remove pivot xs) pivot))
                        (list pivot)
                        (quick-sort< (largers (remove pivot xs) pivot))))]))


; bundles a natural number length with an function that maps numbers from [0, length) to the set of Integers
(struct table ([len : Natural] [array : ( -> Exact-Number Exact-Number)]))

(struct table (len array))

(define t1 (table 1024 (lambda (x)
                         (if (<= 0 x 1023)
                             (- x 1000)
                             (error "t1: bad index")))))

(define t2 (table 100 (lambda (x)
                         (if (<= 0 x 99)
                             x
                             (error "t1: bad index")))))

; looks up the ith value in array of t
(: table-ref : table Exact-Number -> Exact-Number)
(define (table-ref t i)
  ((table-array t) i))


(: find-linear : table -> (Option Natural))
(define (find-linear t)
  ;; (: go : Exact-Nonnegative-Integer -> (Option Natural))
  (define (go i)
    (cond [(= i (table-len t))  #false] ; base case
          [(= (table-ref t i) 0)  i] ; found
          [else  (go (add1 i))]))
  (go 0))
; modify so that it finds the smallest root index


(: find-binary : Table -> (Option Natural))
(define (find-binary t)
  (define (go i j)
    (let ([pivot (and (<= i j) (floor (/ (+ i j) 2)))]) ; reached empty array if pivot is #f
      (and pivot ; base case: empty array
           ; pivot is valid...
           (cond [(= (table-ref t pivot) 0)  pivot] ; found
                 [(< (table-ref t pivot) 0)  (go (add1 pivot) j)] ; search right
                 [else  (go i (sub1 pivot))])))) ; search left
  (go 0 (sub1 (table-len t))))


(: find-binary : Table -> (Option Natural))
(define (find-binary t)
  (define (go i j)
    (let ([pivot (floor (/ (+ i j) 2))]
          [length (+ 1 (- j i))])
      (cond [(= length 1)  (and (= (table-ref t pivot) 0) pivot)]
            [(< length 1)  (error "should not happen")]
            [(= length 2)  (or (and (= (table-ref t pivot) 0) pivot) (go (add1 pivot) j))] ; if smaller isn't 0, check right
            [(= (table-ref t pivot) 0)  pivot] ; found
            [(< (table-ref t pivot) 0)  (go (add1 pivot) j)] ; search right
            [else  (go i (sub1 pivot))]))) ; search left
  (and (not (= (table-len t) 0)) (go 0 (sub1 (table-len t)))))
; empty array should never be in initial problem
; if A.length == 1, then if A[i] == x, then give i, else #false
; if A.length == 2, then if A[i] == x, then give i, else if A[i+1] == x, then give i+1, else give #false
; if A[i] == x  &&  A[i-1] == x  --> search left
; if A[i] == x  &&  A[i-1] < x  --> give i


(: height : Tree Integer Integer -> Integer)
(define (height t0)
  ; measures the height of t
  ; accumulator s is the number of steps it takes to reach t from t0
  ; accumulator m is the maximal height of the part of t0 that is to the left of t (redundant)

  (define (go t s m)
    (cond [(empty? t) (max s m)]
          [else
           (let ([height-left (height (tree-left t) (add1 s) (max s m))]
                 [height-right (height (tree-right t) (add1 s) (max s height-left))]) ; height-left >= m
             (max height-left
                  height-right))]))
  (go t0 0 0))


(: fib : Natural -> Natural)
(define (fib n)
  (cond [(= 0 n) 1]
        [(= 1 n) 1]
        [else (+ (fib (- n 1)) (fib (- n 2)))]))


(: fib : Natural -> Natural)
(define (fib n0)
  (: go : Natural Natural Natural -> Natural)
  ; accumulator a is fib (n0 - n); is (fib n0) by end; represents (fib (n-1))
  ; accumulator b is (fib (n0 - n - 1)); is (fib (n0 - 1)) by end; represents (fib (n-2))
  (define (go n a b)
    (cond [(= 0 n) a]
          [else (go (sub1 n) (+ a b) a)]))
  (go n0 1 0))


;; (define conn
;;   (http-conn-open "www.google.com"; no scheme nor path
;;                   #:ssl #t #:auto-reconnect #t))
;; (let*-values ([(status headers response)
;;               (http-conn-sendrecv! conn "/search?q=berries")])
;;   (displayln status)
;;   (displayln headers)
;;   (displayln (port->string response))
;;   ...
;;   (http-conn-close! conn)





;; WISH LIST OF FUNCTIONS:
