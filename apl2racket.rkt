









¨   map_
⌈    ceiling, maximum
⌊    floor, minimum
+    conjugate, add
×    direction, multiply
*    exp, power
!    factorial, binomial e.g. 2!3
|    abs (magnitude (norm) of vector), mod e.g. (2 mod 3)
⍟    natural log, log base a of b e.g. (log a b)
⊥    dyadic decode    (e.g. 2 ⊥ [1 1 0 3] == 13) ([24 60 60] ⊥ [2 46 40] == 10000)
⊤    dyadic encode (e.g. '[24 60 60] ⊤ 10000 == [2 46 40])
,    flatten, concat (e.g. ∴ M←[2 2] ⍴ ⍳ 4  in  M,99 == [[1 2 99]
                                                       [3 4 99]])
⌽    reverse (can take optional axis# arg), rotate (can take optional axis# arg)
                                              (e.g. 3 ⌽ 'HatStand' == StandHat)
                                              (e.g. ¯2 ⌽ [1 2 3 4 5 6] == [5 6 1 2 3 4])
≡    depth, match
∈    enlist (matrix→list), member
⊃    head_, pick  (e.g. [2 1] ⊃ [[1 2][3 4 5]] == 2)
↑    mix, take (negative args specify take-right)
↓    split, drop (negative args specify drop-right)
∪    unique (remove all_ duplicates), union
∩    intersection
~    logical not, set difference (e.g. [3 1 4 1 5] ~ [5 1] == [3 4])
⍳    index generator, index-of (e.g. 'ABCDABCDEF' ⍳ 'ACF' == [1 3 10])



⍸    where (give indices of truthy elements),




⍴     shape (e.g. ∴ M←[3 4]⍴⍳12), reshape (e.g. [2 4] ⍴ [1 2 3 4 5 6 7] == [[1 2 3 4]
                                                                            [5 6 7 1]]  in  ⍴ M == [3 4])
(e.g. [2 3] ⍴ [1 2 3 4 5 6 7] == [[1 2 3]
                                  [4 5 6]])
\    expand, scan
⍀    expand 1st, scan 1st
/    fold, replicate (e.g. [3 1 ¯2 2] / [6 7 8 9] == [6 6 6 7 0 0 9 9]),
                     (e.g. [1 0 1 0 1] / 'Heart' == Hat)
⌿    reduce 1st, replicate 1st
                              M
                              1  2  3  4
                              5  6  7  8
                              9 10 11 12

                              1 0 2 ⌿ M

                              1  2  3  4
                              9 10 11 12
                              9 10 11 12

                              monadic operator: reduce first
                              +⌿ M
                              15 18 21 24

                              2 +⌿ mat     ⍝ pair-wise
                              6  8 10 12
                              14 16 18 20



















;; ⍪    table, append to 1st axis
;; ⌹    matrix inverse, matrix divide
;; ⊢    right
;; ⊣    left
;; ⊖,⦵  reverse 1st, rotate 1st
;; ⍉     transpose, dyadic transpose
;; ⊆     nest, partition
;; ⊂     enclose, partitioned enclose
;; ?     roll (e.g. ? [6 6 6 6 6] == [4 3 6 3 5]), deal (e.g. 4 ? 100 == [34 5 97 73])
;; ÷     reciprocal, divide
;; ○     multiply by pi, circular functions (trig) (angles given in radians)









;; a←(¯1+⍳⍴⍺)∘.=(⍳⍴1↓⍺,⍵)∘.-⍳⍴⍵
;; pp:⍺+.×((¯1+⍳⍴⍺)∘.=(⍳⍴1↓⍺,⍵)∘.-⍳⍴⍵)+.×⍵
;; (1↓c×¯1+⍳⍴c) P x
;; (a,c÷⍳⍴c) P x
;; c P(x+y) ←→ (((j∘.!j)×y*0⌈-j∘.-j←¯1+⍳⍴c)+.×c)P x
;; gcd:gcd m,(m←⌊/r)|r:1≥⍴r←(⍵≠0)/⍵:+/r
;; lcm:(×/x)÷gcd x←(1↑⍵),lcm 1↓⍵:0=⍴⍵:1
;; c←((0,⍳⍴r)∘.=+⌿~m)+.×(-r)×.*m←T ⍴r
;; rfc:(¯1+⍳⍴1↓⍵)g ⍵
;; g:(⍺-z)g ⍵:tol≥⌈/|z←⍺ step ⍵:⍺-z
;; step:(⌹(⍺∘.-⍺)×.*i∘.≠i←⍳⍴⍺)+.×(⍺∘.*¯1+⍳⍴⍵)+.×⍵
;; ⎕←c←cfr 2 3 5 7
;; tol←1e¯8
;; bfd:⍵∘.=⍳⍴⍵
;; dfb:⍵+.×⍳1↑⍴⍵
;; dfr:⍵[1],x+⍵[1]≤x←dfr 1↓⍵:0=⍴⍵:⍵
;; rfd:⍵[1],rfd x-⍵[1]≤x←1↓⍵:0=⍴⍵:⍵
;; par:2|+/,(i∘.>i←⍳⍴⍵)∧⍵∘.>⍵
;; (lfc c)∘.=⍳1↑⍴c
;; a∨.∧b ←→ ~(~a)∧.∨(~b)
;; a^.=b ←→ ~(~a)∨.≠(~b)
;; a⌊.+b ←→ -(-a)⌈.+(-b)
;; v+.×m ←→ ((k↑v)+.×(k,1↓⍴m)↑m)+(k↓v)+.×(k,0)↓m
;; (i,j)↓a+.×v ←→ ((i,j,0)↓a)+.×v
;; ((c P x+y)-((0*j)+.×(a←ds j∘.!j←¯1+⍳⍴c)+.×c) P x)÷y
;; ⍺⌹⍵←→(⌹⍵)+.×⍺
;; mf:⍵[,1],[1]x+⍵[(1↑⍴x)⍴1]≤x←mf 1 0↓⍵:0=1↑⍴⍵:⍵


















