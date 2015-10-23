; === BIN-PACKING GAME ===
; look here for project assignment: http://www.ccs.neu.edu/course/cs2500f15/ps6.html

(require 2htdp/universe)
(require 2htdp/image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
(define WWIDTH 300) ; pixels
(define WHEIGHT 600) ; pixels
(define GRID-ROWS 20) ; grid units
(define GRID-COLS 10) ; grid units
(define CELL-SIZE (/ WWIDTH GRID-COLS)) ; pixels
(define TICK-SPEED .2) ; seconds/tick
(define BG (empty-scene WWIDTH WHEIGHT))

; Grid units X increase to the right
; Grid units Y increase down

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions

; A Block is a (make-block Number Number Color)
; X and Y are in grid cells
(define-struct block (x y color))

; A Tetra is a (make-tetra Posn BSet)
; The center point is the point around which the tetra rotates
; When it spins.
; The posn is in grid cells
(define-struct tetra (center blocks))

; A Set of Blocks (BSet) is one of:
; - empty
; - (cons Block BSet)
; Order does not matter

; A World is a (make-world Tetra BSet)
; The BSet represents the pile of blocks at the bottom of the screen.
(define-struct world (tetra pile))

; Any -> World
; Start up the game.
(define (main _unused)
  (big-bang WORLD0
            [to-draw render]
            [on-key got-key]
            [on-tick update-world TICK-SPEED]
            [stop-when pile-touch-top?]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7 "Kinds" of Tetras

; BSet
(define O (cons (make-block 0 0 'green)
                (cons (make-block 0 1 'green)
                      (cons (make-block 1 0 'green)
                            (cons (make-block 1 1 'green) empty)))))

(define I (cons (make-block 0 0 'blue)
                (cons (make-block 1 0 'blue)
                      (cons (make-block 2 0 'blue)
                            (cons (make-block 3 0 'blue) empty)))))

(define L (cons (make-block 2 0 'purple)
                (cons (make-block 0 1 'purple)
                      (cons (make-block 1 1 'purple)
                            (cons (make-block 2 1 'purple) empty)))))

(define J (cons (make-block 0 0 'lightblue)
                (cons (make-block 0 1 'lightblue)
                      (cons (make-block 1 1 'lightblue)
                            (cons (make-block 2 1 'lightblue) empty)))))

(define T (cons (make-block 1 0 'orange)
                (cons (make-block 0 1 'orange)
                      (cons (make-block 1 1 'orange)
                            (cons (make-block 2 1 'orange) empty)))))

(define Z (cons (make-block 0 0 'pink)
                (cons (make-block 1 0 'pink)
                      (cons (make-block 1 1 'pink)
                            (cons (make-block 2 1 'pink) empty)))))

(define S (cons (make-block 1 0 'red)
                (cons (make-block 2 0 'red)
                      (cons (make-block 0 1 'red)
                            (cons (make-block 1 1 'red) empty)))))

; Tetras

(define tetO (make-tetra (make-posn 0 0) O))
(define tetI (make-tetra (make-posn 0 0) I))
(define tetL (make-tetra (make-posn 0 1) L))
(define tetJ (make-tetra (make-posn 0 0) J))
(define tetT (make-tetra (make-posn 0 1) T))
(define tetZ (make-tetra (make-posn 0 0) Z))
(define tetS (make-tetra (make-posn 0 1) S))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; make-random-tetra : Number -> Tetra
; makes tetra based on number (which will be randomly selected)
(define (make-random-tetra n)
  (cond [(= n 0) tetO]
        [(= n 1) tetI]
        [(= n 2) tetL]
        [(= n 3) tetJ]
        [(= n 4) tetT]
        [(= n 5) tetZ]
        [(= n 6) tetS]))

;Initial World: 
(define WORLD0 (make-world (make-random-tetra (random 7)) empty))

; Tetras:

; Sets of Tetras:
(define BSET1 (cons tetO (cons tetI empty)))
(define set1 (list (make-block 10 10 'blue) (make-block 7 15 'blue) (make-block 8 17 'blue)))
(define set2 (list (make-block 9 9 'red) (make-block 5 5 'red)))

; Worlds:
(define WORLD1 (make-world tetI set1))
                
                
;;;;;;;;;;;;;;;;;;;;
;; To-Draw

; World -> Image
(define (render w)
 (draw-tetra (world-tetra w) (draw-pile w)))

; World -> Image
; Add the "pile" of boxes to the scene
(define (draw-pile w)
  (draw-blocks (world-pile w) BG))



; Tetra Image -> Image
; Add a Tetra to the scene
(define (draw-tetra t scn)
  (draw-blocks (tetra-blocks t) scn))


; BSet Image -> Image
; Add a set of boxes to the Scene
(define (draw-blocks set scn)
  (cond [(empty? set) scn]
        [(cons? set)
         (draw-block (first set)
                   (draw-blocks (rest set) scn))]))

; Block Image -> Image
; Add one Block to the scene
(define (draw-block b scn)
  (place-image/grid (square CELL-SIZE
                            "solid"
                            (block-color b))
                    (block-x b)
                    (block-y b)
                    scn))

; place-image/grid : Image Number Number Image -> Image
; Add an image using grid coordinates.
(define (place-image/grid fg x y bg)
  (place-image fg
               (* x CELL-SIZE)
               (* y CELL-SIZE)
               bg))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; On-Key

; World KeyEvent -> World
(define (got-key w ke)
   (cond [(string=? ke "left") (tetra-shift-left (world-tetra w))]
         [(string=? ke "right") (tetra-shift-right (world-tetra w))]
         [(string=? ke "s") (tetra-rotate-cw (world-tetra w))]
         [(string=? ke "a") (tetra-rotate-ccw (world-tetra w))]
         [else w]))

; tetra-off-grid? Tetra -> Boolean
; true if any blocks in tetra outside parameters of grid
(define (tetra-off-grid? t)
  (blocks-off-grid? (tetra-blocks t)))


; blocks-off-grid? BSet -> Boolean
; true if any blocks in set outside parameters of grid
(define (blocks-off-grid? s)
  (cond [(empty? s) false]
        [(cons? s) (if (block-off-grid? (first s))
                       true
                       (blocks-off-grid? (rest s)))]))

; block-off-grid? Block -> Boolean
; true if block is outside parameters of grid, false else
(define (block-off-grid? b)
  (and (< (block-x b) 0) (> (block-x b) GRID-COLS)))

; update-posn-left: Posn -> Posn
; moves posn 1 grid unit to the left
(define (update-posn-left p)
  (make-posn (- (posn-x p) 1) (posn-y p)))

; update-posn-right: Posn -> Posn
; moves posn 1 grid unit to the right
(define (update-posn-right p)
  (make-posn (+ (posn-x p) 1) (posn-y p)))

; update-posn-down: Posn -> Posn
; moves posn 1 grid unit down
(define (update-posn-down p)
  (make-posn (posn-x p) (- (posn-y p) 1)))

; blocks-rotate ccw : Tetra -> BSet
; Rotate the set of blocks counterclockwise around the posn.
(define (blocks-rotate-ccw t)
  (cond [(empty? (tetra-blocks t)) empty]
        [(cons? (tetra-blocks t)) (cons (block-rotate-ccw (tetra-center t)
                                                          (first (tetra-blocks t)))
                                        (blocks-rotate-ccw (rest (tetra-blocks t))))]))

; blocks-rotate cw : Tetra -> BSet
; Rotate the set of blocks clockwise around the posn.
(define (blocks-rotate-cw t)
  (cond [(empty? (tetra-blocks t)) empty]
        [(cons? (tetra-blocks t)) (cons (block-rotate-ccw (tetra-center t)
                                                          (first (tetra-blocks t)))
                                        (blocks-rotate-cw (rest (tetra-blocks t))))]))

; tetra-rotate-ccw : Tetra -> Tetra
; Makes a new tetra rotated 90 counterclockwise around its center.
(define (tetra-rotate-ccw t)
  (if (tetra-off-grid? (make-tetra (tetra-center t) (blocks-rotate-ccw t)))
      t
      (make-tetra (tetra-center t) (blocks-rotate-ccw t))))

; tetra-rotate-cw : Tetra -> Tetra
; Makes a new tetra rotated 90 clockwise around its center.
(define (tetra-rotate-cw t)
  (if (tetra-off-grid? (make-tetra (tetra-center t) (blocks-rotate-cw t)))
      t
      (make-tetra (tetra-center t) (blocks-rotate-cw t))))


; block-rotate-ccw : Posn Block -> Block
; Rotate the block 90 counterclockwise around the posn.            
(define (block-rotate-ccw c b)
  (make-block (+ (posn-x c)
                 (- (posn-y c)
                    (block-y b)))
              (+ (posn-y c)
                 (- (block-x b)
                    (posn-x c)))
              (block-color b)))

; block-rotate-cw : Posn Block -> Block
; Rotate the block 90 clockwise around the posn.
(define (block-rotate-cw c b)
  (block-rotate-ccw c (block-rotate-ccw c (block-rotate-ccw c b))))



; blocks-shift-left : BSet -> BSet
; Makes new BSet, blocks shifted 1 coordinate to the left
(define (blocks-shift-left b)
  (cond [(empty? b) empty]
        [(cons? b) (cons (make-block (- (block-x (first b)) 1)
                                     (block-y (first b))
                                     (block-color (first b)))
                         (blocks-shift-left (rest b)))]))

; blocks-shift-right : BSet -> BSet
; Makes new BSet, blocks shifted 1 coordinate to the right
(define (blocks-shift-right b)
  (cond [(empty? b) empty]
        [(cons? b) (cons (make-block (+ (block-x (first b)) 1)
                                     (block-y (first b))
                                     (block-color (first b)))
                         (blocks-shift-right (rest b)))]))

; tetra-shift-left : Tetra -> Tetra
; Makes new Tetra, shifted 1 coordinate to the left
(define (tetra-shift-left t)
  (if (tetra-off-grid? (make-tetra (update-posn-left (tetra-center t))
                                   (blocks-shift-left (tetra-blocks t))))
      t
      (make-tetra (update-posn-left (tetra-center t))
                   (blocks-shift-left (tetra-blocks t)))))

; tetra-shift-right : Tetra -> Tetra
; Makes new Tetra, shifted 1 coordinate to the right
(define (tetra-shift-right t)
  (if (tetra-off-grid? (make-tetra (update-posn-right (tetra-center t))
                                   (blocks-shift-right (tetra-blocks t))))
      t
      (make-tetra (update-posn-right (tetra-center t))
                  (blocks-shift-right (tetra-blocks t)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; On-Tick

; update-world : World -> World
; changes the state of the world
(define (update-world w)
  (make-world (update-tetra w) (update-pile w)))

; update-tetra : world -> world
; changes state of tetra
(define (update-tetra w)
  (if (or (tetra-touch-ground? (world-tetra w))
          (tetra-on-blocks? (world-tetra w) (world-pile w)))
      (drop-tetra (world-tetra w))
      (add-tetra (world-tetra w) (world-pile w))))

; update-pile : world -> world
; Add tetras to the pile
(define (update-pile w)
  (make-world (world-tetra w) (add-tetra (world-tetra w) (world-pile w))))


; add-tetra : Tetra BSet -> BSet
; Adds a tetra to a BSet if touching ground or the set
(define (add-tetra t s)
  (append (tetra-blocks t) s))

; tetra-touch-ground? : Tetra -> Boolean
(define (tetra-touch-ground? t)
  (blocks-touch-ground? (tetra-blocks t)))

; blocks-touch-ground? : BSet -> Boolean
; Are any of the blocks in the set touching the bottom edge of the screen?
(define (blocks-touch-ground? set)
  (cond [(empty? set) false]
        [(cons? set) (if (block-touch-ground? (first set))
                         true
                         (blocks-touch-ground? (rest set)))]))

; tetra-on-blocks? : Tetra BSet -> Boolean
; Is the tetra touching any of the blocks in the BSet?
(define (tetra-on-blocks? t s)
  (blocks-on-blocks? (tetra-blocks t) s))


; pile-touch-top? : World -> Boolean
; Are any of the blocks in the pile touching the top of the screen?
(define (pile-touch-top? w)
  (blocks-touch-top? (world-pile w)))


; blocks-touch-top? : BSet -> Boolean
; Are any of the blocks in the set touching the top of the grid?
(define (blocks-touch-top? s)
  (cond [(empty? s) false]
        [(cons? s) (if (block-touch-top? (first s))
                       true
                       (blocks-touch-top? (rest s)))]))

; block-touch-top? : Block -> Boolean
; Is the block touching the top of the grid?
(define (block-touch-top? b)
  (< (block-y b) 0))

(check-expect (block-touch-top? (make-block 9 10 'blue)) false)
(check-expect (block-touch-top? (make-block 0 -1 'blue)) true)


; block-touch-ground? : Block -> Boolean
(define (block-touch-ground? b)
  (>= (block-y b) GRID-ROWS))

; blocks-on-blocks? : BSet BSet -> Boolean
; Are any blocks in the first set on top of any blocks in second set?
(define (blocks-on-blocks? set1 set2)
  (cond [(empty? set1) false]
        [(cons? set1) (if (block-on-blocks? (first set1) set2)
                           true
                           (blocks-on-blocks? (rest set1) set2))]))

; block-on-blocks? : Block BSet -> Boolean
; Is the block on top of any blocks in the BSet?
(define (block-on-blocks? b s)
  (cond [(empty? s) false]
        [(cons? s) (if (block-on-block? b (first s))
                       true
                       (block-on-blocks? b (rest s)))]))

; block-on-block? : Block Block -> Boolean
; Is the first block on top of the second?
(define (block-on-block? first second)
  (and (= (block-x first) (block-x second))
       (= (block-y first) (block-y second))))

; block->posn : Block -> Posn
; Find position of a block
(define (block->posn b)
  (make-posn (block-x b) (block-y b)))


; drop-tetra : tetra -> tetra
; Shift tetra down 1 space
(define (drop-tetra t)
  (make-tetra (update-posn-down (tetra-center t)) (drop-blocks (tetra-blocks t))))

; drop-blocks : BSet -> BSet
; Shift all blocks in the set down 1 space
(define (drop-blocks s)
  (cond [(empty? s) empty]
        [(cons? s) (cons (drop-block (first s))
                         (drop-blocks (rest s)))]))

; drop-block : Block -> Block
; Make new block shifted 1 space down
(define (drop-block b)
  (make-block (block-x b) (+ (block-y b) 1) (block-color b)))


