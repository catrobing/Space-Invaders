;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))            ;to get Y position of the tank

(define MISSILE (ellipse 5 15 "solid" "red"))


;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))


(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))


(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick
;;         the invader along y by dx pixels per clock tick

(define I0 (make-invader 150 0 INVADER-X-SPEED))
(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right
(define I4 (make-invader 200 100 10))           ;normal


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


(define G0 (make-game empty empty T0))
(define G00 (make-game empty empty T1))
(define G1 (make-game (list I1) (list M1) T1))
(define G2 (make-game (list I1 I2) (list M1 M2) T1))
(define G3 (make-game (list I1 I2 I3) (list M1 M2) T1))

;; ==================================================================

;; ListOfInvader is one of:
;;  - empty
;;  - (cons Invader ListOfInvader)
;; interp. a list of invaders

(define LOI0 empty)
(define LOI1 (list I1))
(define LOI2 (list I1 I2))
(define LOI3 (list I1 I2 I3))
(define LOI4 (list I1 I4))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]                   ;BASE CASE
        [else (... (first loi)                 ;Invader
                   (fn-for-loi (rest loi)))])) ;NATURAL RECURSION

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Invader ListOfInvader)
;;  - self-reference: (rest loi) is ListOfInvader

;; ListOfMissile is one of:
;;  - empty
;;  - (cons Missile ListOfMissile)
;; interp. a list of missiles

(define LOM0 empty)
(define LOM1 (list M1))
(define LOM2 (list M1 M2))
(define LOM3 (list M1 M2 M3))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]                   ;BASE CASE
        [else (... (first lom)                 ;Missile
                   (fn-for-lom (rest lom)))])) ;NATURAL RECURSION

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Missile ListOfMissile)
;;  - self-reference: (rest lom) is ListOfMissile

;;================================================
;; Game -> Game
;; start the world with game G0
;; 
(define (main game)
  (big-bang game                     ; Game
            (on-tick   nextgame)     ; Game -> Game
            (on-key    handlekey)    ; Game KeyEvent -> Game
            (to-draw   render)       ; Game -> Image
            (stop-when gameover?)))  ; Game -> Boolean

;;================= on-tick =========================
;; Game -> Game
;; produce the next game status
(check-expect (nextgame G0)  (make-game empty empty (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1))) ;cannot pass these tests due to new random invader, but still test for debug
(check-expect (nextgame G00) (make-game empty empty (make-tank (+ 50 TANK-SPEED) 1)))
(check-expect (nextgame G1)  (make-game (list (make-invader (+ 150 12) (+ 100 12) 12))
                                        (list (make-missile 150 (- 300 MISSILE-SPEED)))
                                        (make-tank (+ 50 TANK-SPEED) 1)))
(check-expect (nextgame (make-game (list (make-invader 150 0 10) (make-invader 150 100 1)) (list (make-missile 150 300) (make-missile 150 110)) T1))
                        (make-game (list (make-invader 160 10 10)) (list (make-missile 150 290)) (make-tank (+ 50 TANK-SPEED) 1)))

; (define (nextgame game) G0) ;stub

(define (nextgame g)
  (removehit (make-game (nextloi (game-invaders g) INVADE-RATE)   ; next list of invader
                        (nextlom (game-missiles g))               ; next list of missile
                        (nexttank (game-tank g)))))               ; next tank

;; Game -> Game
;; interp. remove hit missile and invader
; (define (removehit g) G1) ;stub
(define (removehit g)
  (make-game (removeinvader (game-invaders g) (game-missiles g)) (removemissile (game-missiles g) (game-invaders g)) (game-tank g)))

;; ListOfInvader -> ListOfInvader
;; interp. remove hit invader
(check-expect (removeinvader LOI1 LOM1) LOI1)
(check-expect (removeinvader LOI1 LOM2) empty)
(check-expect (removeinvader LOI2 LOM2) (list I2))
; (define (removeinvader loi lom) LOI1) ;stub
(define (removeinvader loi lom)
  (cond[(empty? loi) empty]
       [else (if (hitinvader? (first loi) lom)
                 (removeinvader (rest loi) lom)
                 (append (list (first loi)) (removeinvader (rest loi) lom)))]))

;; Invader ListOfMissile -> Boolean
;; interp. to check if an invader is hit by all the missiles
(check-expect (hitinvader? I1 LOM1) false)
(check-expect (hitinvader? I1 LOM2) true)
(check-expect (hitinvader? I1 LOM2) true)
; (define (hitinvader? I1 LOM1) false) ;stub
(define (hitinvader? i lom)
  (cond[(empty? lom) false]
       [else (if (hit? i (first lom))
                 true
                 (hitinvader? i (rest lom)))]))

;; Invader Missile -> Boolean
;; interp. to check if a missile hit a invader
(check-expect (hit? I1 M1) false)
(check-expect (hit? I1 M2) true)
(check-expect (hit? I1 M3) true)
; (define (hit? I1 M1) false) ;stub
(define (hit? i m)
  (cond[(or (empty? i) (empty? m)) false]
       [else (and (<= (abs (- (invader-x i) (missile-x m))) HIT-RANGE)
                  (<= (abs (- (invader-y i) (missile-y m))) HIT-RANGE))]))

;; ListOfMissile -> ListOfMissile
;; interp. remove hit missile
(check-expect (removemissile LOM1 LOI1) LOM1)
(check-expect (removemissile LOM2 LOI1) (list M1))
; (define (removemissile lom loi) LOM1) ;stub
(define (removemissile lom loi)
  (cond[(empty? lom) empty]
       [else (if (hitmissile? (first lom) loi)
                 (removemissile (rest lom) loi)
                 (append (list (first lom)) (removemissile (rest lom) loi)))]))

;; Missile ListOfInvader -> Boolean
;; interp. to check if a missile hit any invader
(check-expect (hitmissile? M1 LOI1) false)
(check-expect (hitmissile? M2 LOI1) true)
(check-expect (hitmissile? M3 LOI2) true)
;(define (hitmissile? M1 LOI1) false); stub
(define (hitmissile? m loi)
  (cond[(empty? loi) false]
       [else (if (hit? (first loi) m)
                 true
                 (hitmissile? m (rest loi)))]))

;; ListOfInvader -> ListOfInvader
;; interp. next list of invader
; (check-expect (nextloi LOI0 INVADE-RATE) empty)
; (check-expect (nextloi LOI1 INVADE-RATE) (list (make-invader (+ 150 12) (+ 100 12) 12)))
; (check-expect (nextloi LOI2) (list (make-invader (+ 150 12) (+ 100 12) 12) (make-invader (- 150 10) (+ HEIGHT 10) -10)))
; (define (nextloi loi) LOI0) ;stub
(define (nextloi loi rate)
  (cond [(empty? loi) (addinvader rate)]
        [else (append (list (nextinvader (first loi))) (nextloi (rest loi) rate))]))

;; Invader-Rate -> ListOfInvader
;; interp. create a new invader

; (define (addinvader rate) I1) ;stub

(define (addinvader rate)
  (cond[(> rate (random 2000)) (list (make-invader (random WIDTH) 0 INVADER-X-SPEED))]
       [else empty]))
  

;; Invader -> Invader
;; interp. move invader to next position
(check-expect (nextinvader I1) (make-invader (+ 150 12) (+ 100 12) 12))
(check-expect (nextinvader I2) (make-invader (- 150 10) (+ HEIGHT 10) -10))
(check-expect (nextinvader (make-invader 290 300 20)) (make-invader 300 320 -20))
(check-expect (nextinvader (make-invader 10 300 -10)) (make-invader 0 310 10))

; (define (nextinvader i) I1) ;stub
(define (nextinvader i)
  (cond[(and (>= (+ (invader-x i) (invader-dx i)) WIDTH) (> (invader-dx i) 0))
         (make-invader WIDTH (+ (invader-y i) (invader-dx i)) (-(invader-dx i)))]
       [(and (<= (+ (invader-x i) (invader-dx i)) 0) (< (invader-dx i) 0))
         (make-invader 0 (- (invader-y i) (invader-dx i)) (-(invader-dx i)))]
       [(> (invader-dx i) 0)
        (make-invader (+ (invader-x i) (invader-dx i)) (+ (invader-y i) (invader-dx i)) (invader-dx i))]
       [(< (invader-dx i) 0)
        (make-invader (+ (invader-x i) (invader-dx i)) (- (invader-y i) (invader-dx i)) (invader-dx i))]))

;; ListOfMissile -> ListOfMissile
;; interp. next list of missile
(check-expect (nextlom LOM0) empty)
(check-expect (nextlom LOM1) (list (make-missile 150 (- 300 MISSILE-SPEED))))
(check-expect (nextlom (list (make-missile 150 5) (make-missile 200 100))) (list (make-missile 200 (- 100 MISSILE-SPEED))))

; (define (nextlom lom) LOM0) ;stub

(define (nextlom lom)
  (cond [(empty? lom) empty]
        [else (if (< (- (missile-y (first lom)) MISSILE-SPEED) 0)
                  (nextlom (rest lom))
                  (append (list (make-missile (missile-x (first lom)) (- (missile-y (first lom)) MISSILE-SPEED))) (nextlom (rest lom))))]))

;; Tank -> Tank
;; interp. next tank
(check-expect (nexttank T1) (make-tank (+ 50 TANK-SPEED) 1))
(check-expect (nexttank T2) (make-tank (- 50 TANK-SPEED) -1))
(check-expect (nexttank (make-tank (- WIDTH 1) 1)) (make-tank WIDTH 1))
(check-expect (nexttank (make-tank (+ 0 1) -1)) (make-tank 0 -1))
 
; (define nexttank t) t) ;stub
 
(define (nexttank t)
  (cond[(and (>= (+ (tank-x t) TANK-SPEED) WIDTH) (= (tank-dir t) 1))
         (make-tank WIDTH 1)]
       [(and (<= (- (tank-x t) TANK-SPEED) 0) (= (tank-dir t) -1))
         (make-tank 0 -1)]
       [(= (tank-dir t)  1) (make-tank (+ (tank-x t) TANK-SPEED)  1)]
       [(= (tank-dir t) -1) (make-tank (- (tank-x t) TANK-SPEED) -1)]))

;;================= on-key ======================
;; Game Key -> Game
;; change direction of a tank by arrow <- or ->
(check-expect (handlekey G0 "up") G0)
(check-expect (handlekey G0 "left") (make-game empty empty (make-tank (/ WIDTH 2) -1)))
(check-expect (handlekey G0 "right") (make-game empty empty (make-tank (/ WIDTH 2) 1)))
(check-expect (handlekey G0 " ") (make-game empty (list (make-missile (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2))) (make-tank (/ WIDTH 2) 1)))
; (define (handlekey g ke) g) ;stub

(define (handlekey g ke)
  (cond[(key=? ke "left")  (make-game (game-invaders g) (game-missiles g) (go-left (game-tank g)))]
       [(key=? ke "right") (make-game (game-invaders g) (game-missiles g) (go-right (game-tank g)))]
       [(key=? ke " ")     (make-game (game-invaders g) (shoot (game-missiles g) (game-tank g)) (game-tank g))]
       [else g]))

;; Tank Key -> Tank
;; interp. tank go left
(check-expect (go-left T0) (make-tank (/ WIDTH 2) -1))
(check-expect (go-left T1) (make-tank 50 -1))
(check-expect (go-left T2) (make-tank 50 -1))

; (define (go-left t) t) ;stub
(define (go-left t)
  (make-tank (tank-x t) -1))

;; Tank Key -> Tank
;; interp. tank go right
(check-expect (go-right T0) (make-tank (/ WIDTH 2) 1))
(check-expect (go-right T1) (make-tank 50 1))
(check-expect (go-right T2) (make-tank 50 1))

; (define (go-right t) t) ;stub
(define (go-right t)
  (make-tank (tank-x t) 1))

;; Key ListOfMissile -> ListOfMissile
;; interp. tank shoot missile
(check-expect (shoot LOM0 T0) (list (make-missile (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2))))
(check-expect (shoot LOM1 T1) (append LOM1 (list (make-missile 50 (- HEIGHT TANK-HEIGHT/2)))))
(check-expect (shoot LOM2 T2) (append LOM2 (list (make-missile 50 (- HEIGHT TANK-HEIGHT/2)))))

; (define (shoot lom t) LOM1) ;stub
(define (shoot lom t)
  (cond [(empty? lom) (list (make-missile (tank-x t) (- HEIGHT TANK-HEIGHT/2)))]              
        [else (append lom (list (make-missile (tank-x t) (- HEIGHT TANK-HEIGHT/2))))]))


;;=============== to-draw ========================
;; Game -> Image
;; render game
(check-expect (render G0) (place-image TANK (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-expect (render G1) (place-image INVADER 150 100 (place-image MISSILE 150 300 (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))))
; (define (render game) BACKGROUND) ;stub

(define (render g)
  (display-loi (game-invaders g) (display-lom (game-missiles g) (display-tank (game-tank g)))))

;; Tank -> Image
;; interp. display tank
(check-expect (display-tank T0) (place-image TANK (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-expect (display-tank T1) (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
; (define (display-tank t)) ;stub

(define (display-tank t)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

;; LOM Image -> Image
;; interp. display missiles and tank
(check-expect (display-lom LOM0 BACKGROUND) BACKGROUND)
(check-expect (display-lom LOM1 BACKGROUND) (place-image MISSILE 150 300 BACKGROUND))
(check-expect (display-lom LOM2 BACKGROUND) (place-image MISSILE 150 300 (place-image MISSILE 150 110 BACKGROUND)))
; (define (display-lom lom i) BACKGROUND) ;stub

(define (display-lom lom i)
  (cond [(empty? lom) i]
        [else (place-image MISSILE (missile-x (first lom)) (missile-y (first lom)) (display-lom (rest lom) i))]))

;; LOI Image -> Image
;; interp. display invaders, missiles and tank

; (define (display-loi loi i) BACKGROUND) ;stub
(check-expect (display-loi LOI0 BACKGROUND) BACKGROUND)
(check-expect (display-loi LOI1 BACKGROUND) (place-image INVADER 150 100 BACKGROUND))
(check-expect (display-loi LOI2 BACKGROUND) (place-image INVADER 150 100 (place-image INVADER 150 HEIGHT BACKGROUND)))

(define (display-loi loi i)
  (cond [(empty? loi) i]       
        [else (place-image INVADER (invader-x (first loi)) (invader-y (first loi)) (display-loi (rest loi) i))])) 

;;============= stop-when =======================
;; Game -> Boolean
;; interp. to check if game is over
(check-expect (gameover? G0) false)
(check-expect (gameover? G1) false)
(check-expect (gameover? G2) true)
(check-expect (gameover? G3) true)
               
; (define (gameover? game) false) ;stub
(define (gameover? g)
  (invaderlanded? (game-invaders g)))

;; LOI -> Boolean
;; interp. to check if list of invader is landed
(check-expect (invaderlanded? LOI0) false)
(check-expect (invaderlanded? LOI1) false)
(check-expect (invaderlanded? LOI2) true)
(check-expect (invaderlanded? LOI3) true)

; (define (invaderlanded? loi) false) ;stub

(define (invaderlanded? loi)
  (cond [(empty? loi) false]                  
        [else
             (if (landed? (first loi))
                 true
                 (invaderlanded? (rest loi)))]))

;; Invader -> Boolean
;; check if a invader reaches the bottom
(check-expect (landed? I1) false)
(check-expect (landed? I2) true)
(check-expect (landed? I3) true)

; (define (landed? i) false) ;stub

(define (landed? i)
  (>= (invader-y i) HEIGHT))

;;======================================================
(define s1 (make-game empty empty (make-tank (/ WIDTH 2) 1)))
(define s2 (make-game (list (make-invader 150 0 10) (make-invader 150 100 1)) (list (make-missile 150 300) (make-missile 150 110)) T1))