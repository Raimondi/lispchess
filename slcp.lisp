

; todo: blocking central pawns
; todo: develop pieces, king safety?

(defun l()(load "slcp2.lisp"))

(require 'asdf)
(asdf:operate 'asdf:load-op 'sqlite)
(use-package :sqlite)
(use-package :iter)

(defparameter *db* nil)

(defun db()
  (unless *db* (setf *db* (connect "slcp.db" :busy-timeout 1000))) *db*)


; a chess square and its accessors.  Google for the x88 algorithm

(defmacro def( &rest args )`(defparameter ,@args))

(defun x88-ok?( square )(zerop (logand square #x88)))
(defun get-row( square )    (ash square -4))
(defun get-col( square ) (logand square #xF))
(defun square-to-string( square )(format nil "~d~d" (aref "abcdefgh" (get-col square))(aref "87654321" (get-row square))))

(def x88-squares (remove-if-not 'x88-ok? (loop for a to 128 collect a)))

(defmacro board-loop( &rest form )
  "a macro to loop over the valid x88 squares, putting the index in the SQ variable"
  `(loop for sq fixnum in x88-squares ,@form))

(defmacro if-0 ( test body )  `(if ,test ,body 0))

(defmacro ms() "make constants that refer to all the chessboard squares"
  `(progn ,@(board-loop collect
    (list 'defconstant (read-from-string (square-to-string sq)) sq))))
(ms)

; a piece is a class with methods to return its value, play a move for that piece, get-row moves, eval that piece on a square, etc

(defmacro make-arr( size form )
  `(let ((arr (make-array ,size)))
     (loop for i below ,size do (setf (aref arr i) ,form)) arr))

(defun make-hash-lookup()
  (make-arr 128 (random most-positive-fixnum)))
    
(defclass piece ()
  ((value :reader value :initarg :value)
   (hash  :reader hash  :initform (make-hash-lookup) :initarg :hash)
   (glyph :reader glyph :initarg :glyph)
   (cache :reader cache :initform (make-array '(128 128)))))

; the chess board, its various state variables

(def empty-square (make-instance 'piece :value 0 :glyph " " :hash (make-array 128 :initial-element 0)))

(def *board* (make-array 128 :initial-element empty-square))
(def *halfmoves* 0)
(def *to-move* 1)

(defmacro board(square)`(aref *board* ,square))

(defun print-board( &optional (bd *board*) (func 'glyph) )
  (let ((div "+---+---+---+---+---+---+---+---+~%"))
    (format t div)
    (loop for row from 0 to 7 do
	  (loop for col from 0 to 7 as sq = (+ col (* row 16)) do
		(format t "| ~d " (funcall func (elt bd sq))))
	  (format t "|~%")
	  (format t div))))
		
; we maintain a "board hash" by xoring a value for a piece and a square whenever we add or remove a piece
(def *hash* 0)
(declaim (type fixnum *hash*))

;(defgeneric hash-ps( piece square )(:method-combination progn))

;(defmethod hash-ps progn ( (piece piece) square )
;  (setf *hash* (logxor *hash* (aref (hash piece) square))))

(defmethod print-object( (piece piece) stream)
  (format stream "#~d" (glyph piece)))

(defun side( piece )(signum (value piece)))
(defgeneric generate( piece square ))
(defgeneric evaluate( piece square ) (:method-combination +)) ; all applicable eval functions will be called and summed

; the move class

(defclass move ()
  ((from :reader from :initarg from)
   (to   :reader to   :initarg to)))

(defun move=( m1 m2 )
  (and (= (from m1) (from m2)) (= (to m1) (to m2))))

(defgeneric extra(move))

(defmethod extra( (m move) ) "")

(defmethod print-object( (move move) stream )
  (format stream "~d~d~d" 
	  (square-to-string (from move)) 
	  (square-to-string (to move))
	  (extra move)))

(defparameter generated-moves nil)
(defparameter move-hash (make-hash-table))

(defun foo( &rest args )
  args)

(defun add-move( type from to &rest args )
  (if (= (abs (value (board to))) 10000)
      (throw 'mate 'mate))
  "create a cache of generated moves because allocating objects is expensive"
  (let ((cache (gethash type move-hash))
	(p-to (when (eq (first args) :promotes-to) (second args))))
    (unless cache
      (setf cache (make-array '(128 128) :initial-element nil))
      (setf (gethash type move-hash) cache))
    (let ((move (aref cache from to)))
      (unless move
	(setf move 
	      (if p-to 
		  (loop for p in p-to collect (apply 'make-instance type `from from `to to :promotes-to p (rest (rest args ))))
		  (apply 'make-instance type `from from `to to args)))
	(setf (aref cache from to) move))
      (if (consp move)
	  (loop for m in move do (push m generated-moves))
	  (push move generated-moves)))))

(defgeneric play( move ))
(defgeneric unplay( move ))

; a simple move just removes a piece from the source square and places it on the target square

(defparameter *captures* nil)

(defgeneric adjust-remove( piece square ) (:method-combination progn))
(defgeneric adjust-add   ( piece square ) (:method-combination progn))

(def white-material -10000)
(def black-material -10000)

(defmacro hashm( piece square variable )
  `(setf ,variable (logxor ,variable (aref (hash ,piece) ,square))))

(defmethod adjust-add progn ( (piece piece) square )
  (hashm piece square *hash*)
  (let ((v (value piece)))
    (if (plusp v) (incf white-material v) (decf black-material v))))

(defmethod adjust-remove progn ( ( piece piece ) square )
  (hashm piece square *hash*)
  (let ((v (value piece)))
    (if (plusp v) (decf white-material v) (incf black-material v))))

(defun remove-piece-at( square )
  (let ((piece (board square)))
    (adjust-remove piece square)
    (setf (board square) empty-square)
    ;(hash-ps piece square)
    piece))

(defun push-piece-at( square ) 
  (push (board square) *captures*)
  (remove-piece-at square))

(defun add-piece-at( piece square )
  ;(format t "adding piece ~d at ~d~%" piece square)
  (adjust-add piece square)
  (assert (eq (board square) empty-square))
  (setf (board square) piece))
  
(defun pop-piece-at( square )
  (let ((piece (pop *captures*)))
    (add-piece-at piece square)))

(defun slide( from to )
  ;(format t "slide from ~d to ~d~%" (square-to-string from) (square-to-string to))
  (add-piece-at (remove-piece-at from) to))

(defparameter *played-moves* nil)

(defun flip-to-move()
  (setf *to-move* (- *to-move*))
  (setf *hash* (logxor *hash* #xffffff)))

(def traffic (make-array 128 :initial-element 0))

(defparameter *seen-hash* (make-hash-table))
(defmacro times-current-seen()`(gethash *hash* *seen-hash* 0))

(defmethod play( move ) 
  (incf *halfmoves*)
  (incf (aref traffic (from move)))
  (incf (aref traffic (to move)))
  (flip-to-move)
  (push move *played-moves*)
  (push-piece-at (to move))
  (slide (from move) (to move))
  (incf (times-current-seen)))


(defmethod unplay( move )
  (decf (times-current-seen))
  (decf *halfmoves*)
  (decf (aref traffic (from move)))
  (decf (aref traffic (to move)))
  (flip-to-move)
  (pop *played-moves*)
  (slide (to move) (from move))
  (pop-piece-at (to move)))

; hoppers are pieces that can jump to any set of related squares

(defclass pawn ( piece )
  ((pcount :accessor pcount :initform 0) ; pawn count
   (cols   :reader   cols   :initform (make-arr 8 (cons 0 nil)))))

(defclass white-pawn (pawn)())
(defclass black-pawn (pawn)())

(def white-pawn (make-instance 'white-pawn :value  100 :glyph "P"))
(def black-pawn (make-instance 'black-pawn :value -100 :glyph "p"))

(defclass hopper (piece) () )

(def centralize (make-array 128 :initial-contents
  `(-35 -35 -35 -35 -35 -35 -35 -35     0 0 0 0 0 0 0 0  
    1 2 4 6 6 4 3 1     0 0 0 0 0 0 0 0 
    1 4 9 16 16 9 4 1   0 0 0 0 0 0 0 0 
    1 9 16 32 32 16 9 1 0 0 0 0 0 0 0 0 
    1 9 16 32 32 16 9 1 0 0 0 0 0 0 0 0 
    1 4 9 16 16 9 4 1   0 0 0 0 0 0 0 0
    1 2 4 6 6 4 3 1     0 0 0 0 0 0 0 0 
    -35 -35 -35 -35 -35 -35 -35 -35     0 0 0 0 0 0 0 0 )))

(defmethod evaluate + ( (piece hopper) square )
  (aref centralize square ))
  
(defgeneric deltas ( hopper ) )

(defmethod generate( (piece hopper) square ) "hopper move generator"
  (loop as side =    (side piece) 
	for delta in (deltas piece)
	for to =     (+ square delta) do
	(when (and (x88-ok? to) (/= side (side (board to))))
	  (add-move 'move square to))))

; let's define a few hoppers

; knights 

(defclass knight (hopper) ())
(def knight-deltas '(-33 -31 -18 -14 14 18 31 33))
(defmethod deltas( (hopper knight) ) knight-deltas)

(defparameter white-knight (make-instance 'knight :value  325 :glyph "N" ))
(defparameter black-knight (make-instance 'knight :value -325 :glyph "n"))

; kings

(defclass king (hopper) ((at :accessor at)))
(def king-deltas '(-17 -16 -15 -1 1 15 16 17))
(defmethod deltas( (hopper king) ) king-deltas )

  
(defclass white-king(king)())
(defclass black-king(king)())

(defun white-attack() (1- (/ white-material 2000.0)))
(defun black-attack() (1- (/ black-material 2000.0)))

(def king-safety (make-array 128 :initial-contents
  `(60 60 20 30 30 20 60 60     0 0 0 0 0 0 0 0  
    25 5 5 -10 -10 5 5 25     0 0 0 0 0 0 0 0 
    -20 -30 -40 -50 -50 -40 -30 -20   0 0 0 0 0 0 0 0 
    -40 -60 -80 -100 -100 -80 -60 -40 0 0 0 0 0 0 0 0 
    -40 -60 -80 -100 -100 -80 -60 -40 0 0 0 0 0 0 0 0 
    -20 -30 -40 -50 -50 -40 -30 -20   0 0 0 0 0 0 0 0
    25 5 5 -10 -10 5 5 25     0 0 0 0 0 0 0 0 
    60 60 20 30 30 20 60 60     0 0 0 0 0 0 0 0 )))

(defun file-open?( pawn col )
  (zerop (car (aref (cols pawn) col))))

(defmethod evaluate + ( (piece white-king) square) ; todo: other king safety scores
  (floor (* (black-attack) 
            (+ (aref king-safety square)
               ))))

(defmethod evaluate + ( (piece black-king) square) ; todo:  other king safety scores
  (floor (* (white-attack)
            (+ (aref king-safety square)
               ))))


(defmethod adjust-add progn ( (king king) square )
  (setf (at king) square))

(defparameter white-king (make-instance 'white-king :value  10000 :glyph "K"))
(defparameter black-king (make-instance 'black-king :value -10000 :glyph "k"))

(defun empty?( square )
  (eq (board square) empty-square))

; sliding pieces

(defclass slider ( piece ) ((dirs :accessor dirs :initarg :dirs)))

(defun add-if-capture( type from to ) 
  (when (= (side (board to)) (- *to-move*))
    (add-move type from to)))

(defun add-ray( square dir )
  (let ((dest (+ square dir)))
    (loop while (x88-ok? dest) do 
	  (progn 
	    (unless (eq (board dest) empty-square)
	      (add-if-capture 'move square dest)
	      (return))
	    (add-move 'move square dest)
	    (incf dest dir)))))

(defmethod generate( (piece slider) square )
  (loop for dir in (dirs piece) do (add-ray square dir)))

(defclass rook( slider ) ())
(defclass white-rook( rook )())
(defclass black-rook( rook )())

(def rook-rays '(-1 1 -16 16))
(def white-rook (make-instance 'white-rook :value  500 :glyph "R" :dirs rook-rays))
(def black-rook (make-instance 'black-rook :value -500 :glyph "r" :dirs rook-rays))

;(defun p-count( pawn col )
 ; (car (aref (cols pawn) col)))

;(defun file-open?( pawn col ) 
;  (zerop (car (aref (cols pawn) col))))

(defmethod evaluate + ( (piece white-rook) square )
  (+ (if-0 (< square a6) 40)
     (let ((col (get-col square)))
       (if-0 (file-open? white-pawn col)
	   (+ 35 (if-0 (file-open? black-pawn col) 30))))))

(defmethod evaluate + ( (piece black-rook) square )
  (+ (if-0 (< square a3) 40)
     (let ((col (get-col square)))
       (if-0 (file-open? black-pawn col)
	   (+ 35 (if-0 (file-open? white-pawn col) 30))))))

; bishops.  Some extra code to count bishops to determine two-bishops advantage
(defclass bishop( slider ) ((pcount :accessor pcount :initform 0)))
(defmethod adjust-add progn    ( (piece bishop) square ) (incf (pcount piece))) 
(defmethod adjust-remove progn ( (piece bishop) square ) (decf (pcount piece)))

(def bishop-rays '(-17 -15 15 17))
(def white-bishop (make-instance 'bishop :value  350 :glyph "B" :dirs bishop-rays))
(def black-bishop (make-instance 'bishop :value -350 :glyph "b" :dirs bishop-rays))
(defmethod evaluate + ( (piece bishop) square )
  (aref centralize square))


(defclass queen( slider )())
(def queen-rays (concatenate 'list rook-rays bishop-rays))
(def white-queen (make-instance 'queen :value  900 :glyph "Q" :dirs queen-rays))
(def black-queen (make-instance 'queen :value -900 :glyph "q" :dirs queen-rays))
(defmethod evaluate + ( (piece queen) square) ; todo: penalize early sallies
  (+ (aref centralize square) 
     (if-0 (and (< *halfmoves* 15) (not (or (= square d1) (= square d8)))) -55)))

   
; pawns are special and unique

 ; pawns on columns

(def *taxi* (make-array '(128 128)))

(board-loop do
  (loop for s2 in x88-squares do
       (labels ((taxi-axis( func ) (abs (- (funcall func sq)(funcall func s2)))))
         (setf (aref *taxi* sq s2) (max (taxi-axis 'get-col)(taxi-axis 'get-row))))))

(def *pawn-hash* 0)

(defun get-cell( pawn square )
  (aref (cols pawn) (get-col square)))

(defmethod adjust-add progn ( (pawn pawn) square)
  (hashm pawn square *pawn-hash*)
  (incf (pcount pawn))
  (let ((cell (get-cell pawn square)))
    (incf (car cell))
    (push (get-row square) (cdr cell))))

(defmethod adjust-remove progn ( (pawn pawn) square)
  (hashm pawn square *pawn-hash*)
  (decf (pcount pawn))
  (let ((cell (get-cell pawn square)))
    (decf (car cell))
    (setf (cdr cell) (delete (get-row square) (cdr cell)))))

(defun isolated?( pawn square )
  (let ((col (get-col square)))
    (not (or (and (> col 0) (plusp (car (aref (cols pawn) (1- col)))))
	     (and (< col 7) (plusp (car (aref (cols pawn) (1+ col)))))))))

(defun doubled?( pawn square )
  (> (car (aref (cols pawn) (get-col square))) 1))

(defmethod evaluate + ( (pawn pawn) square )
  (+ (ash (aref centralize square) -1)
     (if-0 (doubled? pawn square) -20)
     (if-0 (isolated? pawn square) -30)))

(defun clear?( sq foe direction )
  (loop while (x88-ok? sq) do
    (when (eq (board sq) foe) (return-from clear? nil))
    (incf sq direction)) T)

(defun passed?( square foe direction )
 (loop for sq from (1- square) to (1+ square) always
   (clear? (+ sq direction) foe direction)))

(defun white-passed-at?( square ) (passed? square black-pawn -16))
(defun black-passed-at?( square ) (passed? square white-pawn 16))
 
(defun white-passed-score( square ) (+ 50 (ash 3 (- 7 (get-row square)))))
(defun black-passed-score( square ) (+ 50 (ash 3 (get-row square))))

; todo: near/far from king for attack/endgame

(defmethod evaluate + ( (pawn white-pawn) square )
  (+ (if-0 (white-passed-at? square) (white-passed-score square))
))

(defmethod evaluate + ( (pawn white-pawn) (square (eql e2)))
  (+ -30 (if-0 (not (empty? e3)) -30)))
(defmethod evaluate + ( (pawn white-pawn) (square (eql d2)))
  (+ -30 (if-0 (not (empty? d3)) -30)))


(defmethod evaluate + ( (pawn black-pawn) square )
  (+ (if-0 (black-passed-at? square) (black-passed-score square))
))

(defmethod evaluate + ( (pawn black-pawn) (square (eql e7)))
  (+ -30 (if-0 (not (empty? e6)) -30)))
(defmethod evaluate + ( (pawn black-pawn) (square (eql d7)))
  (+ -30 (if-0 (not (empty? d6)) -30)))

(defclass pawn-move (move) ())

(defclass pawn-double (pawn-move) ())

(defclass ep-capture( pawn-move ) ((lift :reader lift :initarg :lift)))

;(defmethod extra( (move ep-capture) ) "ep")

(defmethod play :after ((move ep-capture))
  (push-piece-at (lift move)))

(defmethod unplay :before ((move ep-capture))
  (pop-piece-at (lift move)))

(defclass pawn-promoter( pawn-move ) ((promotes-to :reader promotes-to :initarg :promotes-to)))

(defmethod extra( (move pawn-promoter) ) (elt (format nil "~d" (promotes-to move)) 1))

(defmethod play :after ( (move pawn-promoter) )
  (push-piece-at (to move))
  (add-piece-at (promotes-to move) (to move)))

(defmethod unplay :before ( (move pawn-promoter) )
  (remove-piece-at (to move))
  (pop-piece-at (to move)))

(defun last-was-double?()
  (eq (type-of (first *played-moves*)) 'pawn-double))

; todo: merge pawn genertors 

; TODO : 

(def white-promoters (list white-queen white-rook white-bishop white-knight))
(def black-promoters (list black-queen black-rook black-bishop black-knight))

(defun add-pawn-move( from to )
  (cond ((< to 16)(add-move 'pawn-promoter from to :promotes-to white-promoters))
	((> to h2)(add-move 'pawn-promoter from to :promotes-to black-promoters))
	(T (add-move 'pawn-move from to))))
      
(defun add-if-pawn-capture( from to )
  (when (and (x88-ok? to) (= (- *to-move*) (signum (value (board to)))))
    (add-pawn-move from to)))

(defmethod generate( (pawn white-pawn) square ) 
  (let ((forward (- square 16))(row (get-row square)))
    (when (eq (board forward) empty-square)
      (add-pawn-move square forward)
      (when (and (= row 6) (eq (board (- forward 16)) empty-square))
	(add-move 'pawn-double square (- forward 16))))
    (add-if-pawn-capture square (- square 17))
    (add-if-pawn-capture square (- square 15))
    (when (= row 3) 
      (let ((last (first *played-moves*)))
	(when (and (eq 'pawn-double (type-of last))
		   (= 1 (abs (- square (to last)))))
	  (add-move 'ep-capture square (+ (from last) 16) :lift (to last)))))))

	  
(defmethod generate( (pawn black-pawn) square ) 
  (let ((forward (+ square 16))(row (get-row square)))
    (when (eq (board forward) empty-square)
      (add-pawn-move square forward)
      (when (and (= row 1) (eq (board (+ forward 16)) empty-square))
	(add-move 'pawn-double square (+ forward 16))))
    (add-if-pawn-capture square (+ square 17))
    (add-if-pawn-capture square (+ square 15))
    (when (= row 4) 
      (let ((last (first *played-moves*)))
	(when (and (eq 'pawn-double (type-of last))
		   (= 1 (abs (- square (to last)))))
	  (add-move 'ep-capture square (+ (from last) -16) :lift (to last)))))))

(defun material-score()
  (let ((mb (- white-material black-material)))
    mb))
    
(defun board-eval()
  (+
   (material-score)
   (board-loop for piece = (board sq) sum
	       (if (eq piece empty-square) 0
		 (+ 
		  (* (evaluate piece sq) (signum (value piece))))))))
  
(defun debug-eval()
  (board-loop for piece = (board sq) do
	      (when (zerop (mod sq 8)) (terpri))
	      (if (eq piece empty-square) (princ "0 ")
		(format t "~d " (* (evaluate piece sq) (signum (value piece)))))
	      
	      ))

(defun ray-ends-with( sq delta pieces )
  "see if a ray ends with one of the pieces supplied in the list."
  (loop with s fixnum = (+ sq delta) while (x88-ok? s) do
       (unless (empty? s) (return (member (board s) pieces :test #'eq)))
       (incf s delta)))

(defun hop-delta-ends-with( sq deltas piece )
  "see if a given piece exists at the end of a list of jumps"
  (loop for delt fixnum in deltas for s fixnum = (+ sq delt) thereis
       (and (x88-ok? s)(eq (board s) piece))))

(let ((black-diag-attack (list black-bishop black-queen))
      (black-vert-attack (list black-rook   black-queen)))
  (defun black-attacks?( sq )
    (or (loop for delta in bishop-rays thereis (ray-ends-with sq delta black-diag-attack))
        (loop for delta in rook-rays thereis (ray-ends-with sq delta black-vert-attack))
        (hop-delta-ends-with sq '(-15 -17) black-pawn)
        (hop-delta-ends-with sq knight-deltas black-knight)
        (hop-delta-ends-with sq king-deltas black-king))))

(let ((white-diag-attack (list white-bishop white-queen))
      (white-vert-attack (list white-rook   white-queen)))
  (defun white-attacks?( sq )
    (or (loop for delta in bishop-rays thereis (ray-ends-with sq delta white-diag-attack))
        (loop for delta in rook-rays thereis (ray-ends-with sq delta white-vert-attack))
        (hop-delta-ends-with sq '(15 17) white-pawn)
        (hop-delta-ends-with sq knight-deltas white-knight)
        (hop-delta-ends-with sq king-deltas white-king))))

(defun side-in-check?( side )
  (if (= side 1) (black-attacks? (at white-king)) (white-attacks? (at black-king))))
(defun am-i-in-check?()(side-in-check? *to-move*))
(defun left-in-check?()(side-in-check? (- *to-move*)))
  
(defclass castle (move) ((rfrom :reader rfrom :initarg :rfrom)
			 (rto   :reader rto   :initarg :rto)) )

(defmethod play :after ( (move castle) ) 
  (slide (rfrom move) (rto move)))

(defmethod unplay :after ( (move castle) )
  (slide (rto move) (rfrom move)))

(defmethod generate :after ( (piece white-king) (square (eql e1)))
  (when (and (zerop (aref traffic e1)) (not (black-attacks? e1)))
    (when (zerop (aref traffic h1)) ; king rook not moved?
      (when (and (empty? f1) (empty? g1))
	(unless (or (black-attacks? f1) (black-attacks? g1))
	  (add-move 'castle e1 g1 :rfrom h1 :rto f1))))
    (when (zerop (aref traffic a1))
	  (when (and (empty? d1) (empty? c1) (empty? b1))
	    (unless (or (black-attacks? d1) (black-attacks? c1))
	      (add-move 'castle e1 c1 :rfrom a1 :rto d1))))
  ))

(defmethod generate :after ( (piece black-king) (square (eql e8)))
  (when (and (zerop (aref traffic e8)) (not (white-attacks? e8)))
    (when (zerop (aref traffic h8)) ; king rook not moved?
      (when (and (empty? f8) (empty? g8))
	(unless (or (white-attacks? f8) (white-attacks? g8))
	  (add-move 'castle e8 g8 :rfrom h8 :rto f8))))
    (when (zerop (aref traffic a8))
	  (when (and (empty? d8) (empty? c8) (empty? b8))
	    (unless (or (white-attacks? d8) (white-attacks? c8))
	      (add-move 'castle e8 c8 :rfrom a8 :rto d8))))
  ))

(defparameter pieces (list empty-square white-king black-king white-queen black-queen white-pawn black-pawn
		     white-rook black-rook white-knight black-knight white-bishop black-bishop))

(defun piece-for( str )
  (find-if (lambda(pc)(string= str (glyph pc))) pieces))

(defun decode-fen( str )
  (board-loop for i to 63 do 
              (remove-piece-at sq)
	      (add-piece-at (piece-for (subseq str i (+ i 1))) sq)))

(decode-fen "rnbqkbnrpppppppp                                PPPPPPPPRNBQKBNR")
(incf (times-current-seen))

(defun long-calc-hash()
  (logxor (if (= *to-move* -1) #xffffff 0) (reduce #'logxor (board-loop collect (aref (hash (board sq)) sq)))))

(defun assertions()
  (labels ((locate (piece)(assert (= (at piece)(position piece *board*)))))
    (assert (= *hash* (long-calc-hash)))
    (locate white-king)
    (locate black-king) 
    (assert (= *halfmoves* (length *played-moves*)))
    ))

(defun movegen ( &optional (for-side *to-move*))
  (catch 'mate
    (setf generated-moves nil)
    (board-loop do (when (= (side (board sq)) for-side)
		     (generate (board sq) sq)))
    generated-moves))

(defun perft(depth)
  (let ((count 0))
    (labels ((inner-perft(d)
			 ;(assertions)
			 (if (= d 0) (incf count)
			   (let ((moves (movegen)))
			     (loop for move in moves do
				   (play move)
				   (unless (left-in-check?)
				     (inner-perft (1- d)))
				   (unplay move))))))
      (inner-perft depth) count)))
	
(defun board-to-string()
  (apply 'concatenate 'string (board-loop collect (glyph (board sq)))))

(def null-move (make-instance 'move 'from -1 'to -1))

(def killer-table (make-array 100 :initial-element null-move))

(defun killer?( move depth ) 
  (and (>= depth 0) (move= move (aref killer-table depth))))

(defun add-killer( move depth ) 
  (and (>= depth 0) (setf (aref killer-table depth) move)))

(def history-table (make-array '(128 128) :initial-element 0))

(defun add-history( move ) 
  (when (> (incf (aref history-table (from move) (to move))) 200)
    ;(format t "reducing history~%")
    (loop for i from 0 to 127 do
	  (loop for j from 0 to 127 do 
		(setf (aref history-table i j) (ash (aref history-table i j) -2))))))

(defun show-history()
  (loop for i from 0 to 127 do
	  (loop for j from 0 to 127 as s = (aref history-table i j) do 
		(when (plusp s)
		  (format t "Move ~d-~d     score: ~d~%" (square-to-string i) (square-to-string j) s)))))

(defclass hash-entry() 
  ((score :accessor score :initform nil)
   (flag  :accessor flag :initform nil)
   (depth :accessor depth :initform 0)
   (move  :accessor move :initform nil)
   (age   :accessor age)))

(def pos-hash (make-hash-table :test 'eq))

(defun probe-hash()
  (let ((he (gethash *hash* pos-hash)))
    (unless he
      (setf he (make-instance 'hash-entry))
      (setf (gethash *hash* pos-hash) he)) 
    (setf (age he) 0) he))

(defun clean-hash() ; todo: dispose of "stale" entries, not whole table
  (clrhash pos-hash))

(def hashing t)

(defun side-eval()
 (* (board-eval) *to-move*))

(def hash-move nil)
(def last-to 0)

(declaim (special depth))

(defun score-move( move )
  (cons move
       (+ (if-0 (eq move hash-move) 900)
	  (if-0 (killer? move depth) 300)
	  (if-0 (eq (to move) last-to) 400)
	  (if-0 (not (eq (board (to move)) empty-square)) 200)
	  (aref history-table (from move) (to move) )) ))

(defun order-moves( moves )
  (let ((scored-moves (mapcar #'score-move moves)))
    (setf scored-moves (sort scored-moves #'(lambda(box-1 box-2)(> (cdr box-1) (cdr box-2)))))
    (mapcar #'car scored-moves)))


(defmethod play( (move (eql null-move)) )
  ;(format t "nullplay~%")
  (flip-to-move)
  (push move *played-moves*))

(defmethod unplay( ( move (eql null-move)))
  (flip-to-move)
  (pop *played-moves*))

(def searched 0)

(defclass draw-move(move)())
(def *draw-move* (make-instance 'draw-move))
(defmethod play( (move draw-move)) )
(defmethod print-object( (move draw-move) stream )
  (format stream "draw~%"))

(def nullmoving t)

(defun dfs( alpha beta depth max-depth )
  (when (= (times-current-seen) 3) (return-from dfs (values 0 (list *draw-move*))))
  (let ((depth-left (- max-depth depth)) (he (probe-hash)))
    (setf hash-move (first (move he)))
    (when (and hashing (flag he) (>= (depth he) depth-left))
      (when (>= (flag he) 0) 
	(setf alpha (max alpha (score he))))
      (when (<= (flag he) 0) 
	(setf beta (min beta (score he))))
      (when (>= alpha beta) 
	(return-from dfs (values (score he) (move he)))))

    (incf searched)

    (if (and (>= depth max-depth) (eq (first *captures*) empty-square))
	(side-eval)
      (progn
        ; null move. If we can simply pass on the move, and the opponent can't immediately
        ; kill us, then our position is overwhelming, and we can just return beta.
	(when (and (= 1 0) nullmoving (>= depth-left 2)
		   (not (eq (first *played-moves*) null-move))
					(not (side-in-check? *to-move*))
		   )
	  (play null-move)
	  (let ((null-score (- (dfs (- beta) (1+ (- beta)) (+ depth 2) max-depth ))))
	    (unplay null-move)
	    (when (>= null-score beta) ; passing on a move was still winning(!)
	      ;(princ "!")
	      (return-from dfs beta))))
	
	(let ((moves (movegen))
	      (best-line nil)
              (bonus (if (am-i-in-check?) 1/2 0))
	      (score most-negative-fixnum)
	      (quiescing? (>= depth max-depth)))
	  (when (eq moves 'mate)
	    (return-from dfs 'illegal))
	  (when quiescing?
	    (let ((last-to (to (first *played-moves*))))
	      (setf moves (remove-if-not (lambda(mv)(= (to mv) last-to)) moves))
	      (setf score (max alpha (side-eval)))))
	  (setf moves (order-moves moves))
	  (loop for move in moves for bonus2 = bonus while (< score beta) do
		(play move)
                (unless (eq (first *captures*) empty-square) (incf bonus2 1/2))
		(multiple-value-bind (sc line) (dfs (- beta) (- (max alpha score)) (1+ depth) (+ max-depth bonus2))
		  (unplay move)
		  (unless (eq sc 'illegal) 
		    (setf sc (- sc))
		    (when (> sc score) (setf score sc)(setf best-line (cons move line))
			  (when (> score alpha) (add-history move)))
		    (when (>= score beta) (add-killer move depth)))))
	  (when (and (not quiescing?) (= score most-negative-fixnum))
	    (setf score (if (am-i-in-check?) (- -30000 depth-left) 0))
            )
	  (when (and hashing (>= depth (depth he)))
	    (setf (depth he) depth-left
		  (move he) best-line
		  (score he) score
		  (flag he) (- (if-0 (> score alpha) 1) (if-0 (< score beta 1) 1)))
	    )
	(values score best-line))))))

(defun takeback( &optional (times 1) )(loop repeat times do (unplay (pop *played-moves*))))

(defun string-to-move( str )
  (setf str (remove #\= str))
  (movegen)
  (let ((m (remove-if-not (lambda(m)(string-equal str (format nil "~d" m))) generated-moves)))
	(first m)))
	
(def my-time 300)
(def his-time 300)
    
(defun get-machine-move( &optional (depth nil))
  (clean-hash)
  (format t "hashing is x ~d~%" (if hashing "on" "off"))
  (setf searched 0)
  (let ((d 0)(score (side-eval))(line nil)(finding t)(start-time (get-internal-real-time)))
    (time (loop while (or (< d 4) (and (> my-time 40) (< (- (get-internal-real-time) start-time) 500))) do
	  (setf finding t)(incf d)
	  (loop with alpha = (- score 50000)
		with beta  = (+ score 50000)
		as window = (abs (- beta alpha))
		with s = 0 with l = nil 
		while finding do
		(format t "searching depth ~d with window ~d ~d~%" d alpha beta)
		(multiple-value-setq (s l) (dfs alpha beta 0 d))
		(cond ((<= s alpha) (format t "fail low~%")(decf alpha window))
		      ((>= s beta)  (format t "fail high~%")(incf beta  window))
		      (T (format t "Found score: ~d (~d modes searched) line ~d~%" s searched l) (setf score s line l finding nil))))))
    (values (first line) d)))

(defun machine-move()
  (let ((from-lib (execute-to-list (db) "select move from library where depth > 2 and position=? order by depth desc,random()" (board-to-string))))
    (when (and from-lib (< (times-current-seen) 2))
      (setf from-lib (string-to-move (first (first from-lib))))
      (play from-lib)
      (return-from machine-move from-lib)))
  (setf hashing t)
  (multiple-value-bind (mm depth) (get-machine-move)
    (when (< *halfmoves* 40)
      (execute-non-query (db) "insert into library(halfmoves,depth,position,move) values(?,?,?,?)" *halfmoves* depth (board-to-string) (format nil "~d" mm)))
    (play mm)
    mm))

; UI code

(defun strtok( str sep )
  "tokenize a string.  This ought to be built in, eh?"
  (let ((p (position sep str)))
    (if p (cons (subseq str 0 p) (strtok (subseq str (1+ p)) sep)) (list str))))

(defgeneric command-f( cmd args ))

(defmethod command-f( cmd args ) 
  (format t "unhandled command : ~d -- ~d~%" cmd args )
  (play-xboard-user-move (format nil "~d" cmd)))

(defmacro command( key body )
  `(defmethod command-f( (k (eql ,key)) args ) ,body))

(command 'go (format t "move ~d~%" (machine-move)))

(defun play-xboard-user-move( str )
  (sb-sys:enable-interrupt sb-unix:sigint (lambda(&rest r)(format t "sigint got ~a%"r)))
  (let ((move (string-to-move str)))
    (if move 
	(progn 
	  (play move)
	  (command-f 'go nil)
	  )
	(format t "Unknown move: ~a~%" str))))

(command 'o (if (= *to-move* -1)
      (play-xboard-user-move "e8g8")
      (play-xboard-user-move "e1g1")))

(command 'o- (if (= *to-move* -1)
      (play-xboard-user-move "e8c8")
      (play-xboard-user-move "e1c1")))

(command 'time (setf my-time (/ (read-from-string (first args) ) 100)))

(command 'otim (setf his-time (/ (read-from-string (first args) ) 100)))

(defun handle-input( str )
  (SB-INT:FLUSH-STANDARD-OUTPUT-STREAMS)
  (let ((tokens (strtok str #\Space)))
    (format t "tokens: ~d~%" tokens)
    (command-f (read-from-string (first tokens)) (rest tokens)))
  (SB-INT:FLUSH-STANDARD-OUTPUT-STREAMS))

(defun xboard-loop()
  (loop for line = (read-line) do 
	(print-board) 
	(SB-INT:FLUSH-STANDARD-OUTPUT-STREAMS)
	(terpri)
	(handle-input line) 
	(force-output)
	(SB-INT:FLUSH-STANDARD-OUTPUT-STREAMS)
	)
  )

(defun build-opening-book()
  (with-open-file (book "book.txt")
		  (loop for line = (read-line book nil nil) while line do
			(loop for str in (strtok line #\Space)
			      for move = (string-to-move str) do
			      (execute-non-query (db) "insert into library(halfmoves,depth,position,move) values(?,?,?,?)" *halfmoves* 1000 (board-to-string) (format nil "~d" move))
			      (play move) 
			      collect move into pline
			      finally (mapcar 'unplay (reverse pline))))))
	
(defun expand-opening-book()
  (clean-hash)
  (let ((row (execute-to-list (db) "select row,position,depth,move,halfmoves from library where depth<1000 order by depth,halfmoves limit 1")))
    (setf row (car row))
    (let ((row-key (first row))
	  (q nil)
	  (position (second row))
	  (depth (third row))
	  (move (fourth row))
	  (halfmoves (fifth row)))
      (setf *to-move* (if (evenp halfmoves) 1 -1))
      (decode-fen position)
      (incf depth)
      (print-board)
      (format t "old move is ~d~%" move)
      (multiple-value-bind (score moves) (dfs -100000 100000 0 depth)
	(declare (ignore score))
        (format t "new line is ~d~%" moves)
        (setf q (format nil "update library set move='~d' , depth=~d where row=~d" (format nil "~d" (first moves)) depth row-key)) 
        (print q)
	(execute-non-query(db) q)))))
	
(defun thinker()
        (setf nullmoving nil)
	(expand-opening-book))
		
(require 'sb-posix)

(defun make-exe( &optional (binary-name "slcp.exe") (function 'xboard-loop))
  (setf *db* nil)
  (when (zerop (sb-posix:fork))
    (sb-ext:save-lisp-and-die binary-name
			      :executable t 
			      :purify t :toplevel function)))
(defun make-thinker()
  (make-exe "thinker.exe" 'thinker))
