(defun first-round (l h lh e f mid)
  (let* ((w (* 2 h))
         (r (* 2 (+ l w)))
         (skirt 10)
         (lw (* e 6)))
    (values
     `((m107) ; Fan off
       (m104 :s 203) ; Set hot end temp
       (g28)
       (g92 :x 0 :y 0 :z 0 :e 0) ; Prep nozzle
       (g1 :z 5 :f 1000) ; Lift nozzle
       (m109 :s 203) ; wait for hot end temperature to be reached
       (g21) ; set units to millimeters
       (g90) ; use absolute coordinates
       (m82) ; use absolute distances for extrusion
       (g92 :e 0)
       (g1 :z ,lh)
       (g1 :x ,(- mid h skirt) :y ,(- mid (/ l 2) skirt) :z ,lh :f ,f)
       (g1 :x ,(+ mid h skirt) :e ,(* e (+ w (* 2 skirt))))
       (g1 :y ,(+ mid (/ l 2) skirt) :e ,(* e (+ w (* 4 skirt) l)))
       (g1 :x ,(- mid h skirt) :e ,(* e (+ w (* 6 skirt) l w)))
       (g1 :y ,(- mid (/ l 2) skirt) :e ,(* e (+ w (* 8 skirt) l w l)))
       (g92 :e 0) ; Skirt done
       (g1 :x ,(- mid h lw) :y ,(- mid (/ l 2) lw) :z lh :f ,f)
       (g1 :x ,(+ mid h lw)       :e ,(* e (+ w (* 2 lw))))
       (g1 :y ,(+ mid (/ l 2) lw) :e ,(* e (+ w (* 4 lw) l)))
       (g1 :x ,(- mid h lw)       :e ,(* e (+ w (* 6 lw) l w)))
       (g1 :y ,(- mid (/ l 2) lw) :e ,(* e (+ w (* 8 lw) l w l)))
       (g92 :e 0) ; Round one done
       (g1 :x ,(- mid h) :y ,(- mid (/ l 2)) :z 0 :f ,f)
       (g1 :x ,(+ mid h) :y ,(- mid (/ l 2)) :z ,(* lh (/ w r)) :e ,(* (/ w r) e w))
       (g1 :x ,(+ mid h) :y ,(+ mid (/ l 2)) :z ,(* lh (/ (+ w l) r))
           :e ,(+ (* (/ w r) e w) (* (/ (+ w l) r) e l)))
       (g1 :x ,(- mid h) :y ,(+ mid (/ l 2)) :z ,(* lh (/ (+ w l w) r))
           :e ,(+ (* (/ w r) e w) (* (/ (+ w l) r) e l) (* (/ (+ w l w) r) e w)))
       (g1 :x ,(- mid h) :y ,(- mid (/ l 2)) :z ,lh
           :e ,(+ (* (/ w r) e w) (* (/ (+ w l) r) e l) (* (/ (+ w l w) r) e w) (* e l))))
     (- mid h)
     lh)))

(defun linspace (start stop segs)
  "Return a list of segs+1 elements evently distributed between start and stop.
   Start and stop values included."
  (loop for i from 0 to segs
        collect (+ start (* (- stop start) (/ i segs)))))

(defun x (p)
  "Extract the x value of gcode or other list p"
  (destructuring-bind (&key x &allow-other-keys) (rest p)
    x))

(defun f (p)
  "Extract the x value of gcode or other list p"
  (destructuring-bind (&key f &allow-other-keys) (rest p)
    f))


(defun x-speed0 (xs z l h fl fh segs)
  (loop with l2    = (/ (* (sqrt 2) l) segs)
        with flist = (list (+ fl (* (- fh fl) (/ (+ z h) (* (sqrt 2) l))))
                           (- fh (* (- fh fl) (/ (+ z h) (* (sqrt 2) l)))))
        repeat (ceiling (/ (length xs) 2))
        append (if (evenp (floor (- (first xs) (/ l (sqrt 2)) 0.001) l2))
                   flist
                   (reverse flist))))

(defun y-speed0 (ys z l h fl fh y-segs)
  (loop with above? = (> (+ z h) (/ l (sqrt 2)))
        with zh = (if above?
                      (- (+ z h) (/ l (sqrt 2)))
                      (+ z h))
        for y in ys collect
                    (if (evenp (floor y (/ l y-segs)))
                        (if above?
                            (+ fl (* (- fh fl) (/ zh (/ l (sqrt 2)))))
                            (+ fl (* (- fh fl) (/ zh (/ l (sqrt 2))))))
                        (if above?
                            (- fh (* (- fh fl) (/ zh (/ l (sqrt 2)))))
                            (- fh (* (- fh fl) (/ zh (/ l (sqrt 2)))))))))

(defun angle-move (x z l h lh e fl fh mid x-segs y-segs)
  "Moves printer along x axis. Doesn't change printer's z position."
  (let ((op0 (if (< (- x mid) 0) ;; A few changes of sign only difference between
                 #'+             ;; left->right moves and right->left moves
                 #'-))
        (op1 (if (< (- x mid) 0)
                 #'-
                 #'+))
        (op2 (if (< (+ z h) (/ l (sqrt 2)))
                 #'+
                 #'-)))
    ;; TODO funcall op0 needs to shift here at
    (let ((lin-x (nreverse (cons (+ mid mid (- x) (funcall op2 (/ (funcall op0 lh) 2)))
                                 (remove-if #'(lambda (p) (> (abs (- p mid)) (abs (- x mid))))
                                            (linspace (funcall op0 mid (/ l (sqrt 2)))
                                                      (funcall op1 mid (/ l (sqrt 2)))
                                                      x-segs)))))
          (lin-y (rest (linspace (funcall op1 mid (/ l 2))
                                 (funcall op0 mid (/ l 2))
                                 y-segs)))
          (lin-z (rest (linspace z (+ z (/ lh 2)) y-segs))))
      (values
       (append
        (cons '(g92 :e 0) ; The x-moves
              (mapcar #'(lambda (x e f) (list 'g1 :x x :e e :f f))
                      lin-x
                      (mapcar #'(lambda (new-x) (* e
                                                   (abs (- (first (last lin-x)) x))
                                                   (/ (- new-x x)
                                                      (- (first (last lin-x)) x))))
                              lin-x)
                      (x-speed0 lin-x z l h fl fh x-segs)))
        (cons '(g92 :e 0) ; The y-moves
              (mapcar #'(lambda (y z e f) (list 'g1 :y y :z z :e e :f f))
                      lin-y
                      lin-z
                      (rest (linspace 0 (* e l) y-segs))
                      (y-speed0 lin-y z l h fl fh y-segs))))
       (first (last lin-x))
       (first (last lin-z))))))

(defun spir (x z l h lh e fl fh &optional (mid 100) (segs 8) (y-segs 5))
  "Make gcode for a full spiral.
   Also return the last x-position and z-position of the spiral."
  (multiple-value-bind (gcode1 x1 z1)
      (angle-move x z l h lh e fl fh mid segs y-segs)
    (multiple-value-bind (gcode2 x2 z2)
        (angle-move x1 z1 l h lh e fl fh mid segs y-segs)
      (values
       (append gcode1 gcode2)
       x2
       z2))))

(defun last-gcodes ()
  '((m107) ; turn fan off
    (m104 :s 0) ; turn hot end off
    (g28 :x 0))) ; home x axis

(defun full-gcode ()
  (let ((l 100)
        (h 10)
        (lh 0.35)
        (e 0.14)
        (fl 1200)
        (fh 3300)
        (mid 100)
        (segs 8)
        (y-segs 5))
    (multiple-value-bind (first-gcode first-x first-z)
        (first-round l h lh e fl mid)
      (append first-gcode
              '((m106 :s 250)) ; Get cooling fan going
              (loop with (gcode x z) = (multiple-value-list
                                        (spir first-x first-z l h lh e fl fh mid segs y-segs))
                    append gcode
                    do (setf (values gcode x z) (spir x z l h lh e fl fh mid segs y-segs))
                    while (< z (- (* l (sqrt 2)) h)))
              (last-gcodes)))))

(defun print-gcodes (gcodes &optional (stream t))
  (loop for gcode in gcodes
        do (format stream "~a " (first gcode))
           (loop for param in (rest gcode)
                 do (if (numberp param)
                        (format stream "~,3f " param)
                        (format stream "~a" param)))
           (format stream "~%")))

(with-open-file (s "~/Desktop/logo.gcode" :if-does-not-exist :create
                                          :if-exists :supersede
                                          :direction :output)
  (print-gcodes (full-gcode) s))


