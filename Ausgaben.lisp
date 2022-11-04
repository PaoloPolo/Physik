(defun function-hyperbole (a p r)
  (* (- a) (expt (/ a r) p)))

(defun print-hyperbole (a p r-start r-end r-step)
  (with-open-file (stream "Ausgabe-Hyperbel.txt" :external-format :utf-8
						 :direction :output
						 :if-exists :supersede)
    (format stream "Hyperbel für a: ~a, p: ~a~%" a p)
    (loop for r from r-start to r-end by r-step 
	  do (format stream "~,3f;~,2@f~%" r (function-hyperbole a p r)))))

(defconstant +epsilon-zero+ 8.854187817d-12)

(defun function-electric (Q r)
  (* (/ (* 4 pi +epsilon-zero+)) (/ Q (expt r 2))))

(defun print-electric (Q r-start r-end r-step)
  (with-open-file (stream "Ausgabe-el-Feldstaerke.txt" :external-format :utf-8
						       :direction :output
						       :if-exists :supersede)
    (format stream "Elektrische Feldstärke für Q: ~a~%" Q)
    (loop for r from r-start to r-end by r-step
	  do (format stream "~,3f;~,2@f~%" r (function-electric Q r)))))

(defconstant +e+ 1.602176565d-19)

(defconstant +k+ 1.3806488d-23)

(defun function-shockley (I_s Temperature n U)
  (* I_s (- (exp (/ (* U +e+) (* n +k+ Temperature))) 1)))

(defun print-shockley (I_s Temperature n U-start U-end U-step)
  (with-open-file (stream "Ausgabe-Shockley.txt" :external-format :utf-8
						 :direction :output
						 :if-exists :supersede)
    (format stream "Shockley-Gleichung für I_s: ~a, Temperatur T: ~a, n: ~a~%" I_s Temperature n)
    (loop for U from U-start to U-end by U-step
	  do (format stream "~,3f;~a~%" U (function-shockley I_s Temperature n U)))))
