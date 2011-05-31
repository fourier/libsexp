(defparameter *punct* 'p)
(defparameter *epslilon* '$)
(defparameter *terminals* (list'a 'b 'id '+ '*))
(defparameter *nonterminals* (list 'E1 'E 'T 'F))
(defparameter *grammar*
  (list 
   (list 'E1 'E)               ;; E' -> E
   (list 'E (list 'E '+ 'T))   ;; E -> E+T
   (list 'E 'T)                ;; E -> T
   (list 'T (list 'T '* 'F))   ;; T -> T*F
   (list 'T 'F)                ;; T -> F
   (list 'F (list 'a 'E 'b))   ;; F -> aEb
   (list 'F 'id)))             ;; F -> id

(defparameter *I0* (list (list 'E1 
                         (list *punct* 'E))))

;; determine the rule to holding *punct* followed by nonterminal
;; returns nonterminal if (*punct*,nonterminal) pair found or nil otherwise
(defun nonterminal-from-rule-for-closure (rule)
  (nonterminal-for-closure (second rule)))

;; helper function for rule-for-closure;
;; do the actual work for right-side of the rule
(defun nonterminal-for-closure (production)
  (when (and (listp production)
             (>= (length production) 2))
    (let ((fst (first production))
          (snd (second production)))
      (or (and (eq fst *punct*)
               (find snd *nonterminals*))
          (nonterminal-for-closure (rest production))))))

;; returns a list of all rules with nonterminal as a left part
(defun find-all-productions (nonterminal)
  (when (find nonterminal *nonterminals*)
    (remove-if-not
     (lambda (x)
       (eq (first x) nonterminal))
     *grammar*)))

;; determine if the rule is not epsilon-rule
(defun production-p (rule)
  (let ((production (second rule)))
    (not
     (and (atom production)
          (eq production *epslilon*)))))

;; construct rule for the closure - add punct at the first position
;; before nonterminal
(defun rule-for-closure (rule)
  (let ((nonterminal (first rule))
        (production (second rule)))
    (if (listp production)
        (cons nonterminal (list (cons *punct* production)))
        (cons nonterminal (list (list *punct* production))))))

;; construct all closure rules for given rule with punct
(defun closure-rules-for-closure-rule (rule)
  (mapcar #'rule-for-closure (find-all-productions (nonterminal-from-rule-for-closure rule))))

;; add rules to the closure(I) if they are not already there
(defun add-closure-rules-to-closure (closure-I rules)
  (dolist (X rules)
    (when (not
           (find X closure-I :test #'equal))
      (setf closure-I (append closure-I (list X)))))
  closure-I)
    
;; construct closure array for the given set of rules with punct
(defun closure (I)
  (let ((s (length I)))
    (dolist (rule I)
      (let ((rules (closure-rules-for-closure-rule rule)))
        (setf I (add-closure-rules-to-closure I rules))))
    (if (= (length I) s)
        I
        (closure I))))

;; Pretty-print the rule
(defun pretty-print-rule (R)
  (let ((rule-nonterminal (first R))
        (rule (second R)))
  ;; print the rule leading nonterminal (left part)
    (format t "~a -> " (symbol-name rule-nonterminal))
    (if (atom rule)
        ;; if atom just print it
        (format t "~a" (symbol-name rule))
        ;; if list walk through the list and print element
        (dolist (X rule)
          ;; in case if it is a punct print '.' instead
          (if (eq X *punct*)
              (format t ".")
              (format t "~a" (symbol-name X)))))
  (format t "~%")))


  
