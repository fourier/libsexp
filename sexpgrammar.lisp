(defparameter *punct* 'p)
(defparameter *epslilon* '$)
(defparameter *terminals* (list '\( '\) 'id '+ '*))
(defparameter *nonterminals* (list 'E1 'E 'T 'F))
(defparameter *grammar*
  (list 
   (list 'E1 'E)               ;; E' -> E
   (list 'E (list 'E '+ 'T))   ;; E -> E+T
   (list 'E 'T)                ;; E -> T
   (list 'T (list 'T '* 'F))   ;; T -> T*F
   (list 'T 'F)                ;; T -> F
   (list 'F (list '\( 'E '\)))   ;; F -> [E]
   (list 'F 'id)))             ;; F -> id

(defparameter *I0* (list (list 'E1 
                         (list *punct* 'E))))

;; while loop macro
(defmacro while (test &rest body) 
  `(do () 
       ((not ,test)) 
     ,@body))


(defun push-back (x l)
  "Non-modifying function for appending element x to the end of the list l"
  (append l (list x)))

(defun pretty-print-rule (R)
  "Pretty-print the rule R"
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


(defun nonterminal-from-rule-for-closure (rule)
  "Determine the rule to holding *punct* followed by nonterminal
Returns nonterminal if (*punct*,nonterminal) pair found or nil otherwise"
  (nonterminal-for-closure (second rule)))

(defun nonterminal-for-closure (production)
  "Helper function for rule-for-closure;
do the actual work for right-side of the rule"
  (when (and (listp production)
             (>= (length production) 2))
    (let ((fst (first production))
          (snd (second production)))
      (or (and (eq fst *punct*)
               (find snd *nonterminals* :test #'equal))
          (nonterminal-for-closure (rest production))))))


(defun find-all-productions (nonterminal)
  "Returns a list of all rules with nonterminal as a left part"
  (when (find nonterminal *nonterminals*)
    (remove-if-not
     (lambda (x)
       (eq (first x) nonterminal))
     *grammar*)))

(defun production-p (rule)
  "Determine if the rule is not epsilon-rule"
  (let ((production (second rule)))
    (not
     (and (atom production)
          (eq production *epslilon*)))))

(defun rule-for-closure (rule)
  "Construct rule for the closure - add punct at the first position
before nonterminal"
  (let ((nonterminal (first rule))
        (production (second rule)))
    (if (listp production)
        (cons nonterminal (list (cons *punct* production)))
        (cons nonterminal (list (list *punct* production))))))

(defun closure-rules-for-closure-rule (rule)
  "Construct all closure rules for given rule with punct"
  (mapcar #'rule-for-closure
          (find-all-productions
           (nonterminal-from-rule-for-closure rule))))


(defun add-closure-rules-to-closure (closure-I rules)
  "Add rules to the closure(I) if they are not already there"
  (dolist (X rules)
    (when (not
           (member X closure-I :test 'equal))
      (setf closure-I (push-back X closure-I))))
  closure-I)
    
(defun closure (I)
  "Construct closure array for the given set of rules with punct"
  (let ((s (length I)))
    (dolist (rule I)
      (let ((rules (closure-rules-for-closure-rule rule)))
        (setf I (add-closure-rules-to-closure I rules))))
    (if (= (length I) s)
        I
        (closure I))))

(defun move-punct-to-right (rule)
  "If the rule contains the punct, move punct to the next position
like A -> .B becomes A -> B.
if rule is like A -> B.
then return nil;;no modifaction 
Also if no punct found returns nil"
  (let* ((left-nonterminal (first rule))
         (right-production (second rule))
         (found nil)
         (result nil)
         (punct-position (member *punct* right-production :test 'equal)))
    (when punct-position
      (if (= 1 (length punct-position))
          nil ;; last symbol is punct
          (if (and punct-position
               (> (length punct-position) 1))
              (progn
                (dolist (x right-production)
                  (if (not (eq x *punct*))
                      (progn
                        (setf result
                              (push-back x result))
                        (when found
                          (setf result (push-back *punct* result))
                          (setf found nil)))
                      (setf found t)))
                (list left-nonterminal result)))))))

        

(defun items(I)
  "Create a closure for the initial items list
then move puncts for every rule in this closure
and produce the closure of all puncts until nothing to add"
  (let* ((C (closure I)))
    (make-closure-rules-for-items (length C) C)))

(defun make-closure-rules-for-items (prev-size C)
  "helper function for adding items to items list
used to iterate through all closures and to stop
iteration when nothing to add"
  (let* ((C1
          (add-closure-rules-to-closure
           C (rules-from-closure-with-moved-punct C)))
         (new-size (length C1)))
    (if (= new-size prev-size)
        C1
        (make-closure-rules-for-items new-size C1))))

(defun rules-from-closure-with-moved-punct (C)
  "Move punct to one position to the right for all rules;
and create closure for all such new rules"
  (dolist (R C)
    (let* ((moved-punct-rule (move-punct-to-right R))
           (new-closure (add-closure-rules-to-closure
                         C
                         (list moved-punct-rule)))
           (closure-moved (closure new-closure)))
      (setf C (add-closure-rules-to-closure C closure-moved))))
  C)


(defun goto (I X)
  (let ((goto-list nil)
        (found nil)
        (result-list nil))
    (dolist (R I)
      (dolist (A (second R))
        (if (eq A *punct*)
          (setf found t)
          (when found
            (setf found nil)
            (when (eq A X)
              (setf goto-list (push-back R goto-list)))))))
    (when goto-list
      (dolist (R goto-list)
        (setf result-list
              (append result-list (closure (list (move-punct-to-right R))))))
      result-list)))

            
(defun create-all-puncts-from-rule (rule)
  "Create all puncts from the rule. i.e.
for rule A -> B.CD will result the list
A -> BC.D
A -> BCD."
  (let ((result nil)
        (current-generated-rule rule))
        (while (setf current-generated-rule
                     (move-punct-to-right current-generated-rule))
          (setf result (push-back current-generated-rule result)))
        result))
    
    
