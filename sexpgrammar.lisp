(defparameter *punct* 'p)
(defparameter *empty* 'empty)
(defparameter *end*   '$)
(defparameter *terminals* (list '\( '\) 'id '+ '*))
(defparameter *nonterminals* (list 'E1 'E 'T 'F))
(defparameter *grammar*
  (list 
   (list 'E1 'E)               ;; E' -> E
   (list 'E (list 'E '+ 'T))   ;; E -> E+T
   (list 'E 'T)                ;; E -> T
   (list 'T (list 'T '* 'F))   ;; T -> T*F
   (list 'T 'F)                ;; T -> F
   (list 'F (list '\( 'E '\)))   ;; F -> (E)
   (list 'F 'id)))             ;; F -> id

(defparameter *I0* (list (list 'E1 
                         (list *punct* 'E))))


(defun push-back (x l)
  "Non-modifying function for appending element x to the end of the list l"
  (append l (list x)))


(defun list-diff (L1 L2)
  "Create a list containing difference between lists L1 and L2"
  (let ((result nil))
    (dolist (X L1)
      (when (not (member X L2 :test 'equal))
        (setf result (push-back X result))))
    (dolist (X L2)
      (when (not (member X L1 :test 'equal))
        (setf result (push-back X result))))
    result))


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
          (eq production *empty*)))))

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


(defun add-items-to-closure (closure-I rules)
  "Add rules to the closure(I) if they are not already there"
  (dolist (X rules)
    (when (not
           (member X closure-I :test 'equal))
      (setf closure-I (push-back X closure-I))))
  closure-I)
    
(defun closure (I)
  "Construct closure array for the given set of rules with punct"
  (when I
  (let ((s (length I)))
    (dolist (rule I)
      (let ((rules (closure-rules-for-closure-rule rule)))
        (setf I (add-items-to-closure I rules))))
    (if (= (length I) s)
        I
        (closure I)))))

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


(defun goto (I X)
  "goto function for the LR(0) grammar
from book \"Compilers: Principles, Techniques, and Tools\" by  Aho, Sethi, Ullman"
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
        (let ((rule (move-punct-to-right R)))
          (when rule
            (setf result-list
                  (append result-list (closure (list (move-punct-to-right R)))))))))
      result-list))

;; auxulary function - remove later if not needed
(defun find-grammar-symbols-after-dot (I)
  (let ((result nil))
    (dolist (R I)
      (let* ((right-production (second R))
             (punct-position (member *punct* right-production :test 'equal)))
        (when ( > (length punct-position) 1)
          (let ((grammar-symbol (second punct-position)))
           (when (not (find grammar-symbol result))
              (setf result (push-back grammar-symbol result)))))))
        result))
        
(defun items (I0)
  "items function for the LR(0) grammar
from book \"Compilers: Principles, Techniques, and Tools\" by  Aho, Sethi, Ullman"
  (let* ((C (closure I0)) ;; first closure 
         (items (list C)) ;; first items = { I0 }
         (symbols (append *nonterminals* *terminals*))
         (updated t))
    (loop while updated
       do
         (setf updated nil)         
         (dolist (I items)
           (dolist (X symbols)
             (let ((goto-items (goto I X)))
               (when (and goto-items
                          (notevery (lambda (x)
                                      (member x C :test 'equal))
                                    goto-items))
                 (setf C (add-items-to-closure C goto-items))
                 (setf items (push-back goto-items items))
                 (setf updated t))))))
    items))

(defun print-items (items-list)
  (dolist (I items-list)
    (format t "---------------~%")
    (dolist (X I)
      (pretty-print-rule X))))


(defun first-function (word first-table)
  (let ((first-list nil))                 ; result
    (labels ((add-to-results-atom (item)
               (when (not (member item first-list))
                 (setf first-list
                       (append first-list (list item)))))
             (add-to-results (item)
               (if (atom item)
                   (add-to-results-atom item)
                   (dolist (r item)
                     (add-to-results-atom r)))))
      (if (atom word)
          (gethash word first-table)
        ;; when X-is a word, like X1 X2 X3
        (progn 
          (dolist (Y word)
            (let ((first-list-Y (gethash Y first-table)))
              (dolist (Z first-list-Y)
                (unless (eq Z *empty*)
                  (add-to-results Z)))
              (unless (member *empty* first-list-Y)
                (return))))
          (when (not first-list)
            (add-to-results *empty*))
          first-list)))))

                        

(defun create-first-tables (terminals nonterminals grammar)
  (let ((first-table (make-hash-table))) ; define result as a hash table
    ;; helper functions
    (labels ((epsilon-production-p (rule)        ; is rule the epsilon-production?
               (eq (second rule) *empty*)) 
             (find-productions (Y)      ; all productions from the given
                                        ; nonterminal Y
               (remove-if-not (lambda (rule)
                                (eq (first rule) Y))
                              grammar))
             (add-to-results-atom (alpha item) ; add single result to the table
               (when (and item (not (member item (gethash alpha first-table))))
                 (setf (gethash alpha first-table)
                       (append (gethash alpha first-table) (list item)))))
             (add-to-results (alpha item) ; add result(list or atom) to the table
               (if (atom item)
                   (add-to-results-atom alpha item)
                   (dolist (r item)
                     (add-to-results-atom alpha r))))
             (first-table-size ()
               (loop for key being the hash-keys of first-table 
                  using (hash-value value) 
                  sum (length value))))
      ;; 1) add all terminals to the table
      (dolist (X terminals)
        (add-to-results-atom X X))
      ;; 2) add all empty productions to appropriate nonterminals
      (dolist (R grammar)
        (when (epsilon-production-p R)
          (add-to-results-atom (first R) *empty*)))
      ;; loop until table size is not changing
      (let ((current-table-size 0))
        (loop while (/= current-table-size
                        (setf current-table-size (first-table-size)))
             do
      ;; 3) loop by all nonterminals
      (dolist (X nonterminals)
        (let ((productions            ; list of non-epsilon productions from X
               (remove-if #'epsilon-production-p (find-productions X))))
          ;; loop by elements of every production        
          (dolist (R productions)
            (let ((rule (second R)))
              (if (atom rule)
                  ;; if atom simply put FIRST(rule)
                  ;; to the result list
                  (when (gethash rule first-table)
                    (add-to-results X (gethash rule first-table)))
                  ;; othewise loop by all elements
                  (progn 
                    (dolist (Y rule)
                      ;; for every element Yi find FIRST(Yi)
                      (if (not (eq Y X))
                          (let ((first-list-Y (gethash Y first-table)))
                            ;; if FIRST(Yi) doesn't contain epsilon, add
                            ;; FIRST(Y) to the FIRST list and stop the loop
                            (when (not (member *empty* first-list-Y))
                              (add-to-results X first-list-Y)
                              (return)))
                          (add-to-results X (gethash Y first-table)))))))))))
        ))
    first-table))
  
(defun test-create-first-tables()
  (let ((*terminals* (list '\( '\) 'id '+ '*))
        (*nonterminals* (list 'E1 'E 'T 'T1 'F))
        (*grammar* 
         (list 
          (list 'E (list 'T 'E1))      ; E -> TE1
          (list 'E1 (list '+ 'T 'E1)) ; E1 -> +TE1
          (list 'E1 *empty*)       ; E1 -> epsilon
          (list 'T (list 'F 'T1))      ; T -> FT1
          (list 'T1 (list '* 'F 'T1)) ; T1 -> *FT1
          (list 'T1 *empty*)       ; T1 -> epsilon
          (list 'F (list '\( 'E '\))) ; F -> (E)
          (list 'F 'id))))           ; F -> id
    (let ((table (create-first-tables *terminals* *nonterminals* *grammar*)))
      (dolist (X *nonterminals*)
        (format t "term = ~a: " X)
        (print
         (gethash X table))
        (format t "~%")))))

(defun test-create-first-tables1()
  (let* ((*terminals* (list 'a 'b 'c))
         (*nonterminals* (list 'S 'E 'L))
         (*grammar*
          (list
           (list 'S (list 'a)) ; S -> a
           (list 'S (list 'L)) ; S -> L
           (list 'E 'S)        ; E -> S
           (list 'E (list 'S 'E)) ; E -> SE
           (list 'L (list 'b 'c)) ;L -> bc
           (list 'L (list 'b 'E 'c)))) ; L -> bEc
         (first-table (create-first-tables *terminals* *nonterminals* *grammar*)))
      (dolist (X
                (append *nonterminals*
                        (list (list 'S 'E) ;; FIRST(SE)  = {a,b}
                              (list 'b 'E 'c) ;; FIRST(bEc) = {b}
                              (list 'b 'c)))) ;; FIRST(bc)  = {b}
        (format t "term = ~a: " X)
        (print
         ;; (gethash X first-table))
         (first-function X first-table))
        (format t "~%"))))


