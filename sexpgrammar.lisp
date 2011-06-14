(defconstant +punct+ 'p)
(defconstant +empty+ 'empty)
(defconstant +end+   '$)

;; CLtL2 states (in section 25.1.3) that the 
;; value-form in a `defconstant' form must be evaluable at compile-time, 
;; and that you must ensure this yourself. 
(eval-when (:compile-toplevel :load-toplevel :execute) 
(defstruct grammar
  (terminals nil :type list)
  (nonterminals nil :type list)
  (rules nil :type list)))

;; 0-grammar
(defconstant +grammar-0+ (make-grammar
                          :terminals (list '\( '\) 'id '+ '*)
                          :nonterminals (list 'E1 'E 'T 'F)
                          :rules 
                          (list 
                           (list 'E1 'E)               ; E' -> E
                           (list 'E (list 'E '+ 'T))   ; E -> E+T
                           (list 'E 'T)                ; E -> T
                           (list 'T (list 'T '* 'F))   ; T -> T*F
                           (list 'T 'F)                ; T -> F
                           (list 'F (list '\( 'E '\))) ; F -> (E)
                           (list 'F 'id))))            ; F -> id

(defconstant +I0+ (list (list 'E1 
                              (list +punct+ 'E))))

;; 1-grammar
(defconstant +grammar-1+ (make-grammar
                          :terminals (list '\( '\) 'id '+ '*)
                          :nonterminals (list 'E1 'E 'T 'T1 'F)
                          :rules
                          (list 
                           (list 'E (list 'T 'E1))     ; E -> TE1
                           (list 'E1 (list '+ 'T 'E1)) ; E1 -> +TE1
                           (list 'E1 +empty+)          ; E1 -> epsilon
                           (list 'T (list 'F 'T1))     ; T -> FT1
                           (list 'T1 (list '* 'F 'T1)) ; T1 -> *FT1
                           (list 'T1 +empty+)          ; T1 -> epsilon
                           (list 'F (list '\( 'E '\))) ; F -> (E)
                           (list 'F 'id))))            ; F -> id

;; 2-grammar

(defconstant +grammar-2+ (make-grammar
                          :terminals (list 'a '\( '\))
                          :nonterminals (list 'S 'E 'L)
                          :rules
                          (list
                           (list 'S (list 'a))            ; S -> a
                           (list 'S (list 'L))            ; S -> L
                           (list 'E 'S)                   ; E -> S
                           (list 'E (list 'S 'E))         ; E -> SE
                           (list 'L (list '\( '\)))       ;L -> ()
                           (list 'L (list '\( 'E '\)))))) ; L -> (E)



(defun push-back (x l)
  "Non-modifying function for appending element x to the end of the list l"
  (append l (list x)))


(defun append-unique (item lst)
  "Appends the list lst with the element item only if no such element in list.
If item is a list do the same for all its elements. Returns the new list"
  (flet ((append-unique-atom (X the-list)
           (if (and X (not (member X the-list)))
               (append the-list (list X))
               the-list)))
    (if (atom item)
        (append-unique-atom item lst)
        (let ((result lst))
          (dolist (r item)
            (setf result (append-unique-atom r result)))
          result))))



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
          (if (eq X +punct+)
              (format t ".")
              (format t "~a" (symbol-name X)))))
    (format t "~%")))


(defun print-items (items-list)
  (dolist (I items-list)
    (format t "---------------~%")
    (dolist (X I)
      (pretty-print-rule X))))


(defun nonterminal-from-rule-for-closure (grammar rule)
  "Determine the rule to holding +punct+ followed by nonterminal
Returns nonterminal if (+punct+,nonterminal) pair found or nil otherwise"
  (nonterminal-for-closure grammar (second rule)))

(defun nonterminal-for-closure (grammar production)
  "Helper function for rule-for-closure;
do the actual work for right-side of the rule"
  (when (and (listp production)
             (>= (length production) 2))
    (let ((fst (first production))
          (snd (second production)))
      (or (and (eq fst +punct+)
               (find snd (grammar-nonterminals grammar) :test #'equal))
          (nonterminal-for-closure grammar (rest production))))))


(defun find-all-productions (grammar nonterminal)
  "Returns a list of all rules with nonterminal as a left part"
  (when (find nonterminal (grammar-nonterminals grammar))
    (remove-if-not
     (lambda (x)
       (eq (first x) nonterminal))
     (grammar-rules grammar))))

(defun production-p (rule)
  "Determine if the rule is not epsilon-rule"
  (let ((production (second rule)))
    (not
     (and (atom production)
          (eq production +empty+)))))

(defun rule-for-closure (rule)
  "Construct rule for the closure - add punct at the first position
before nonterminal"
  (let ((nonterminal (first rule))
        (production (second rule)))
    (if (listp production)
        (cons nonterminal (list (cons +punct+ production)))
        (cons nonterminal (list (list +punct+ production))))))

(defun closure-rules-for-closure-rule (grammar rule)
  "Construct all closure rules for given rule with punct"
  (mapcar #'rule-for-closure
          (find-all-productions
           grammar
           (nonterminal-from-rule-for-closure grammar rule))))


(defun add-items-to-closure (closure-I rules)
  "Add rules to the closure(I) if they are not already there"
  (dolist (X rules)
    (when (not
           (member X closure-I :test 'equal))
      (setf closure-I (push-back X closure-I))))
  closure-I)

(defun closure (grammar I)
  "Construct closure array for the given set of rules with punct"
  (when I
    (let ((s (length I)))
      (dolist (rule I)
        (let ((rules (closure-rules-for-closure-rule grammar rule)))
          (setf I (add-items-to-closure I rules))))
      (if (= (length I) s)
          I
          (closure grammar I)))))

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
         (punct-position (member +punct+ right-production :test 'equal)))
    (when punct-position
      (if (= 1 (length punct-position))
          nil ;; last symbol is punct
          (if (and punct-position
                   (> (length punct-position) 1))
              (progn
                (dolist (x right-production)
                  (if (not (eq x +punct+))
                      (progn
                        (setf result
                              (push-back x result))
                        (when found
                          (setf result (push-back +punct+ result))
                          (setf found nil)))
                      (setf found t)))
                (list left-nonterminal result)))))))


(defun goto (grammar I X)
  "goto function for the LR(0) grammar
from book \"Compilers: Principles, Techniques, and Tools\" by  Aho, Sethi, Ullman"
  (let ((goto-list nil)
        (found nil)
        (result-list nil))
    (dolist (R I)
      (dolist (A (second R))
        (if (eq A +punct+)
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
                  (append result-list (closure grammar (list (move-punct-to-right R)))))))))
    result-list))

;; auxulary function - remove later if not needed
(defun find-grammar-symbols-after-dot (I)
  (let ((result nil))
    (dolist (R I)
      (let* ((right-production (second R))
             (punct-position (member +punct+ right-production :test 'equal)))
        (when ( > (length punct-position) 1)
          (let ((grammar-symbol (second punct-position)))
            (when (not (find grammar-symbol result))
              (setf result (push-back grammar-symbol result)))))))
    result))

(defun items (grammar I0)
  "items function for the LR(0) grammar
from book \"Compilers: Principles, Techniques, and Tools\" by  Aho, Sethi, Ullman"
  (let* ((C (closure grammar I0)) ;; first closure 
         (items (list C)) ;; first items = { I0 }
         (symbols (append (grammar-nonterminals grammar)
                          (grammar-terminals grammar)))
         (updated t))
    (loop while updated
       do
         (setf updated nil)         
         (dolist (I items)
           (dolist (X symbols)
             (let ((goto-items (goto grammar I X)))
               (when (and goto-items
                          (notevery (lambda (x)
                                      (member x C :test 'equal))
                                    goto-items))
                 (setf C (add-items-to-closure C goto-items))
                 (setf items (push-back goto-items items))
                 (setf updated t))))))
    items))


(defun first-function (word first-table)
  "FIRST(X) function for LL and LR(0) parsers
from book \"Compilers: Principles, Techniques, and Tools\" by  Aho, Sethi, Ullman.
Uses the first-table which shall be previously generated by
create-first-table function.
This function accepts either atoms or lists(as a word in grammar)"
  (let ((first-list nil))                 ; result
    (labels
        ((add-to-results (item) ; add result(list or atom) to the table
           (setf first-list
                 (append-unique item first-list)))
         (every-contains-empty (word)
           (if (not word) t
               (and 
                (member +empty+ (gethash (car word) first-table))
                (every-contains-empty (cdr word))))))
      (if (atom word)
          (if (eq word +empty+)
              (list +empty+)
              (gethash word first-table))
          ;; when X-is a word, like X1 X2 X3
          (progn 
            (dolist (Y word)
              (let ((first-list-Y (gethash Y first-table)))
                (dolist (Z first-list-Y)
                  (unless (eq Z +empty+)
                    (add-to-results Z)))
                (unless (member +empty+ first-list-Y)
                  (return))))
            (when (every-contains-empty word)
              (add-to-results +empty+))
            first-list)))))


(defun create-first-table (grammar)
  "FIRST(X) function for LL and LR(0) parsers
from book \"Compilers: Principles, Techniques, and Tools\" by  Aho, Sethi, Ullman.
Creates the hash-table with keys - terminals or nonterminals. Shall be used
with conjunction with first-function"
  (let ((first-table (make-hash-table)) ; define result as a hash table
        (terminals (grammar-terminals grammar))
        (nonterminals (grammar-nonterminals grammar))
        (rules (grammar-rules grammar)))
    ;; helper functions
    (labels ((epsilon-production-p (rule)        ; is rule the epsilon-production?
               (eq (second rule) +empty+)) 
             (find-productions (Y)      ; all productions from the given
                                        ; nonterminal Y
               (remove-if-not (lambda (rule)
                                (eq (first rule) Y))
                              rules))
             (add-to-results (alpha item) ; add result(list or atom) to the table
               (setf (gethash alpha first-table)
                     (append-unique item (gethash alpha first-table))))
             (first-table-size ()
               (loop for key being the hash-keys of first-table 
                  using (hash-value value) 
                  sum (length value))))
      ;; 1) add all terminals to the table
      (dolist (X terminals)
        (add-to-results X X))
      ;; 2) add all empty productions to appropriate nonterminals
      (dolist (R rules)
        (when (epsilon-production-p R)
          (add-to-results (first R) +empty+)))
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
                                   (when (not (member +empty+ first-list-Y))
                                     (add-to-results X first-list-Y)
                                     (return)))
                                 (add-to-results X (gethash Y first-table)))))))))))))
    first-table))


(defun create-follow-table (grammar)
  (let ((follow-table (make-hash-table)) ; define result as a hash table
        (first-table (create-first-table grammar))
        (nonterminals (grammar-nonterminals grammar))
        (rules (grammar-rules grammar)))
    ;; helper functions
    (labels ((add-to-results (alpha item) ; add result(list or atom) to the table
                                        ; except +empty+
               (let ((items (remove-if (lambda (x) (eq x +empty+))
                                       (gethash alpha follow-table))))
                 (setf (gethash alpha follow-table)
                       (append-unique item items))))
             (follow-table-size ()
               (loop for key being the hash-keys of follow-table 
                  using (hash-value value) 
                  sum (length value))))
      ;; 1) put the $ to the follow table for the start symbol
      (add-to-results (first (first rules)) +end+)
      ;; loop until table size is not changing
      (let ((current-table-size 0))
        (loop while (/= current-table-size
                        (setf current-table-size (follow-table-size)))
           do
           ;; 2) loop by nonterminals
             (dolist (X nonterminals)
               ;; 2.1) loop by rules to find rules like A->wXv
               (dolist (R rules)
                 (let ((rule (second R)))
                   (when (listp rule)
                     (let ((tail (member X rule)))
                       (when (and tail (> (length tail) 1))
                         ;; (print tail)
                         (add-to-results X (first-function (cdr tail) first-table)))))))
               ;; 2.2) loop by rules again to find rules like A->wB or A->wBv
               (dolist (R rules)
                 (let ((rule (second R)))
                   (if (atom rule)
                       (when (eq rule X)
                         (add-to-results X (gethash (first R) follow-table)))
                       (let ((tail (member X rule)))
                         (when (and tail
                                    (or (= (length tail) 1)
                                        (find +empty+
                                              (first-function (cdr tail) first-table))))
                           (add-to-results X (gethash (first R) follow-table)))))))))))
    follow-table))


(defun create-ll0-table (grammar)
  (let ((first-table (create-first-table grammar))
        (follow-table (create-follow-table grammar))
        (ll0-table (make-hash-table)))
    (flet ((add (term rule)
             ;; already contains rule
             (if (gethash term (gethash (first rule) ll0-table)) 
                 (setf (gethash term (gethash (first rule) ll0-table))
                       (append (gethash term (gethash (first rule) ll0-table))
                               (list rule)))
                 ;; no rule, add a new one
                 (setf (gethash term (gethash (first rule) ll0-table))
                       (list rule)))))
      ;; initialize results table
      (dolist (X (append (grammar-nonterminals grammar) (list +end+)))
        (setf (gethash X ll0-table) (make-hash-table)))
      (dolist (rule (grammar-rules grammar))
        (let ((first-for-production (first-function (second rule) first-table)))
          (dolist (X first-for-production)
            (when (member X (grammar-terminals grammar))
              (add X rule)))
          ;; check for epsilon production
          (when (member +empty+ first-for-production)
            (let ((follow-for-production (gethash (first rule) follow-table)))
              (dolist (Y follow-for-production)
                (when (member Y (grammar-terminals grammar))
                  (add Y rule)))
              (when (member +end+ follow-for-production)
                (add +end+ rule)))))))
    ll0-table))



;;
;; Tests
;;

(defun test-create-first-table0()
  (let ((first-table (create-first-table +grammar-0+)))
    (dolist (X (grammar-nonterminals +grammar-0+))
      (format t "term = ~a: " X)
      (print
       (first-function X first-table))
      (format t "~%"))))


(defun test-create-first-table1()
  (let ((first-table (create-first-table +grammar-1+)))
    (dolist (X (grammar-nonterminals +grammar-1+))
      (format t "term = ~a: " X)
      (print
       (first-function X first-table))
      (format t "~%"))))

(defun test-create-first-table2()
  (let ((first-table (create-first-table +grammar-2+)))
    (dolist (X
              (append (grammar-nonterminals +grammar-2+)
                      (list 
                       (list 'S 'E) ;;FIRST(SE)  = {a,b}
                       (list 'b 'E 'c);;  FIRST(bEc) = {b}
                       (list 'b 'c))));;  FIRST(bc)  = {b}
      (format t "term = ~a: " X)
      (print
       (first-function X first-table))
      (format t "~%"))))


(defun test-ll0-table-grammar0()
  (let ((ll-table (create-ll0-table +grammar-0+))
        (terminals+end (append (grammar-terminals +grammar-0+) (list +end+)))
        (nonterminals (grammar-nonterminals +grammar-0+)))
    (dolist (X nonterminals)
      (dolist (Y terminals+end)
        (format t "M[~a,~a] = ~a~%" X Y (gethash Y (gethash X ll-table))))
      (format t "~%"))))

(defun test-ll0-table-grammar1()
  (let ((ll-table (create-ll0-table +grammar-1+))
        (terminals+end (append (grammar-terminals +grammar-1+) (list +end+)))
        (nonterminals (grammar-nonterminals +grammar-1+)))
    (dolist (X nonterminals)
      (dolist (Y terminals+end)
        (format t "M[~a,~a] = ~a~%" X Y (gethash Y (gethash X ll-table))))
      (format t "~%"))))

(defun test-ll0-table-grammar2()
  (let ((ll-table (create-ll0-table +grammar-2+))
        (terminals+end (append (grammar-terminals +grammar-2+) (list +end+)))
        (nonterminals (grammar-nonterminals +grammar-2+)))
    (dolist (X nonterminals)
      (dolist (Y terminals+end)
        (format t "M[~a,~a] = ~a~%" X Y (gethash Y (gethash X ll-table))))
      (format t "~%"))))
