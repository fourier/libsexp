(defconstant +dot+ '\.)
(defconstant +empty+ 'empty)
(defconstant +end+   '$)

(defstruct grammar
  (terminals nil :type list)
  (nonterminals nil :type list)
  (start-symbol nil :type symbol)
  (rules nil :type list))

(defstruct parse-table
  (action-table (vector) :type vector)
  (goto-table (vector) :type vector))

;; grammar 0 - from Aho, Ulman
(defparameter *grammar-0* (make-grammar
                           :terminals (list '\( '\) 'id '+ '*)
                           :nonterminals (list 'E1 'E 'T 'F)
                           :start-symbol 'E1
                           :rules 
                           (list 
                            (list 'E1 'E)               ; E' -> E
                            (list 'E (list 'E '+ 'T))   ; E -> E+T
                            (list 'E 'T)                ; E -> T
                            (list 'T (list 'T '* 'F))   ; T -> T*F
                            (list 'T 'F)                ; T -> F
                            (list 'F (list '\( 'E '\))) ; F -> (E)
                            (list 'F 'id))))            ; F -> id


(defparameter *I0* (list (list 'E1 
                               (list +dot+ 'E))))

;; grammar 1 - from Aho, Ulman, for LL0 parsers
(defparameter *grammar-1* (make-grammar
                           :terminals (list '\( '\) 'id '+ '*)
                           :nonterminals (list 'E1 'E 'T 'T1 'F)
                           :start-symbol 'E1
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

;; grammar 2 - SEXP - for lisp(scheme) syntax

(defparameter *grammar-2* (make-grammar
                           :terminals (list 'atom 'OpenParen 'CloseParen)
                           :nonterminals (list 'S1 'S 'E 'L)
                           :start-symbol 'S1
                           :rules
                           (list
                            (list 'S1 'S)
                            (list 'S 'atom)                ; S -> atom
                            (list 'S 'L)                   ; S -> L
                            (list 'E 'S)                   ; E -> S
                            (list 'E (list 'S 'E))         ; E -> SE
                            (list 'L (list 'OpenParen 'CloseParen))       ;L -> ()
                            (list 'L (list 'OpenParen 'E 'CloseParen))))) ; L -> (E)

;; grammar 3 - from wikipedia article about LR0 parsers

(defparameter *grammar-3* (make-grammar
                           :terminals (list '0 '1 '\* '+)
                           :nonterminals (list 'E1 'E 'B)
                           :start-symbol 'E1
                           :rules
                           (list
                            (list 'E1 'E)
                            (list 'E (list 'E '* 'B))       ; (1) E -> E * B
                            (list 'E (list 'E '+ 'B))       ; (2) E -> E + B
                            (list 'E 'B)                    ; (3) E -> B
                            (list 'B '0)                    ; (4) B -> 0
                            (list 'B '1))))                 ; (5) B -> 1

;; grammar 4 - from wikipedia article about SLR parsers

(defparameter *grammar-4* (make-grammar
                           :terminals (list '1)
                           :nonterminals (list 'S 'E)
                           :start-symbol 'S
                           :rules
                           (list
                            (list 'S 'E)                    ; (0) S -> E
                            (list 'E (list '1 'E))          ; (1) E -> 1 E
                            (list 'E '1))))                 ; (2) E -> 1

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


(defun string-replace (str1 sub1 sub2)
  "Replaces the occurence of sub1 in str1 with sub2"
  (let ((str1 (string str1))
        (str2 "")
        (sub1 (string sub1))
        (sub2 (string sub2))
        (index1 0))
    (loop
       (if (string-equal str1 sub1
                         :start1 index1
                         :end1 (min (length str1)
                                    (+ index1 (length sub1))))
           (progn
             (setq str2 (concatenate 'string str2 sub2))
             (incf index1 (length sub1)))
           (progn
             (setq str2 (concatenate 'string
                                     str2
                                     (subseq str1 index1 (1+ index1))))
             (incf index1)))
       (unless (< index1 (length str1))
         (return str2)))))


(defun pretty-print-rule-to-string (R)
  "Pretty print rule to the string (as a return value)"
  (let ((rule-nonterminal (first R))
        (rule (second R)))
    (with-output-to-string (str)
      ;; print the rule leading nonterminal (left part)
      (format str "~a -> " rule-nonterminal)
      (if (atom rule)
          ;; if atom just print it
          (format str "~a" rule)
          ;; if list walk through the list and print element
          (dolist (X rule)
            ;; in case if it is a dot print '.' instead
            (if (eq X +dot+)
                (format str ".")
                (format str "~a " X))))
      str)))


(defun pretty-print-rule (R)
  "Pretty-print the rule R"
  (format t "~a" (pretty-print-rule-to-string R))
  (format t "~%"))

(defun print-grammar (grammar)
  (let ((rules (grammar-rules grammar)))
           (dotimes (i (length rules))
             (format t "(~d) ~a" i (nth i rules))
             (format t "~%"))))

(defun print-items (items-list)
  (dotimes (i (length items-list))
    (format t "I~d: ~%" i)
    (dolist (X (nth i items-list))
      (pretty-print-rule X))))

(defun nonterminal-for-closure (grammar production)
  "Helper function for rule-for-closure;
do the actual work for right-side of the rule"
  (when (and (listp production)
             (>= (length production) 2))
    (let ((fst (first production))
          (snd (second production)))
      (or (and (eq fst +dot+)
               (find snd (grammar-nonterminals grammar) :test #'equal))
          (nonterminal-for-closure grammar (rest production))))))


(defun nonterminal-from-rule-for-closure (grammar rule)
  "Determine the rule to holding +dot+ followed by nonterminal
Returns nonterminal if (+dot+,nonterminal) pair found or nil otherwise"
  (nonterminal-for-closure grammar (second rule)))



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
  "Construct rule for the closure - add dot at the first position
before nonterminal"
  (let ((nonterminal (first rule))
        (production (second rule)))
    (if (listp production)
        (cons nonterminal (list (cons +dot+ production)))
        (cons nonterminal (list (list +dot+ production))))))

(defun closure-rules-for-closure-rule (grammar rule)
  "Construct all closure rules for given rule with dot"
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
  "Construct closure array for the given set of rules with dot"
  (when I
    (let ((s (length I)))
      (dolist (rule I)
        (let ((rules (closure-rules-for-closure-rule grammar rule)))
          (setf I (add-items-to-closure I rules))))
      (if (= (length I) s)
          I
          (closure grammar I)))))

(defun move-dot-to-right (rule)
  "If the rule contains the dot, move dot to the next position
like A -> .B becomes A -> B.
if rule is like A -> B.
then return nil;;no modifaction 
Also if no dot found returns nil"
  (let* ((left-nonterminal (first rule))
         (right-production (second rule))
         (found nil)
         (result nil)
         (dot-position (member +dot+ right-production :test 'equal)))
    (when dot-position
      (if (= 1 (length dot-position))
          nil ;; last symbol is dot
          (if (and dot-position
                   (> (length dot-position) 1))
              (progn
                (dolist (x right-production)
                  (if (not (eq x +dot+))
                      (progn
                        (setf result
                              (push-back x result))
                        (when found
                          (setf result (push-back +dot+ result))
                          (setf found nil)))
                      (setf found t)))
                (list left-nonterminal result)))))))

(defun goto (grammar I X)
  "GOTO function for the LR(1) grammar
from book \"Compilers: Principles, Techniques, and Tools\" by  Aho, Sethi, Ullman"
  (let ((goto-list nil)
        (result-list nil))
    (dolist (R I)
      (let ((production (second R)))
        (when (listp production)
          (let ((found (member +dot+ production)))
            (when (and found
                       ;; at least one symbol after dot
                       (> (length found) 1)
                       (eq (second found) X))
              (setf goto-list (push-back R goto-list)))))))
    (when goto-list
      (dolist (R goto-list)
        (let ((rule (move-dot-to-right R)))
          (when rule
            (setf result-list
                  (append result-list (closure grammar (list rule))))))))
    result-list))

;; auxulary function - remove later if not needed
(defun find-grammar-symbols-after-dot (I)
  (let ((result nil))
    (dolist (R I)
      (let* ((right-production (second R))
             (dot-position (member +dot+ right-production :test 'equal)))
        (when ( > (length dot-position) 1)
          (let ((grammar-symbol (second dot-position)))
            (when (not (find grammar-symbol result))
              (setf result (push-back grammar-symbol result)))))))
    result))

(defun grammar-ll0-items (grammar)
  "items function for the LR(1) grammar
from book \"Compilers: Principles, Techniques, and Tools\" by  Aho, Sethi, Ullman"
  (let* ((I0 (list (rule-for-closure (first
                                (find-all-productions
                                 grammar
                                 (grammar-start-symbol grammar))))))
         (C (closure grammar I0)) ;; first closure 
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
  "FIRST(X) function for LL and LR(1) parsers
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
  "FIRST(X) function for LL and LR(1) parsers
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


(defun slr-table (grammar)
  "Create the SLR parse table for the given grammar"
  (let* ((table (make-parse-table))     ; resulting table
         (items (grammar-ll0-items grammar)) ; items (LL0)
         (follow (create-follow-table grammar))) ; follow table
    (flet ((index-in (items item)
             (let ((r -1))
               (dotimes (i (length items))
                 (when (equal item (nth i items))
                   (setf r i)))
           r))
           (remove-dot (R)
             (let ((prod (remove +dot+ (second R))))
               (cons (first R)
                     (if (= (length prod) 1)
                         prod
                         (list prod)))))
           (string-for-value (value new-value)
             (if (or (not value)
                     (string= value new-value))
                 new-value
                 (format nil "~a/~a" value new-value))))
      ;; 1. initialize results with vectors of hashes
      (setf (parse-table-action-table table)         
            (make-array (length items) :initial-element nil)
            (parse-table-goto-table table)
            (make-array (length items) :initial-element nil))
      (dotimes (i (length items))
        (setf (aref (parse-table-goto-table table) i)
              (make-hash-table)
              (aref (parse-table-action-table table) i)
              (make-hash-table)))
      ;; 2. create goto-table
      (dotimes (i (length items))
        (dolist (X (grammar-nonterminals grammar))
          (let* ((goto-items (goto grammar (nth i items) X))
                 (index (index-in items goto-items)))
            (when (not (eq index -1 ))
              (setf (gethash X
                             (aref (parse-table-goto-table table) i))
                    index)))))
      ;; 3. create action-table
      (let ((terminals (append (grammar-terminals grammar) (list +end+))))
        (dotimes (i (length items))
          (let ((item (nth i items)))
            (dolist (R item)
              (let ((after-dot (member +dot+ (second R))))
                (cond
                  ;; 3.1 if S' -> S. in I_i
                  ((eq (first R) (grammar-start-symbol grammar))
                   (when (eq (car (last (second R))) +dot+)
                     ;; value and new-value are just
                     ;; preparations for conflicts
                     (let ((value
                            (gethash +end+
                                     (aref
                                      (parse-table-action-table table) i))))
                       (setf (gethash +end+
                                      (aref (parse-table-action-table table) i))
                             (string-for-value value "acc")
                             ))))
                  ;; 3.2 A -> w. in I_i
                  ((and (> (length after-dot) 1)
                        (member (second after-dot) terminals))
                   ;; terminal
                   (let* ((term (second after-dot))
                          (Ij (goto grammar (nth i items) term))
                          (j (index-in items Ij)))
                     (when (and Ij
                                (/= j -1 ))
                       (let ((value (gethash term
                                      (aref (parse-table-action-table table) i)))
                             (new-value (format nil "s~d" j)))
                         (setf (gethash term
                                        (aref (parse-table-action-table table) i))
                               (string-for-value value new-value))))))
                   ;; 3.3 A -> w.av in I_i, a - terminal
                  ((and (eq (car (last (second R))) +dot+)
                        (not (eq (first R) (grammar-start-symbol grammar))))
                   (let* ((follow-A (gethash (first R) follow))
                          (rule (remove-dot R))
                          (index (index-in (grammar-rules grammar) rule)))
                     (when (= index -1) (print rule))
                     (dolist (term follow-A)
                       (let ((value (gethash term
                                      (aref (parse-table-action-table table) i)))
                             (new-value (format nil "r~d" index)))
                         (setf (gethash
                                term
                                (aref (parse-table-action-table table) i))
                               (string-for-value value new-value))))))))))))
      table)))


    
(defun print-slr-table-and-grammar-to-file (grammar table filename)
  (labels ((enum-formatting-function (x)
             (concatenate 'string "E" (symbol-name x)))
           (enum-nonterm-formatting-function (x)
             (concatenate 'string "ENONTERM_" (symbol-name x)))
           (enum-formatter (stream enum-name enum-list)
             (format stream "typedef enum~%{~%")
             (format stream "~{  ~a~^,~%~}~%} ~a;~%~%"
                     enum-list enum-name))
           (array-formatter-decl (stream arr-name arr-list &key (type "int"))
             (let ((arr-size-string
                    (concatenate 'string (string-upcase arr-name) "_SIZE")))
               (format stream "#define ~a ~d~%" arr-size-string (length arr-list))
               (let ((s
                      (with-output-to-string (str)
                        (format str "const ~a ~a[~a]"
                                type arr-name arr-size-string))))
                 (format stream "extern ~a;~%~%" s)
                 s)))
           (array2-formatter-decl (stream arr-name arr-type rows-list cols-list)
             (let ((rows-string
                    (concatenate 'string (string-upcase arr-name) "_ROWS"))
                   (cols-string
                    (concatenate 'string (string-upcase arr-name) "_COLS")))
               (format stream "#define ~a ~d~%" rows-string (length rows-list))
               (format stream "#define ~a ~d~%" cols-string (length cols-list))
               (let ((s
                      (with-output-to-string (str)
                        (format str "const ~a ~a[~a][~a]"
                                arr-type arr-name rows-string cols-string))))
                 (format stream "extern ~a;~%~%" s)
                 s)))
           (array-to-string (arr)
             (with-output-to-string (str)
               (format str "{~{~a~^, ~}};~%~%"
                       arr)))
           (rule-to-string (R)
             (with-output-to-string (str)
               (format str "{~a, ~d, \"~a\"}"
                       (enum-nonterm-formatting-function (first R))
                       (if (listp (second R))
                           (length (second R))
                           1)
                       (pretty-print-rule-to-string R))))
           (action-formatter (action)
             (with-output-to-string (str)
               (cond ((null action)
                      (format str "{~a, -1}" (enum-formatting-function 'Invalid)))
                     ((string= action "acc")
                      (format str "{~a, 0}" (enum-formatting-function 'Accept)))
                     ((char= #\s (char action 0))
                      (format str "{~a, ~d}"
                              (enum-formatting-function 'Shift)
                              (parse-integer action :start 1)))
                     ((char= #\r (char action 0))
                      (format str "{~a, ~d}"
                              (enum-formatting-function 'Reduce)
                              (parse-integer action :start 1)))))))
    (let* ((filename-header (concatenate 'string filename ".h"))
           (filename-source (concatenate 'string filename ".c"))
           (enum-terminals (mapcar #'enum-formatting-function
                                (append (grammar-terminals grammar)
                                        (list 'end))))
           (enum-nonterminals (mapcar #'enum-nonterm-formatting-function
                                (grammar-nonterminals grammar)))
           (include-guard
            (format nil "__~a__" (string-upcase
                                  (string-replace
                                   (file-namestring filename-header) "." "_"))))
           (declarations (make-hash-table)))
      (with-open-file (header (merge-pathnames filename-header)
                              :direction :output
                              :if-exists :supersede)
        (format header "/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */~%")
        ;; opening include-guard
        (format header "#ifndef ~a~%#define ~a~%~%" include-guard include-guard)
        (enum-formatter header "ParserState"
                        (mapcar #'enum-formatting-function
                                (list 'Invalid 'Shift 'Reduce 'Accept)))
        ;; declaration of the action table element
        ;; terminals enum
        (enum-formatter header "TerminalType" enum-terminals)
        ;; nonterminals enum
        (enum-formatter header "NonterminalType" enum-nonterminals)
        (format header
                "~%typedef struct~%{~%  ParserState type;~%  int number;~%} action;~%~%")
        (format header
                "typedef struct~%{~%  NonterminalType start_symbol;~%  int size;~%  const char* print_form;~%} grammar_rule;~%~%")
        ;; lists of terminals and nonterminals
        (setf (gethash 'terminals-list declarations)
              (array-formatter-decl header "terminals_list" enum-terminals))
        (setf (gethash 'nonterminals-list declarations)
              (array-formatter-decl header "nonterminals_list" enum-nonterminals))
        ;; list of grammar rules
        (setf (gethash 'grammar-rules declarations)
              (array-formatter-decl header "grammar_rules_list"
                                    (grammar-rules grammar) :type "grammar_rule"));
        ;; declaration of the ACTION table
        (setf (gethash 'action-table declarations)
              (array2-formatter-decl header "action_table" "action"
                                     (parse-table-action-table table)
                                     (append (grammar-terminals grammar)
                                             (list +end+))))
        ;; declaration of the GOTO table
        (setf (gethash 'goto-table declarations)
              (array2-formatter-decl header "goto_table" "int"
                                     (parse-table-goto-table table)
                                     (grammar-nonterminals grammar)))
        ;; closing include-guard
        (format header "~%#endif /* ~a */~%" include-guard))
      (with-open-file (source (merge-pathnames filename-source)
                              :direction :output
                              :if-exists :supersede)
        (format source "/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */~%")
        (format source "#include \"~a\"~%~%" (file-namestring filename-header))
        ;; terminals
        (format source "~a~%" (concatenate 'string
                                           (gethash 'terminals-list declarations)
                                           " = "
                                           (array-to-string enum-terminals)))
        ;; nonterminals
        (format source "~a~%" (concatenate 'string
                                           (gethash 'nonterminals-list declarations)
                                           " = "
                                           (array-to-string enum-nonterminals)))
        (format source "~a~%" (concatenate 'string
                                           (gethash 'grammar-rules declarations)
                                           " = "
                                           (with-output-to-string (str)
                                             (format str "{~{~a~^, ~}};~%~%"
                                                     (mapcar
                                                      #'rule-to-string
                                                      (grammar-rules grammar))))))
        ;; action table
        (let ((action-rows nil))
          (dotimes (i (length (parse-table-action-table table)))
            (push 
             (with-output-to-string (str)
               (let ((actions-list nil))
                 (dolist (X (append (grammar-terminals grammar)
                                    (list +end+)))
                   (push
                    (action-formatter 
                     (gethash X (aref (parse-table-action-table table) i)))
                   actions-list))
                 (format str "{~{~a~^, ~}}" (reverse actions-list))))
             action-rows))
          (format source "~a =  ~%{~%" (gethash 'action-table declarations))
          (format source "~{  ~a~^,~%~}" (reverse action-rows))
          (format source "~%};~%~%"))
        ;; goto table

        (let ((goto-rows nil))
          (dotimes (i (length (parse-table-goto-table table)))
            (push
             (with-output-to-string (str)
               (let ((goto-list nil))
                 (dolist (X (grammar-nonterminals grammar))
                   (let ((goto-elt (gethash X (aref (parse-table-goto-table table) i))))
                     (push
                      (if goto-elt goto-elt -1)
                      goto-list)))
                 (format str "{~{~a~^, ~}}" (reverse goto-list))))
             goto-rows))
          (format source "~a = ~%{~%" (gethash 'goto-table declarations))
          (format source "~{  ~a~^,~%~}" (reverse goto-rows))
          (format source "~%};~%~%"))))))

(when (> (length *posix-argv*) 0)
  (print-slr-table-and-grammar-to-file *grammar-2*
                                       (slr-table *grammar-2*)
                                       (second *posix-argv*)))
(quit)
    
;; ;;
;; ;; Tests
;; ;;

;; (defun test-create-first-table0()
;;   (let ((first-table (create-first-table *grammar-0*)))
;;     (dolist (X (grammar-nonterminals *grammar-0*))
;;       (format t "term = ~a: " X)
;;       (print
;;        (first-function X first-table))
;;       (format t "~%"))))


;; (defun test-create-first-table1()
;;   (let ((first-table (create-first-table *grammar-1*)))
;;     (dolist (X (grammar-nonterminals *grammar-1*))
;;       (format t "term = ~a: " X)
;;       (print
;;        (first-function X first-table))
;;       (format t "~%"))))

;; (defun test-create-first-table2()
;;   (let ((first-table (create-first-table *grammar-2*)))
;;     (dolist (X
;;               (append (grammar-nonterminals *grammar-2*)
;;                       (list 
;;                        (list 'S 'E) ;;FIRST(SE)  = {a,b}
;;                        (list 'b 'E 'c);;  FIRST(bEc) = {b}
;;                        (list 'b 'c))));;  FIRST(bc)  = {b}
;;       (format t "term = ~a: " X)
;;       (print
;;        (first-function X first-table))
;;       (format t "~%"))))


;; (defun test-ll0-table-grammar0()
;;   (let ((ll-table (create-ll0-table *grammar-0*))
;;         (terminals+end (append (grammar-terminals *grammar-0*) (list +end+)))
;;         (nonterminals (grammar-nonterminals *grammar-0*)))
;;     (dolist (X nonterminals)
;;       (dolist (Y terminals+end)
;;         (format t "M[~a,~a] = ~a~%" X Y (gethash Y (gethash X ll-table))))
;;       (format t "~%"))))

;; (defun test-ll0-table-grammar1()
;;   (let ((ll-table (create-ll0-table *grammar-1*))
;;         (terminals+end (append (grammar-terminals *grammar-1*) (list +end+)))
;;         (nonterminals (grammar-nonterminals *grammar-1*)))
;;     (dolist (X nonterminals)
;;       (dolist (Y terminals+end)
;;         (format t "M[~a,~a] = ~a~%" X Y (gethash Y (gethash X ll-table))))
;;       (format t "~%"))))

;; (defun test-ll0-table-grammar2()
;;   (let ((ll-table (create-ll0-table *grammar-2*))
;;         (terminals+end (append (grammar-terminals *grammar-2*) (list +end+)))
;;         (nonterminals (grammar-nonterminals *grammar-2*)))
;;     (dolist (X nonterminals)
;;       (dolist (Y terminals+end)
;;         (format t "M[~a,~a] = ~a~%" X Y (gethash Y (gethash X ll-table))))
;;       (format t "~%"))))

;; (defun print-action-goto-table (grammar table)
;;   (let ((len (length (parse-table-goto-table table)))
;;         (nonterminals (remove (grammar-start-symbol grammar)
;;                               (grammar-nonterminals grammar)))
;;         (terminals (append (grammar-terminals grammar) (list +end+))))
;;     (format t "GOTO:~%")
;;     (dolist (X nonterminals)
;;       (format t "  ~a" X))
;;     (format t "~%")
;;     (dotimes (i len)
;;       (format t "~d " i)
;;       (dolist (X nonterminals)
;;         (format t "~a " (gethash X (aref (parse-table-goto-table table) i))))
;;       (format t "~%"))
;;     (format t "ACTION:~%")
;;     (dolist (X terminals)
;;       (format t " ~a" X))
;;     (format t "~%")
;;     (dotimes (i len)
;;       (format t "~d " i)
;;       (dolist (X terminals)
;;         (format t "~a " (gethash X (aref (parse-table-action-table table) i))))
;;       (format t "~%"))))
