;;; defstruct undefined label per le label non definite
;;; prima di tutto leggo le label e dopo inizio a compilare riga per riga

;; Struct for a defined-label
(defstruct defined-label
  name
  row)


;; Return the content of a file
(defun read-file (filename)
  (with-open-file (file filename :direction :input)
    (labels ((read-helper ()
	      (let ((line (read-line file nil nil)))
          ; Read file until the end
		      (when line
            ; Add the parsed line but skip if blank 
           (let ((parsed (parse-line line)))
              (if parsed (cons parsed (read-helper)) (read-helper)))))))  
      (read-helper))))


; Split line into a list
(defun parse-line (line)
  (labels ((parse-recursively (l)
    ; Trim the spaces
    (let ((trim-line (string-trim '(#\Space) l)))
        ; Stop if line is finished
        (when (> (length trim-line) 0)
          ; Find the split position
          (let ((split-position ((lambda (pos trim) (if pos pos (length trim)))
                                  (position #\Space trim-line) trim-line)))
            ; Add first word and recursively parse the rest 
            (cons (subseq trim-line 0 split-position)
                  (parse-recursively
                    (subseq trim-line split-position (length trim-line)))))))))
    ; Set up the line before starting to split
    (labels ((remove-comment (l)
      ; Remove the comment from line
      (let ((comment-position (search "\\" l)))
        (if comment-position (subseq l 0 comment-position) l))))
      ; Parse the line recursively
      (parse-recursively
        (substitute #\Space #\Tab (remove-comment (string-downcase line)))))))


(defun search-labels (line-list row)
  (when line-list
    ; Get the first word of an instruction
    (let ((first-word (first (car line-list))))
      ; The word is a label if and only if is not a reserver keyword
      (if (not (to-opcode first-word))
        ; Add label if is not alredy defined
        (cons (make-defined-label :name first-word :row row)
              (search-labels (cdr line-list) (+ row 1)))
        (search-labels (cdr line-list) (+ row 1))))))

(defun resolve-label (target-label labels-list)
  (when labels-list
    ; Return the row where a label is defined
    (if (equal (defined-label-name (car labels-list)) target-label)
      (defined-label-row (car labels-list))
      (resolve-label target-label (cdr labels-list)))))

(defun check-labels (labels-list)
  (labels ((check-recursively (label labels-list)
    (when labels-list 
      ; Return the label if is alredy defined
      (if (resolve-label (defined-label-name label) labels-list)
        label
        (check-recursively (car labels-list) (cdr labels-list))))))
    (let ((alredy-defined
            (check-recursively (car labels-list) (cdr labels-list))))
      (if alredy-defined
        ; If a label is alredy defined print error and return nil
        (format t "COMPILE ERROR: Label \"~A\" is alredy defined at row ~A."
          (defined-label-name alredy-defined)
          (defined-label-row alredy-defined))
        ; If no labels is alredy defined everithing is fine
        T))))

;; Get instruction opcode and check if accepts argument
(defun to-opcode (instruction)
  (cond ((equal instruction "add") (values 100  t))
        ((equal instruction "sub") (values 200  t))
        ((equal instruction "sta") (values 300  t))
        ((equal instruction "lda") (values 500  t))
        ((equal instruction "bra") (values 600  t))
        ((equal instruction "brz") (values 700  t))
        ((equal instruction "brp") (values 800  t))
        ((equal instruction "dat") (values 0    t))
        ((equal instruction "hlt") (values 0    nil))
        ((equal instruction "inp") (values 901  nil))
        ((equal instruction "out") (values 902  nil))))

(defun assembler (line-list defined-labels)
  (T))

; Given a file, return the content of the memory.
(defun lmc-load (filename)
  (let ((line-list (read-file filename)))
    (format t "~A~%" line-list)
    ; Searh all labels defined in asembly file
    (let ((labels-list (search-labels line-list 0)))
      (format t "~A~%" labels-list)
      ; Check if labels are defined more than once
      (when (check-labels labels-list)
        ; Covert each line into machine code
        T
        ))))
      


;(let ((groceries '(eggs bread butter carrots)))
;   (format t "~{~A~^, ~}.~%" groceries)         ; Prints in uppercase
;   (format t "~@(~{~A~^, ~}~).~%" groceries))   ; Capitalizes output
; ;; prints: EGGS, BREAD, BUTTER, CARROTS.
; ;; prints: Eggs, bread, butter, carrots.
      
; Se una istruzione è DAT allora faccio un append di 0