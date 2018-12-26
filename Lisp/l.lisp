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
           (let ((parsed (parse-line (format-line line))))
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


;(defun search-labels (line-list)
;  ((when (car line-list)
;      
;      )))


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
        

;; Given a file, return the content of the memory.
;(defun lmc-load (filename)
;  (let ((line-list (read-file filename)))
;    (let ((labels-list (search-labels line-list)))
;      (write labels-list)
;      )))

(defun lmc-load (filename)
  (let ((line-list (read-file filename)))
    line-list))


; Se una istruzione è DAT allora faccio un append di 0
