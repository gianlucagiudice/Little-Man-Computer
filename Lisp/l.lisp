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
		      (when line 
            (cons (parse-line line) (read-helper))))))
      (read-helper))))

;; Convert line into a list
(defun parse-line (line)
  (labels ((parse-recursively (l) 
    (let ((trim-line (string-trim '(#\Space) l)))
      (when (> (length trim-line) 0)
        (let ((split-position
                ((lambda (pos trim) (if pos pos (length trim)))
                  (position #\Space trim-line) trim-line)))
          (cons
            (subseq trim-line 0 split-position)
            (parse-recursively
              (subseq trim-line split-position (length trim-line)))))))))
  ; Set up string for parsing
  (parse-recursively (substitute #\Space #\Tab
                        (remove-comment
                          (string-downcase line))))))

;(defun format-line (line)
;  (substitute #\Space #\Tab
;    (remove-comment (string-downcase line))))

;; Remove comment from line
(defun remove-comment (line)
  (let ((comment-position (search "\\" line)))
    (if comment-position
      (subseq line 0 comment-position)
      line)))


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
(defun lmc-load (filename)
  (read-file filename))


;instruction(add, 100, _).
;instruction(sub, 200, _).
;instruction(sta, 300, _).
;instruction(lda, 500, _).
;instruction(bra, 600, _).
;instruction(brz, 700, _).
;instruction(brp, 800, _).
;instruction(dat, 0  , _).
;instruction(dat, 0     ).
;instruction(hlt, 0     ).
;instruction(inp, 901   ).
;instruction(out, 902   ).