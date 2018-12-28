;;; Matr: 830694
;;; Written by: Gianluca Giudice.

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


;; Split line into a list
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


;; Search all the labels defined in the assembly file
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


;; Return the row where a target label is defined
(defun resolve-label (target-label labels-list)
  (when labels-list
    ; Return the row where a label is defined
    (if (equal (defined-label-name (car labels-list)) target-label)
      (defined-label-row (car labels-list))
      (resolve-label target-label (cdr labels-list)))))


;; Check if all the labels are defined only once
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


;; Convert each line of the programm
(defun assembler (line-list labels-list)
  (labels ((assembler-line (instruction)
    (labels ((evaluate-argument (argument)
      (multiple-value-bind
        ; Check if the argument is a label or a number
        (value len) (parse-integer argument :junk-allowed t)
        (if (and (= len (length argument)) (and (>= value 0) (< value 100)))
          ; If is a valid number return it
          value
          ; Else check if is a valid label
          (let ((resolved (resolve-label argument labels-list)))
            (if resolved resolved
              (format t "COMPILE ERROR: Label ~A unefined.~%" argument)))))))
    ; Evaluate the length of instruction
    (cond ((= (length instruction) 1)
            ; DAT is a special instruction: it can either have or not argument
            (if (equal (first instruction) "dat")
              ; Standardise DAT instruction
              (assembler-line (list "dat" "0"))
              ; If is not a DAT instruction compile
              (multiple-value-bind
                ; Check if is a valid instruction whitout argument
                (opc has-argument) (to-opcode (first instruction))
                  (if (and opc (not has-argument))
                    opc (format t "COMPILE ERROR: Invalid instruction.~%")))))
          ((= (length instruction) 2)
            (multiple-value-bind
              ; Check if is a valid instruction with argument
              (opc has-argument) (to-opcode (first instruction))
                ; If length is 2 there are two cases
                (cond ((and opc has-argument)
                      ; Simple instruction followed by its argument
                        (let ((arg (evaluate-argument (second instruction))))
                          (when arg (+ opc arg))))
                      ((and (not opc) (not has-argument))
                      ; Label followed by instruction whitout argument
                        (assembler-line (cdr instruction)))
                      ; Compile Error
                      (t (format t "COMPILE ERROR: Invalid instruction.~%")))))
          ((= (length instruction) 3)
            ; First word must be a label and second and instruction
            (when (and (not (to-opcode (first instruction)))
                       (to-opcode (second instruction)))
              (assembler-line (cdr instruction))))
          (t (format t "COMPILE ERROR: Invalid instruction.~%"))))))
    ; Compile each instruction
    (when line-list
      (let ((compiled (assembler-line (car line-list))))
        (if compiled
          ; If instruction compiled succesfully continue recursivly
          (cons compiled (assembler (cdr line-list) labels-list))
          ; If error occured, compile process fail
          (format t "Instruction: ~@(~{~A~^ ~}~).~%" (car line-list)))))))


; Given a file, return the content of the memory.
(defun lmc-load (filename)
  (let ((line-list (read-file filename)))
    ; Control memeory overflow
    (if (<= (length line-list) 100)
      ; Searh all labels defined in asembly file 
      (let ((labels-list (search-labels line-list 0)))
          ; Check if labels are defined more than once
          (when (check-labels labels-list)
            ; Compile each line
            (let ((mem (assembler line-list labels-list)))
              (when (= (length mem) (length line-list))
                ; Each line has been compiled
                (format t "Msg: Compiled succesfully.~%")
                ; Fill the memory whit 0s
                (append mem
                  (make-list (- 100 (length mem)) :initial-element '0))))))
      ; Memory overflow
      (format t "COMPILE ERROR: Too many instructions to load in memory.~%"))))

; Given a state return the new state
(defun one-instruction (state)
  ; If is a "state" continue
  (when (eq (car state) 'STATE)
    ; Getter and setter for state
    (labels ((get-element (element)
                (nth (+ (position element state) 1) state))
              (set-element (element new)
                (setf (nth (+ (position element state) 1) state) new)))
      (labels ((execute-instruction (machine-code)
        (multiple-value-bind (opc arg) (floor machine-code 100)
          (labels ((evaluate-flag (acc)
                      (if (or (>= acc 1000) (< acc 0)) 'flag 'noflag))
                   (increment-pc (pc) (mod (+ pc 1) 100)))
            (cond
                  ; ADD instruction
                  ((= opc 1)
                    (progn
                      ; Evaluate ADD
                      (set-element :acc
                        (+ (get-element :acc) (nth arg (get-element :mem))))
                      ; Set the flag
                      (set-element :flag (evaluate-flag (get-element :acc)))
                      ; Increment the PC
                      (set-element :pc (increment-pc (get-element :pc)))
                      ; Return the state
                      state))
                  ; SUB instruction
                  ((= opc 2)
                    (progn
                      ; Evaluate SUB
                      (set-element :acc
                        (- (get-element :acc) (nth arg (get-element :mem))))
                      ; Set the flag
                      (set-element :flag (evaluate-flag (get-element :acc)))
                      ; Increment the PC
                      (set-element :pc (increment-pc (get-element :pc)))
                      ; Return the state
                      state))
                  ; STA instruction
                  ((= opc 3)
                    (progn
                      ; Store the acc in memory
                      (setf (nth arg (get-element :mem)) (get-element :acc))
                      ; Increment the PC
                      (set-element :pc (increment-pc (get-element :pc)))
                      ; Return the state
                      state))
                  ; LDA instruction
                  ((= opc 5)
                    (progn
                      ; Load Acc from Memory
                      (set-element :acc (nth arg (get-element :mem)))
                      ; Increment the PC
                      (set-element :pc (increment-pc (get-element :pc)))
                      ; Return the state
                      state))
                  ; BRA instruction
                  ((= opc 6)
                    (progn
                      ; Jump
                      (set-element :pc arg)
                      ; Return the state
                      state))
                  ; BRZ instruction
                  ((= opc 7)
                    (progn
                      (if (and (= (get-element :acc) 0)
                               (eq (get-element :flag) 'noflag))
                          ; If conditions satisfied jump
                          (set-element :pc arg)
                          ; Else increment the PC
                          (set-element :pc (increment-pc (get-element :pc))))
                      ; Return the state
                      state))
                  ; BRP instruction
                  ((= opc 8)
                    (progn
                      (if (eq (get-element :flag) 'noflag)
                          ; If conditions satisfied jump
                          (set-element :pc arg)
                          ; Else increment the PC
                          (set-element :pc (increment-pc (get-element :pc))))
                      ; Return the state
                      state))
                  ; INP instruction
                  ((and (= opc 9) (= arg 01))
                    (if (null (get-element :in))
                      ; Empty input list
                      (format t
                        "RUNTIME ERROR: Trying to read empty input list")
                      ; Execute input instruction
                      (progn
                        ; Load the first element of input in acc
                        (set-element :acc (car (get-element :in)))
                        ; Remove the first element of input
                        (set-element :in (cdr (get-element :in)))
                        ; Increment the PC
                        (set-element :pc (increment-pc (get-element :pc)))
                        ; Return the state
                        state)))
                  ; OUT instruction
                  ((and (= opc 9) (= arg 02))
                    (progn
                        ; Append acc value to out list
                        (set-element :out
                          (append (get-element :in) (get-element :out)))
                        ; Increment the PC
                        (set-element :pc (increment-pc (get-element :pc)))
                        ; Return the state
                        state))
                  ; HLT
                  ((= opc 0) (cons 'halted-state (cdr state)))
                  ; ERROR
                  (t (format t "RUNTIME ERROR: Instruction not valid.")))))))
        ; Execute instruction pointed by the program counter
        (execute-instruction (nth (get-element :PC) (get-element :MEM)))))))


; Execute until the end
(defun execution-loop (state)
  (when state
    (if (eq (car state) 'state)
      ; Recursivly continue if state
      (execution-loop (one-instruction state))
      ; Hatled state reached
      (progn
        (format t "Msg: Execution has been completed.")
        (nth 10 state)))))