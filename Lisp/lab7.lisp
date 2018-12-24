;;; RLE
(defstruct run value length)

(defun find-run (to-process processed)
  (cond ((null to-process)
          (values processed nil))
        ;;; (1 1 2 2 3) () => (1 2 2 3) (1)
        ((null processed)
          (find-run (cdr to-process) (list (car to-process))))
        ((eq (car to-process) (car processed))
          (find-run (cdr to-process) (cons (car to-process) processed)))
        (t (values processed to-process))))

(defun rle-encode (lst)
  (when lst
    (multiple-value-bind (run what-remains) (find-run lst nil)
      (cons
        (make-run :value (car run) :length (length run))
        (rle-encode what-remains)))))

;(defun rle-decode (lst)
;  (labels ((unpack-run (run)
;            (if (= (run-length r) 0)
;              nil
;              (cons (run value r)
;                    (unpack-run (make-run :value (run-value r)
;                                          :length (- (run-length r) 1))))))))

(defun rle-decode (lst)
  (flet ((unpack-run (r)
            (make-list (run-length r) :initial-element (run-value r))))
    (when lst
      (append (unpack-run (car lst)) (rle-decode (cdr lst))))))




(defun tree-search (root value)
(let ((v (unless (null root) (node-value root))))
  (cond ((null root) nil)
  ((= v value) t)
  ((> v value) (tree-search (node-left root) value))
  ((< v value) (tree-search (node-right root) value)))))
