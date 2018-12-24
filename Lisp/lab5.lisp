;; Definiamo una funzione per moltiplicare ogni valore di una lista per due
(defun double-list (lst)
  (if (null lst)
      nil
      (cons (* 2 (car lst)) (double-list (cdr lst)))))

;; Ora ne definiamo una versione iterativa
(defun double-list-iter (lst acc)
  (if (null lst)
      (reverse acc)
      (double-list-iter (cdr lst) (cons (* 2 (car lst)) acc))))

;; Possiamo generalizzare la visita della lista applicando una funzione
;; ad ogni elemento della lista
(defun map-list (lst func)
  (if (null lst)
      nil
      (cons (funcall func (car lst)) (map-list (cdr lst) func))))
;; Recuperiamo la vecchia funzione di raddoppiare liste utilizzando
;; (map-list lst (lambda (x) (* 2 x)))
;; Common Lisp fornisce già una funzione che fa quello che fa map-list:
;; (mapcar funzione list)

;; Dato che possiamo passare e ritornare funzioni come argomento possiamo
;; avere applicazioni parziali. In questo caso di una funzione di due
;; elementi
(defun partial-apply (func x)
  (lambda (y) (funcall func x y)))
;; In questo modo possiamo riottenere la funzione per raddoppiare i valori
;; in una lista come una applicazione parziale di mapcar in cui il primo
;; argomento è fissato a (lambda (x) (* 2 x)).
;; Ad esempio:
(let ((list-doubler (partial-apply #'mapcar (lambda (x) (* 2 x)))))
  (format t "~a~%" (funcall list-doubler '(1 2 3)))
  (format t "~a~%" (funcall list-doubler '(0 -3 8 9 -6)))
  (format t "~a~%" (funcall list-doubler '(8 7 6))))
;; Nel precedente codice "list-doubler" è il risultato dell'applicazione di
;; mapcar in cui abbiamo fissato il primo argomento ad una lista, ottenenedo
;; quindi un "raddoppiatore" del valore contenuto in una lista

;; Definizione della struttura di nodo con tre campi:
;; value (valore contenuto)
;; left e right, i due sottoalberi.
;; Di default sono tutte a nil
(defstruct node
  value
  left
  right)
;; make-node (per creare un nodo), node-value, node-left e node-right
;; sono creati in automatico quando la stuttura node viene definita.

;; RItorna un albero in cui è stato inserito il nuovo valore "value"
(defun tree-insert (root value)
  (let ((v (unless (null root) (node-value root))))
    (cond ((null root) (make-node :value value))
	  ((= v value) root)
	  ((> v value)
	   (make-node :value v
		      :left (tree-insert (node-left root) value)
		      :right (node-right root)))
	  ((< v value)
	   (make-node :value v
		      :left (node-left root)
		      :right (tree-insert (node-right root) value))))))

;; Funzione che effettua la ricerca all'interno di un albero binario di ricerca
(defun tree-search (root value)
  (let ((v (unless (null root) (node-value root))))
    (cond ((null root) nil)
	  ((= v value) t)
	  ((> v value) (tree-search (node-left root) value))
	  ((< v value) (tree-search (node-right root) value)))))

