;; author *this* => ("Dylan Ball" "Arathnim@gmail.com")

(proclaim '(optimize (speed 3) (safety 0) (debug 0) (space 0)))
(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
(defpackage parmesan
   (:use cl alexandria iterate anaphora))

(in-package parmesan)

(defvar *str* nil)
(defvar *ind* 0)

(defstruct (parser-result (:conc-name result-))
   value (index 0 :type fixnum) status)

(defun cat (list)
   (format nil "~{~a~}" list))

(defmacro cat* (&rest rest)
  `(cat (list ,@rest)))

(defun merge-chars (seq)
   (coerce seq 'string))

(defun merge-str (seq)
   (if (every #'stringp seq)
       (cat seq)
       seq))

(declaim (inline fail))
(defun fail (expected actual &optional (index *ind*))
   (make-parser-result :value (list expected actual) :index index :status nil))

(declaim (inline pass))
(defun pass (form n)
   (declare (type fixnum n))
   (make-parser-result :value form :index (+ *ind* n) :status t))

(defun desugar (exp)
   (cond ((stringp exp) `(str ,exp))
         ((symbolp exp) `(,exp))
         ((numberp exp) `(str ,(string (code-char exp))))
         ((characterp exp) `(string ,exp))
         (t exp)))

(defun handle-result (r)
   (if (not (result-status r))
       (error "parsing error at position ~a, expected \"~a\", but found \"~a\""
         (result-index r) (first (result-value r)) (second (result-value r)))
       (result-value r)))

(defmacro parse (str form)
   `(let ((*str* ,str)
          (*ind* 0))
          (handle-result ,form)))

(defmacro test-remaining (n)
   `(>= (length *str*) (+ *ind* ,n)))

;; choice  ~ in the matching operation, returns the first form that consumes input
;; any     ~ matches zero or more of the next form
;; many    ~ matches one or more of the next form
;; one-of  ~ matches one character from the given string
;; none-of ~ matches only if none of the chars in the string match
;; times   ~ parses x occurances of y
;; try     ~ returns a parsing result, no effect on the normal parser stack
;; except  ~ matches one character from the string if the form does not match
;; ret     ~ removes the current parsing result and tries to parse x instead
;; option  ~ tries x, if it fails, parse y
;; seq     ~ matches each form sequentially, returns a list of forms or nil
;; par     ~ re-enter parse mode from normal function, internal use only!
;; str     ~ explicitly matches a string
;; sep     ~ seperates by repeated matching. mostly magic

(defun chr (c)
   (declare (type character c))
   (if (test-remaining 1) 
       (if (eql (char *str* *ind*) c)
           (pass c 1)
           (fail (string c) (char *str* *ind*)))
       (fail (string c) nil)))

(defun str (s)
   (declare (type string s))
   (iter (for i from 0 below (length s))
         (for x = (if (test-remaining (+ 1 i)) 
                      (char *str* (+ i *ind*)) 
                      (leave (fail (string (char s i)) nil (+ *ind* i)))))
         (for y = (char s i))
         (if (not (eql x y))
             (leave (fail (string y) x (+ *ind* i))))
         (finally (return (pass s (length s))))))

(defun one-of (s)
   (if (test-remaining 1)
       (iter (for x in-string s)
             (when (eql x (char *str* *ind*))
                   (leave (pass x 1)))
             (finally (return (fail s (char *str* *ind*)))))
       (fail s nil)))

;; TODO find a way to make errors accept negated logic
(defun none-of (s)
   (if (test-remaining 1) 
       (iter (for x in-string s)
             (when (eql x (char *str* *ind*))
                   (leave (fail s (char *str* *ind*))))
             (finally (return (pass (char *str* *ind*) 1))))
       (fail s nil)))

;; (defmacro many (form)
;;    (with-gensyms (res count acc) 
;;       `(let ((*ind* *ind*))
;;          (iter (for ,res = ,(desugar form))
;;                (for ,count upfrom 0)
;;                (setf *ind* (second ,res))
;;                (when (not (third ,res))
;;                      (if (> ,count 0)
;;                          (leave (list (if (every #'characterp ,acc) (merge-chars ,acc) ,acc) (second ,res) t))
;;                          (fail)))
;;                (collect (first ,res) into ,acc)))))

;; (defmacro any (form)
;;    (with-gensyms (res count acc) 
;;       `(let ((*ind* *ind*))
;;          (iter (for ,res = ,(desugar form))
;;                (for ,count upfrom 0)
;;                (setf *ind* (second ,res))
;;                (when (not (third ,res))
;;                      (leave (list (if (every #'characterp ,acc) (merge-chars ,acc) ,acc) (second ,res) t)))
;;                (collect (first ,res) into ,acc)))))

;; (defmacro seq (&rest forms)
;;    (if (not (eql 1 (length forms)))
;;        (with-gensyms (res inner)
;;          `(let ((,res ,(desugar (car forms))))
;;             (if (third ,res)
;;                 (let* ((*ind* (second ,res)) (,inner (seq ,@(cdr forms))))
;;                    (if (third ,inner) 
;;                        (list (cons (first ,res) (first ,inner)) 
;;                              (second ,inner)
;;                              t)
;;                        (fail)))
;;                 (fail))))
;;       (with-gensyms (str res)
;;          `(let ((,res ,(desugar (car forms))))
;;             (if (third ,res)
;;                 (list (cons (first ,res) nil) (second ,res) t)
;;                 (fail))))))
