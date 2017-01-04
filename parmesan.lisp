;; author *this* => ("Dylan Ball" "Arathnim@gmail.com")

(proclaim '(optimize (speed 0) (safety 3) (debug 3) (space 0)))
(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
(ql:quickload '(alexandria iterate anaphora) :silent t)
(defpackage parmesan
   (:use cl alexandria iterate anaphora))

(in-package parmesan)

(defvar *source* nil)
(defvar *index* 0)

(defstruct (source)
   value
   (cached-string "")
   (max-index -1))

(defun get-character (n)
   (if (stringp (source-value *source*))
       (char (source-value *source*) n)
       (if (> n (source-max-index *source*))
           (iter (until (eql (source-max-index *source*) n))
                 (append-character (read-char (source-value *source*)))
                 (finally (return (char (source-cached-string *source*) n))))
           (char (source-cached-string *source*) n))))

(defun append-character (c)
   (setf (source-cached-string *source*)
         (concatenate 'string
            (source-cached-string *source*)
            (string c)))
   (incf (source-max-index *source*)))

(declaim (inline error?))
(defstruct (parser-error (:conc-name error-) (:predicate error?))
   type index args)

(defun kitten (&rest list)
   (format nil "~{~a~}" list))

(defun cat (&rest list)
   (format nil "~{~a~}" 
      (remove-if-not (lambda (x) x) list)))

(declaim (inline pass))
(defun pass (value n)
   (incf *index* n)
   value)

(declaim (inline fail))
(defun fail (type args &optional (index *index*))
   (make-parser-error :type type :args args :index index))

(defmacro on-success (exp &rest body)
   (with-gensyms (r)
      `(let ((,r ,exp)) 
         (if (error? ,r)
             ,r
             (progn ,@body)))))

(defmacro on-failure (exp &rest body)
   (with-gensyms (r)
      `(let ((,r ,exp)) 
         (if (error? ,r)
             (progn ,@body)
             ,r))))

(defun desugar (exp)
   (cond ((stringp exp) `(str ,exp))
         ((symbolp exp) `(,exp))
         ((numberp exp) `(str ,(string (code-char exp))))
         ((characterp exp) `(chr ,exp))
         (t exp)))

(defun handle-result (r)
   (on-failure r
       (error "parsing error at position ~a, error ~s"
         (error-index r)
         (list (error-type r) (error-args r)))))

(defmacro parse (src form)
   `(let ((*source* (make-source :value ,src))
          (*index* 0))
          (handle-result (funcall ,form))))

;; (declaim (inline test-remaining))
(defun test-remaining (n)
   (if (stringp (source-value *source*))
       (>= (length (source-value *source*)) (+ *index* n))
       t))

;; choice  ~ in the matching operation, returns the first form that consumes input
;; any     ~ matches zero or more of the next form
;; many    ~ matches one or more of the next form
;; one-of  ~ matches one character from the given string
;; none-of ~ matches only if none of the chars in the string match
;; times   ~ parses x occurances of y
;; try     ~ returns a parsing result, no effect on the normal parser stack
;; except  ~ matches one character from the string if the form does not match
;; ret     ~ removes the current parsing result and tries to parse x instead
;; optional~ tries x, if it fails, parse y
;; seq     ~ matches each form sequentially, returns a list of forms or nil
;; par     ~ re-enter parse mode from normal function, internal use only!
;; str     ~ explicitly matches a string
;; sep     ~ seperates by repeated matching. mostly magic

(defun chr (c)
   (declare (type character c))
   (lambda ()
      (if (test-remaining 1) 
          (if (eql (get-character *index*) c)
              (pass c 1)
              (fail (string c) nil))
          (fail (string c) nil))))

(defun str (s)
   (declare (type string s))
   (lambda () 
      (iter (for c in-string s)
            (for r = (funcall (chr c)))
            (on-failure r (leave r))
            (finally (return s)))))

(defun one-of (s)
   (lambda () 
      (iter (for c in-string s)
            (on-success (funcall (try (chr c)))
               (leave (funcall (chr c))))
            (finally (return (fail s nil))))))

;; TODO find a way to make errors accept negated logic
(defun none-of (s)
   (lambda () 
      (iter (for c in-string s)
            (on-success (funcall (try (chr c)))
               (leave (fail s nil)))
            (finally (return c)))))

(defun try (p)
   (lambda () 
      (let ((*index* *index*))
        (funcall p))))

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
