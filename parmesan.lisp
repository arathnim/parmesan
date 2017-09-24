;; (author *this-code*) => ("Dylan Ball" "Arathnim@gmail.com")

(proclaim '(optimize (speed 0) (safety 3) (debug 3) (space 0)))

(ql:quickload '(alexandria iterate wernicke) :silent t)

(defpackage parmesan
   (:use cl alexandria iterate)
   (:import-from wernicke "ONE-OF" "STR" "CHR" "NONE-OF" "CHOICE"
       "RANGE" "TRY" "OPTIONAL" "TIMES" "DEFINE-PARSER" "RUN-PARSER" "ERROR?")
   (:export "DEFPARSER" "PARSE" "ONE-OF" "NONE-OF" 
      "ANY" "MANY" "SEQ" "CHOICE" "RANGE" "TRY" "OPTIONAL" "TIMES"))

(in-package parmesan)

;; compressing

(define-parser many (p)
   (let ((result (run-parser (wernicke:many p))))
      (if (and (not (error? result)) (every #'characterp result)) 
          (coerce result 'string) 
          result)))

(define-parser any (p)
   (let ((result (run-parser (wernicke:any p))))
      (if (and (not (error? result)) (every #'characterp result)) 
          (coerce result 'string) 
          result)))

(define-parser seq (&rest p)
   (let ((result (run-parser (apply #'wernicke:seq p))))
      (if (and (not (error? result)) (eql (length p) 1)) 
          (car result) 
          result)))

;; shorthand for characters and strings

(defmacro parse (string form)
   `(let ((wernicke:*run-hooks* 
            (list (lambda (p) (if (stringp p) (wernicke::str p) p))
                  (lambda (p) (if (characterp p) (wernicke::chr p) p)))))
         (wernicke:parse ,string ,form)))

;; variable assignment

(defun symbol-eq (x y)
   (and (symbolp x) (symbolp y) (string= (string x) (string y))))

(defun generate-single-binding (var form)
  `(lambda () (setf ,var (run-parser ,form)) ,var))

(defun generate-multiple-binding (vars form)
   (with-gensyms (parse-result)
     `(lambda ()
        (let ((,parse-result (run-parser ,form))) 
          (if (listp ,parse-result) 
              (progn
                (when (not (eql (length ,parse-result) ,(length vars)))
                      (error "result of ~a is the wrong length for the variable list ~a" ',form '(,@vars)))
                ,@(iter (for x in vars)
                        (for y upfrom 0) 
                        (collect `(setf ,x (nth ,y ,parse-result)))))
              ,parse-result)))))

(defun extract-binding (exp)
   (when (and (listp exp) (member '<- exp :test #'symbol-eq))
         (subseq exp 0 (position '<- exp :test #'symbol-eq))))

(defun without-binding (exp)
   (car (subseq exp (+ 1 (position '<- exp :test #'symbol-eq)))))

(defun transform-binds (forms)
   (iter (for exp in forms)
         (for vars = (extract-binding exp))
         (when vars (appending vars into free-variables))
         (collect
            (if vars
               (if (> (length vars) 1)
                   (generate-multiple-binding vars (without-binding exp))
                   (generate-single-binding (car vars) (without-binding exp)))
               exp)
            into new-forms)
         (finally (return (list free-variables new-forms)))))

(defun binding-seq (forms)
   (let* ((result (transform-binds forms))
          (vars (first result))
          (new-forms (second result)))
          (if vars
             (with-gensyms (r)
               `(let (,@vars) 
                 (let ((,r (run-parser (seq ,@(butlast new-forms)))))
                       (if (error? ,r)
                           ,r
                           ,(car (last new-forms))))))
             `(wernicke::run-parser (seq ,@new-forms)))))

;; no-args calling shorthand

(defmacro defparser (name ll &rest body)
   `(progn (define-parser ,name ,ll ,(binding-seq body))
          ,(unless ll `(defparameter ,name (funcall (function ,name))))))

