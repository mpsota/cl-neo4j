(in-package #:cl-neo4j)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun locked-value-lock-name (name)
    "Used for locked-values"
    (intern (format nil "~A-LOCK" name) (find-package 'cl-neo4j)))

  (defmacro defvar-locked (name &optional default docstring)
    "Replacement for defvar for thread-locked variables"
    `(progn
       (defvar ,name ,default ,@(when docstring `(,docstring)))
       (defvar ,(locked-value-lock-name name) (bt:make-lock ,(string name)))))

  (defmacro with-locked-var (place &body body)
    `(bt:with-lock-held (,(locked-value-lock-name place))
       ,@body)))

(defun normalize-alist (alist)
  ;; (print alist)
  (mapcar (lambda (el)
            (if (listp (car el))
                (normalize-alist el);; looks wrong...
                (cons (json:lisp-to-camel-case (if (symbolp (car el))
                                                   (symbol-name (car el))
                                                   (car el)))
                      (cdr el))))
          alist))
