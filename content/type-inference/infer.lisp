; An environment is an object that associates variables to values.  They are
; represented by association lists.

(defun env-empty () ; returns an empty environment
  '())

(defun env-update (var val env) ; returns a copy of 'env' but with 'var'
  (cons (cons var val) env))     ; associated to 'val'

(defun env-join (env1 env2) ; return a new environment containing the bindings
  (append env1 env2))        ; in env1 and env2 (env1 has precedence)

(defun env-bound-p (var env) ; returns true if the variable is bound in 'env'
  (if (assoc var env) t))

(defun env-value (var env) ; returns the value associated to 'var' in 'env'
  (cdr (assoc var env)))(defun env-empty () ; returns an empty environment
  '())

(defun env-update (var val env) ; returns a copy of 'env' but with 'var'
  (cons (cons var val) env))     ; associated to 'val'

(defun env-join (env1 env2) ; return a new environment containing the bindings
  (append env1 env2))        ; in env1 and env2 (env1 has precedence)

(defun env-bound-p (var env) ; returns true if the variable is bound in 'env'
  (if (assoc var env) t))

(defun env-value (var env) ; returns the value associated to 'var' in 'env'
  (cdr (assoc var env)))

;------------------------------------------------------------------------------
;
; Variables are symbols starting with a "?" (e.g. ?a)

(defparameter *variable-count* 0)

(defun new-variable ()
  (incf *variable-count*)
  (intern (format nil "?~A" *variable-count*)))

(defun variablep (x)
  (and (symbolp x) (char= (elt (symbol-name x) 0) #\?)))

(defun variable< (x y)
  "Check if variable X was generated before Y."
  (string< (symbol-name x) (symbol-name y)))

;;; Unification

(defun get-val (var env)
  "Get the value of a variable"
  (if (variablep var)
      (env-value var env)
      var))

(defun unify (x y env)
  "return the values that must be substituted for the variables so that x and y
unify"
  (let ((x* (get-val x env))
        (y* (get-val y env)))
    (cond ((equal x* y*)
           env)
          ((and (variablep x*)
                (or (not (variablep y*))
                    (variable< y* x*)))
           (env-update x* y* env))
          ((variablep y*)
           (env-update y* x* env))
          ((and (consp x*) (consp y*))
           (uni (rest x*) (rest y*)
                (uni (first x*) (first y*) env)))
          (t
           (error "Failed to unify")))))

(defun subst-vars (x env)
  "return x, where each variable is substituted for the value it is associated
to in env"
  (cond ((variablep x)
         (let ((y (get-val x env)))
           (if (variablep y)
               y
               (subst-vars y env))))
        ((consp x)
         (cons (subst-vars (first x) env)
               (subst-vars (rest x) env)))
        (t
         x)))

;; Generics or something

(defun genericp (var prefix)
  "is the variable 'var' generic in the 'prefix'"
  (cond ((null prefix)
         t)
        ((and (equal (cadar prefix) var)
              (not (equal (caar prefix) 'let)))
         nil)))

(defun instance (x prefix)
  "generate an instance of x with new variables in place of the generic
variables in prefix"
  (labels ((new (x env success)
             (cond ((and (variablep x) (genericp x prefix))
                    (if (env-bound-p x env)
                        (funcall success (env-value x env) env)
                        (let ((var (new-variable)))
                          (funcall success var (env-update x var env)))))
                   ((consp x)
                    (new (first x) env
                         (lambda (a env)
                           (new (rest x) env
                                (lambda (b env)
                                  (funcall success (cons a b) env))))))
                   (t
                    (funcall success x env)))))
    (new x (env-empty) (lambda (a env)
                         (declare (ignore env))
                         a))))

;; Types

(defun constant-type (x)
  (cond ((integerp x)
         :int)
        ((floatp x)
         :float)
        ((or (eql x 'true) (eql x 'false))
         :bool)
        (t
         (error "Unknown constant type."))))

;; The type inference machinery

(defparameter +global-var-types+
  '((+  . (-> num num num))
    (-  . (-> num num num))
    (*  . (-> num num num))
    (/  . (-> num num num))
    (<  . (-> num num bool))
    (<= . (-> num num bool))
    (=  . (-> num num bool))
    (>= . (-> num num bool))))

(defun infer (f)
  (let ((e (env-empty)))
    (labels
        ;; Algorithm J
        ((j (p f)
           (cond ((and (symbolp f)
                       (not (eql f 'true))
                       (not (eql f 'false)))
                  (if (env-bound-p f p)
                      (let* ((x (env-value f p))
                             (kind (first x))
                             (type (cadr x)))
                        (if (eq kind :let)
                            (instance type p)
                            type))
                      (instance (cdr (assoc f +global-var-types+))
                                (env-empty))))
                 ((not (consp f))
                  (instance (constant-type f) (env-empty)))
                 ((eq (first f) 'if)
                  (let ((cond (j p (second f)))
                        (true-branch (j p (third f)))
                        (false-branch (j p (fourth f))))
                    (setf e (unify true-branch
                                   false-branch
                                   (unify cond :bool e)))
                    true-branch))
                 ((eq (first f) 'let)
                  (j (env-join (mapcar #'(lambda (x)
                                           (list (first x) 'let (j p (cadr x))))
                                       (cadr f))
                               p)
                     (caaddr f)))
                 (t
                  (let ((result (new-variable))
                        (oper (j p (first f)))
                        (args (mapcar #'(lambda (x)
                                          (j p x))
                                      (rest f))))
                    (setf e (unify oper
                                   (cons '-> (append args (list result)))
                                   e))
                    result)))))
      (let ((term (j (env-empty) f)))
        (subst-vars term e)))))
