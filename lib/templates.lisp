(in-package :eudoxia.www)

;;; Templates

(register-emb "layout" #p"templates/layout.tmpl")
(register-emb "post" #p"templates/post.tmpl")

(defun render-layout (title content)
  (execute-emb "layout" :env (list :title title
                                   :content content)))

(defun render-post (title tags content)
  (render-layout title
    (execute-emb "post" :env (list :title title :content content))))

;;; Wax template tags

(with-backend :html
  (defrule page () (a tree)
    (render-layout (gethash "title" a) (emit tree))))
