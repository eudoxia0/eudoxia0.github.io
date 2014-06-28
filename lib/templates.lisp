(in-package :eudoxia.www)

;;; Templates

(register-emb "layout" #p"templates/layout.tmpl")
(register-emb "post" #p"templates/post.tmpl")

(defun render-layout (title content)
  (execute-emb "layout" :env (list :title title
                                   :content content)))

(defun render-post (title tags content)
  (render-layout title
    (execute-emb "post" :env (list :title title))))

;;; Wax template tags

(defun e (text) (wax.emitter:emit text :html))

(with-backend :html
  (defrule layout () ((&rest title) &rest content)
    (render-layout (e title) (e content)))

  (defrule post () ((&rest title) (tags) &rest content)
    (render-post (e title) tags (e content))))
