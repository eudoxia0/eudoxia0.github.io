(in-package :eudoxia.www)

;;; Templates

(register-emb "layout" (path #p"templates/layout.tmpl"))
(register-emb "post" (path #p"templates/post.tmpl"))

(defun render-layout (title content)
  (execute-emb "layout" :env (list :title title
                                   :content content)))

(defun render-post (title tags content)
  (render-layout title
    (execute-emb "post" :env (list :title title :content content))))

;;; Wax template tags

(with-backend :html
  (defrule page () (a tree)
    (render-layout (gethash "title" a) (emit tree)))
  (defrule essay () (a tree)
    (let ((title (gethash "title" a))
          (links (pop-by-name tree "links")))
      ;; Emit the links before the rest
      (emit links)
      (print wax.html::*links*)
      (render-post title nil (emit tree)))))
