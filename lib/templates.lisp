(in-package :eudoxia.www)

(setf *auto-escape* nil)

;;; Templates

(defmacro layout (title content)
  `(html5
    (:head
     (:meta :charset "utf-8")
     (:link :href "/static/css/style.css" :rel "stylesheet")
     (:title ,title))
    (:body
     (:section :id "content"
       ,content))))

(defmacro post (title tags content)
  `(layout
    ,title
    (:article
     (:div :class "tags" ,tags)
     ,content)))

;;; Wax template tags

(defun e (text) (wax.emitter:emit text :html))

(with-backend :html
  (defrule layout () ((&rest title) &rest content)
    (layout (e title) (e content)))

  (defrule post () ((&rest title) (tags) &rest content)
    (post (e title) tags (e content))))
