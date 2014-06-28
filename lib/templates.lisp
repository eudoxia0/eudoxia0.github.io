(in-package :eudoxia.www)

;;; Templates

(defmacro layout (title &rest content)
  `(html5
    (:head
     (:meta :charset "utf-8")
     (:link :href "/static/css/style.css" :rel "stylesheet")
     (:title ,title))
    (:body
     (:section :id "content"
       ,@content))))

(defmacro post (title tags &rest content)
  `(layout
    ,title
    (:article
     (:div :class "tags" ,tags)
     ,@content)))

;;; Wax template tags

(with-backend :html
  (defrule layout () ((&rest title) &rest content)
    (layout title content))

  (defrule post () ((&rest title) (tags) &rest content)
    (post title tags content)))
