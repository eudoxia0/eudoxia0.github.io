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

;;; Wax template tags

