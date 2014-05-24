(ql:quickload (list :closure-template))

(closure-template:compile-cl-templates #p"templates/templates.tmpl")

(defparameter +build-path+ #p"build/")

(defmacro save (string path)
  `(with-open-file (stream (merge-pathnames
                            ,path
                            +build-path+)
                           :direction :output
                           :if-exists :supersede)
     (write-string ,string stream)))

(save (view:index) #p"index.html")
