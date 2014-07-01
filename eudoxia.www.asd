(asdf:defsystem eudoxia.www
  :author "Fernando Borretti"
  :depends-on (:wax :cl-emb)
  :serial t
  :components ((:module "lib"
                :serial t
                :components
                ((:file "package")
                 (:file "files")
                 (:file "templates")))
               (:file "site")))
