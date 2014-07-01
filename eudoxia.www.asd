(asdf:defsystem eudoxia.www
  :author "Fernando Borretti"
  :depends-on (:wax :cl-emb)
  :components ((:module "lib"
                :components
                ((:file "package")
                 (:file "files")
                 (:file "templates")))
               (:file "site")))
