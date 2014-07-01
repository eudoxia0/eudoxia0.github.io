(in-package :eudoxia.www)

(defparameter +base-path+
  (asdf:component-pathname (asdf:find-system :eudoxia.www)))

(defun path (path)
  (merge-pathnames path +base-path+))

(defparameter +build-path+
  #p"build/")

(defun move-file (pathname output-type content)
  (declare (pathname pathname)
           (string output-type)
           (string content))
  (let* ((content-dir (make-pathname
                       :directory (pathname-directory pathname)))
         (build-dir (merge-pathnames content-dir +build-path+))
         (output-path (make-pathname
                       :directory (pathname-directory build-dir)
                       :name (pathname-name pathname)
                       :type output-type)))
    (ensure-directories-exist build-dir)
    (with-open-file (stream (merge-pathnames output-path +base-path+)
                            :direction :output
                            :if-exists :supersede)
      (write-string content stream))))

(defun page (pathname)
  (declare (pathname pathname))
  (move-file pathname
             "html"
             (process (merge-pathnames pathname +base-path+))))
