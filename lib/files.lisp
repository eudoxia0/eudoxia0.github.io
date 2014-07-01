(in-package :eudoxia.www)

(defun path (path)
  (asdf:system-relative-pathname :eudoxia.www path))

(defparameter +build-path+
  (path #p"build/"))

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
    (with-open-file (stream output-path
                            :direction :output
                            :if-exists :supersede)
      (write-string content stream))))

(defun page (pathname)
  (declare (pathname pathname))
  (move-file (path pathname)
             "html"
             (process (path pathname))))
