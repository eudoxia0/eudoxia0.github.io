(ql:quickload (list :cl-markup :scribble))
(defpackage eudoxia.www
  (:use :cl :cl-markup))
(in-package :eudoxia.www)
(scribble:enable-scribble-syntax)

(defun scribble:pp (text)
  text)

(defmacro layout (title &rest content)
  `(html5
    (:head
     (:meta :charset "utf-8")
     (:link :href "/static/css/style.css" :rel "stylesheet")
     (:title ,title))
    (:body
     (:section :id "content"
       ,@content))))

(defmacro base-page (title &rest content)
  `(layout ,title ,@content))

(defmacro post (title &rest content)
  `(layout ,title ,@content))

(defparameter +build-path+
  (merge-pathnames #p"build/"))

(defun move-file (pathname output-type content)
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

(defun read-file (path)
  (declare (pathname path))
  (with-open-file (stream path)
    (flet ((read-stream ()
             (read stream nil :my-eof)))
      (let ((forms (list))
            (node (read-stream)))
        (loop while (not (eq node :my-eof)) do
          (push node forms)
          (setf node (read-stream)))
        forms))))

(defmacro page (pathname)
  `(move-file ,pathname "html" ,@(read-file pathname)))
