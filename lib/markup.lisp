(ql:quickload :cl-markup)
(defpackage eudoxia.www
  (:use :cl :cl-markup))
(in-package :eudoxia.www)

(defun read-bracket-string (stream char)
  (declare (ignore char))
  (coerce (loop for ch = (read-char stream nil #\Space) until (char= ch #\])
                collect ch)
          'string))

(set-macro-character #\[ #'read-bracket-string)

(defmacro layout (title &rest content)
  `(html5
    (:head
     (:title ,title))
    (:body
     (:section :id "content"
       ,@content))))

(defmacro post (title &rest text)
  `(layout ,title ,@text))

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
