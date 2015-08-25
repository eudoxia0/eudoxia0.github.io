(ql:quickload (list :dbi :yason :local-time :group-by))

(defvar *connection*
  (dbi:connect :sqlite3
               :database-name (merge-pathnames #p".newsbeuter/cache.db"
                                               (user-homedir-pathname))))

(let* ((output (merge-pathnames #p"lisp-github.json"
                                (user-homedir-pathname)))
       (query (dbi:prepare *connection*
                           "SELECT pubDate FROM rss_item WHERE feedurl = ?"))
       (results (mapcar #'(lambda (result)
                            (local-time:unix-to-timestamp
                             (getf result :|pubDate|)))
                        (dbi:fetch-all
                         (dbi:execute query "http://planet.lisp.org/github.atom")))))
  (with-open-file (stream output
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (yason:encode
     (mapcar #'(lambda (group)
                 (list
                  (local-time:format-timestring nil
                                                (first group)
                                                :format (list :year #\- :month #\- :day))
                  (1- (length group))))
             (group-by:group-by-repeated results
                                         :keys (list #'identity)
                                         :tests (list #'(lambda (a b)
                                                          (= (local-time:day-of a)
                                                             (local-time:day-of b))))))
     stream)))
