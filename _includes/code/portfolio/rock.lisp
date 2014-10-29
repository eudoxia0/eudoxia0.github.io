;; We define an environment for the 'rock' ASDF system
(defenv :rock
  ;; These are our dependencies
  :assets ((:jquery :2.1.1)
           (:bootstrap :3.2.0)
           (:highlight-lisp :0.1))
  :bundles ((:js
             ;; This is a JS bundle. It compiles the JS files
             ;; of the dependencies below:
             :assets ((:jquery :2.1.1)
                      (:bootstrap :3.2.0)
                      (:highlight-lisp :0.1))
             ;; Our custom JS: assets/js/scripts.js
             :files (list #p"js/scripts.js")
             ;; Combined JS file: assets/build/js/scripts.js
             :destination #p"js/scripts.js")
            (:css
             ;; This is a CSS bundle. Note that we don't
             ;; include jQuery
             :assets ((:bootstrap :3.2.0)
                      (:highlight-lisp :0.1))
             :files (list #p"css/style.css")
             :destination #p"css/style.css")))

;; Download the assets and compile the bundles for this
;; environment. Dependencies are only downloaded when we need them
(build :rock)
