(defmachine my-machine
  :system (:ubuntu :14.04 :64)
  :memory 1024
  :cpu-count 5
  :ip "192.128.65.20")

;; Bring it up to do some work
(start my-machine)

;; Connect to it using the trivial-ssh library and run a command
(ssh:with-connection (conn "192.128.65.20" (ssh:agent "vagrant"))
  (ssh:with-command (conn iostream "whoami")))

;; Shut it down
(stop my-machine)
