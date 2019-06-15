(defsystem "eval-bot"
  :description "An IRC bot for Common Lisp code evaluation"
  :author "Teemu Likonen <tlikonen@iki.fi>"
  :licence "GNU Affero General Public License version 3"
  :depends-on ("bordeaux-threads"
               "trivial-irc" "alexandria" "split-sequence" "babel")
  :components
  ((:file "sandbox-impl" :depends-on ("common"))
   (:file "sandbox-extra" :depends-on ("common" "sandbox-impl" "sandbox-cl"))
   (:file "sandbox-cl" :depends-on ("sandbox-impl"))
   (:file "common")
   (:file "eval-bot" :depends-on ("common" "sandbox-impl"))))
