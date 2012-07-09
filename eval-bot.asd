(defsystem :eval-bot
  :depends-on (:bordeaux-threads :trivial-irc :alexandria
                                 :split-sequence)
  :components
  ((:file "clbot-common")
   (:file "sandbox-impl" :depends-on ("clbot-common" "filesystem"))
   (:file "sandbox-cl" :depends-on ("sandbox-impl"))
   (:file "sandbox-fs" :depends-on ("clbot-common" "filesystem" "sandbox-impl"))
   (:file "clhs-url")
   (:file "eval-bot" :depends-on ("clbot-common" "sandbox-impl" "clhs-url"))
   (:file "filesystem" :depends-on ("clbot-common"))))
