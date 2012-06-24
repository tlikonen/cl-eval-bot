(defsystem :eval-bot
  :depends-on (:bordeaux-threads :trivial-irc :alexandria
                                 :split-sequence)
  :components
  ((:file "sandbox-impl")
   (:file "sandbox-cl" :depends-on ("sandbox-impl"))
   (:file "clhs-url")
   (:file "clbot-common")
   (:file "eval-bot" :depends-on ("clbot-common" "sandbox-impl" "clhs-url"))
   (:file "filesystem" :depends-on ("clbot-common"))))
