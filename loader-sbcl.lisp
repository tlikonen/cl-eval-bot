;;;; Eval-bot loader

(require :asdf)
(require :sb-posix)

(asdf:initialize-output-translations
 (list :output-translations
       :ignore-inherited-configuration
       (list (merge-pathnames "**/*.*")
             (merge-pathnames "build/**/*.*"))))

(asdf:initialize-source-registry
 (list :source-registry
       :ignore-inherited-configuration
       (list :directory (merge-pathnames "src/"))
       (list :tree (merge-pathnames "quicklisp/dists/"))))

(flet ((probe-load (path)
         (when (probe-file path)
           (load path)))
       (funcallstr (string &rest args)
         (apply (read-from-string string) args)))
  (or (probe-load "quicklisp/setup.lisp")
      (let ((url "http://beta.quicklisp.org/quicklisp.lisp")
            (init (nth-value 1 (sb-posix:mkstemp "/tmp/quicklisp-XXXXXX"))))
        (unwind-protect
             (progn
               (sb-ext:run-program "wget" (list "-O" init "--" url)
                                   :search t :output t)
               (when (probe-load init)
                 (funcallstr "quicklisp-quickstart:install"
                             :path "quicklisp/")))
          (delete-file init)))))

(ql:quickload '("swank" "eval-bot"))

(in-package #:eval-bot)

(defparameter *freenode*
  (make-client :server "irc.freenode.net"
               :nickname "clbot"
               :username "clbot"
               :realname "Common Lisp bot"
               :listen-targets nil
               :auto-join nil))

(loop :for port :from 50000 :upto 50050
      :do (handler-case
              (return (swank:create-server :port port :dont-close t))
            (sb-bsd-sockets:address-in-use-error () nil)))
