;;;; Eval-bot --- An IRC bot for CL eval and help

;; Copyright (C) 2012 Teemu Likonen <tlikonen@iki.fi>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public
;; License along with this program. If not, see
;; <http://www.gnu.org/licenses/>.

(cl:defpackage #:sandbox-fs)
(cl:in-package #:sandbox-fs)

(cl:declaim (cl:optimize (cl:safety 3)))

(cl:loop
 :for func :in '()           ;FIXME: Add function's symbols to the list.
 :do
 (cl:setf (cl:get func :sandbox-locked) cl:t)
 (cl:export (cl:list func))
 (cl:eval
  `(cl:defun ,func (cl:&rest args)
     (cl:signal
      'clbot-common:filesystem-command
      :fs-cmd (cl:find-symbol (cl:symbol-name func)
                              "FILESYSTEM")
      :fs-args args
      :cd (cl:get (cl:find-symbol "CD" sandbox-impl:*sandbox*)
                  :sandbox-cd)))))
