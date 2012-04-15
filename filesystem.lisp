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

(defpackage #:filesystem
  (:use #:cl)
  (:shadow #:directory)
  (:import-from #:general
                #:queue #:queue-add #:queue-pop #:queue-clear
                #:queue-length))

(in-package #:filesystem)

(defclass file ()
  ((id :reader id :initarg :id)
   (changed :accessor changed :initform t)
   (lock :reader lock :initform (bt:make-lock "file"))))

(defclass directory (file)
  ((owners :accessor owners :initarg :owners :initform nil)
   (writers :accessor writers :initarg :writers :initform nil)
   (files :reader files :initform (make-hash-table :test #'equal))
   (parent :accessor parent :initarg :parent)))

(defclass regular-file (file)
  ((content :accessor content :initform nil)))

(defclass pointer () ((id :reader id :initarg :id)))
(defclass directory-ptr (pointer) nil)
(defclass regular-file-ptr (pointer) nil)

(define-condition file-system-error (error)
  ((message :initarg :message))
  (:report (lambda (c s) (princ (slot-value c 'message) s))))

(defvar *root-owners* nil)
(defvar *root* (make-instance 'directory :id 0
                              :owners *root-owners* :parent nil))
(defvar *fsdata* (make-hash-table))

(defgeneric follow (object))

(defmethod follow ((ptr directory-ptr))
  (gethash (id ptr) *fsdata*))

(defmethod follow ((ptr regular-file-ptr))
  (gethash (id ptr) *fsdata*))

(defun path-to-string (path)
  (format nil "~{~A~^/~}"
          (mapcar (lambda (item)
                    (if (stringp item)
                        item
                        (ecase item
                          (:root "") (:parent "..") (:current "."))))
                  path)))

(defgeneric file-print-name (name object))

(defmethod file-print-name ((name string) (type directory-ptr))
  (format nil "~S/" name))

(defmethod file-print-name ((name string) (type regular-file-ptr))
  (format nil "~S" name))

(defun allocate-id (id object)
  (setf (gethash id *fsdata*) object))

(defun release-id (id)
  (remhash id *fsdata*)
  (queue-add (gethash :free-ids *fsdata*) id))

(defun init-fsdata ()
  (setf (gethash :id-counter *fsdata*) 0
        (gethash :free-ids *fsdata*) (make-instance 'queue)
        (gethash :root-id *fsdata*) (id *root*))
  (allocate-id (id *root*) *root*))

(defun get-root-dir ()
  (gethash (gethash :root-id *fsdata*) *fsdata*))

(defun next-id ()
  (let ((free (queue-pop (gethash :free-ids *fsdata*))))
    (or free (incf (gethash :id-counter *fsdata*)))))

(defun valid-name-p (object)
  (and (stringp object)
       (plusp (length object))
       (not (find #\/ object))
       (every #'graphic-char-p object)))

(defun parse-path-string (string)
  (let ((path (split-sequence:split-sequence #\/ string)))
    (when (string= "" (first path))
      (setf (first path) :root))
    (setf path (delete "" path :test #'string=)
          path (nsubstitute :parent ".." path :test #'string=)
          path (nsubstitute :current "." path :test #'string=))))

(defun valid-irc-user-p (user)
  (let ((exc (position #\! user))
        (at (position #\@ user)))
    (and exc at
         (> at exc)
         (find-if #'alphanumericp (subseq user 0 exc))
         (find-if #'alphanumericp (subseq user exc at))
         (find-if #'alphanumericp (subseq user at))
         user)))

(defun normalize-user (user)
  (setf user (string-trim " " (string-downcase user))))

(defun default-irc-user-pattern (user)
  (setf user (normalize-user user))
  (let ((pos (position #\! user)))
    (concatenate 'string "*" (subseq user pos))))

(defun insert-before-nth (nth new-item list)
  (cond ((null list) (list new-item))
        ((minusp nth) (cons new-item list))
        (t (loop :for (item . rest) :on list
                 :for i :upfrom 0
                 :if (= i nth) :collect new-item
                 :collect item
                 :if (and (null rest) (> nth i)) :collect new-item))))

(defun remove-nth (nth list)
  (loop :for item :in list
        :for n :upfrom 0
        :unless (= n nth) :collect item))

(defun error-file-exists (name)
  (error 'file-system-error
         :message (format nil "File ~A already exists." name)))

(defun error-not-regular-file (name)
  (error 'file-system-error
         :message (format nil "~A is not a regular file." name)))

(defun error-file-not-found (name)
  (error 'file-system-error
         :message (format nil "File not found: ~A" name)))

(defvar *max-file-versions* 5)
(defvar *max-file-lines* 5)

(defun add-to-regular-file (file line &optional nth)
  (assert (typep file 'regular-file))
  (with-slots (lock content changed) file
    (let ((new (if nth
                   (insert-before-nth nth line (first content))
                   (append (first content) (list line)))))
      (when (> (length new) *max-file-lines*)
        (error 'file-system-error
               :message "The file is full (max ~D lines)."
               *max-file-lines*))
      (bt:with-lock-held (lock)
        (setf content (cons new content)
              content (subseq content 0 (min *max-file-versions*
                                             (length content)))
              changed t)))))

(defun delete-from-regular-file (file nth)
  (assert (typep file 'regular-file))
  (with-slots (lock content changed) file
    (bt:with-lock-held (lock)
      (let ((new (remove-nth nth (first content))))
        (setf content (cons new content)
              content (subseq content 0 (min *max-file-versions*
                                             (length content)))
              changed t)))))

(defun revert-to-version (file version)
  (assert (typep file 'regular-file))
  (with-slots (lock content changed) file
    (unless (<= 0 version (1- (length content)))
      (error 'file-system-error
             :message (format nil "Available versions 0-~D."
                              (length content))))
    (bt:with-lock-held (lock)
      (setf content (cons (nth version content) content)
            content (subseq content 0 (min *max-file-versions*
                                           (length content)))
            changed t))))

(defun read-regular-file (file &optional (version 0))
  (assert (typep file 'regular-file))
  (if (<= 0 version (1- (length (content file))))
      (nth version (content file))
      (error 'file-system-error
             :message (format nil "Available versions 0-~D."
                              (1- (length (content file)))))))

(defun delete-file-permanently (dir name)
  ;; This does not check subdirectories.
  (let ((ptr (gethash name (files dir))))
    (if (typep ptr 'pointer)
        (progn
          (bt:with-lock-held ((lock dir))
            (remhash name (files dir)))
          (release-id (id ptr)))
        (error-file-not-found name))))

;; TODO: copy-file, move-file

(defun public-dir-p (dir)
  (eql :public (owners dir)))

(defun make-dir-public (dir)
  (bt:with-lock-held ((lock dir))
    (setf (owners dir) :public
          (changed dir) t)))

(defun add-to-owners (dir user)
  (with-slots (owners lock changed) dir
    (bt:with-lock-held (lock)
      (unless (listp owners)
        ;; Probably :PUBLIC
        (setf owners nil))
      (pushnew user owners :test #'string-equal)
      (setf changed t))))

(defun delete-from-owners (dir nth)
  (with-slots (owners lock changed) dir
    (bt:with-lock-held (lock)
      (setf owners (remove-nth nth owners)
            changed t))))

(defun add-to-writers (dir user)
  (with-slots (writers lock changed) dir
    (bt:with-lock-held (lock)
      (pushnew user writers :test #'string-equal)
      (setf changed t))))

(defun delete-from-writers (dir nth)
  (with-slots (writers lock changed) dir
    (bt:with-lock-held (lock)
      (setf writers (remove-nth nth writers)
            changed t))))

(defun match-user (pattern user)
  (let* ((patterns (split-sequence:split-sequence #\* pattern))
         (first (first patterns))
         middle last)

    (loop :for (item . rest) :on (rest patterns)
          :if (null rest) :do (setf last item)
          :else :collect item :into mid
          :finally (setf middle mid))

    (and (<= (length first) (length user))
         (string-equal first user :end2 (length first))

         (or (not last)
             (and (<= (length last) (length (subseq user (length first))))
                  (string-equal last user :start2 (- (length user)
                                                     (length last)))))
         (or (not middle)
             (loop :with mstr := (subseq user (length first)
                                         (- (length user)
                                            (length (or last ""))))
                   :with pos := 0
                   :for pat :in middle
                   :for match := (search pat mstr :start2 pos
                                         :test #'string-equal)
                   :if match :do (setf pos (+ match (length pat)))
                   :else :return nil
                   :finally (return t)))

         user)))

(defun ownerp (dir user)
  (labels ((ownp (dir)
             (cond ((and (listp (owners dir))
                         (some (lambda (pat)
                                 (match-user pat user))
                               (owners dir)))
                    (return-from ownerp t))
                   ((parent dir)
                    (ownp (follow (parent dir))))
                   (t (return-from ownerp nil)))))
    (bt:with-lock-held ((lock dir))
      (ownp dir))))

(defun write-access-p (dir user)
  (or (public-dir-p dir)
      (some (lambda (pat)
              (match-user pat user))
            (writers dir))
      (ownerp dir user)))

(defun mkdir-access-p (dir user)
  (or (public-dir-p dir)
      (ownerp dir user)))

(defun list-files (dir)
  (loop :for name :being :each :hash-key :in (files dir)
        :using (hash-value val)
        :collect (file-print-name name val) :into all-files
        :finally (return (sort all-files #'string-lessp))))

(defun create-directory (dir name)
  (if (gethash name (files dir))
      (error-file-exists name)
      (with-slots (files id changed lock) dir
        (let ((new (make-instance 'directory
                                  :parent (make-instance 'directory-ptr :id id)
                                  :id (next-id))))
          (bt:with-lock-held (lock)
            (setf (gethash name files) (make-instance 'directory-ptr
                                                      :id (id new))
                  changed t))
          (allocate-id (id new) new)))))

(defun create-regular-file (dir name)
  (if (gethash name (files dir))
      (error-file-exists name)
      (with-slots (files changed lock) dir
        (let ((file (make-instance 'regular-file :id (next-id))))
          (bt:with-lock-held (lock)
            (setf (gethash name files) (make-instance 'regular-file-ptr
                                                      :id (id file))
                  changed t))
          (allocate-id (id file) file)))))

(defun find-target (dir path)
  (cond
    ((null path) dir)
    ((eql :root (first path))
     (find-target (get-root-dir) (rest path)))
    ((eql :parent (first path))
     (let ((parent-ptr (parent dir)))
       (if (typep parent-ptr 'directory-ptr)
           (find-target (follow parent-ptr) (rest path))
           (error 'file-system-error :message "Parent directory not found."))))
    ((eql :current (first path))
     (find-target dir (rest path)))
    ((stringp (first path))
     (let ((ptr (gethash (first path) (files dir))))
       (if ptr
           (find-target (follow ptr) (rest path))
           (error 'file-system-error
                  :message (format nil "Directory not found: ~A"
                                   (first path))))))
    (t (error 'file-system-error
              :message (format nil "Unknown path component: ~A"
                               (first path))))))

(defun map-subdirs (function dir)
  (map nil (lambda (subdir)
             (funcall function subdir)
             (map-subdirs function subdir))
       (loop :for value :being :each :hash-value :in (files dir)
             :if (typep value 'directory-ptr) :collect (follow value))))

(defmacro do-subdirs ((var dir &optional result-form) &body body)
  `(progn (map-subdirs (lambda (,var) ,@body) ,dir)
          ,result-form))
