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
  (:use #:cl #:split-sequence)
  (:shadow #:directory)
  (:import-from #:clbot-common
                #:*data-dir*
                #:queue #:queue-add #:queue-pop #:queue-clear
                #:queue-length #:queue-list))

(in-package #:filesystem)

(defclass file ()
  ((id :reader id :initarg :id)
   (changed :accessor changed :initarg :changed :initform t)
   (atime :accessor atime :initform (get-universal-time))
   (lock :reader lock :initform (bt:make-lock "file"))))

(defclass directory (file)
  ((owners :accessor owners :initarg :owners :initform nil)
   (editors :accessor editors :initarg :editors :initform nil)
   (files :reader files :initform (make-hash-table :test #'equal))
   (parent :accessor parent :initarg :parent)))

(defclass regular-file (file)
  ((content :accessor content :initarg :content :initform nil)))

(defclass pointer () ((id :reader id :initarg :id)))
(defclass directory-ptr (pointer) nil)
(defclass regular-file-ptr (pointer) nil)

(define-condition file-system-error (error)
  ((message :initarg :message))
  (:report (lambda (c s) (princ (slot-value c 'message) s))))

(defun make-empty-root-dir ()
  (make-instance 'directory :id 0 :owners nil :parent nil))

(defvar *fsdata* (make-hash-table))

(defvar *fsdata-dir*
  (merge-pathnames (make-pathname :directory '(:relative "fs"))
                   *data-dir*))

(defvar *fsdata-filename* (make-pathname :name "fsdata"))

(defun update-atime (file)
  (setf (atime file) (get-universal-time)))

(defun release-id (id)
  (remhash id *fsdata*)
  (queue-add (gethash :free-ids *fsdata*) id)
  (write-fsdata-to-disk))


;;; Disk read and write

(defun directory-id-component (id)
  (let* ((unit 1000)
         (q (truncate id unit)))
    (make-pathname
     :directory (list :relative (format nil "~D-~D" (* q unit)
                                        (+ (* q unit) (1- unit)))))))

(defun make-id-file-pathname (id)
  (make-pathname :name (format nil "~D" id)
                 :defaults (merge-pathnames (directory-id-component id)
                                            *fsdata-dir*)))

(defun write-file-to-disk (file)
  (let ((path (make-id-file-pathname (id file))))
    (ensure-directories-exist path)
    (bt:with-lock-held ((lock file))
      (with-open-file (s path :direction :output
                         :if-exists :supersede
                         :element-type 'character)
        (print-file file s))
      (setf (changed file) nil))
    path))

(defun write-changed-files-to-disk ()
  (loop :for file :being :each :hash-value :in *fsdata*
        :if (and (typep file 'file) (changed file))
        :do (write-file-to-disk file)))

(defun read-regular-file-from-stream-1 (stream)
  ;; Slots in the stream: id, content.
  (make-instance 'regular-file :id (read stream) :content (read stream)
                 :changed nil))

(defun read-directory-from-stream-1 (stream)
  ;; Slots in the stream: id, parent id, owners, editors, files.
  (let ((dir (make-instance
              'directory
              :id (read stream)
              :parent (make-instance 'directory-ptr :id (read stream))
              :owners (read stream)
              :editors (read stream)
              :changed nil)))
    (loop :for (name type id) :in (read stream)
          :do (setf (gethash name (files dir))
                    (ecase type
                      (:r (make-instance 'regular-file-ptr :id id))
                      (:d (make-instance 'directory-ptr :id id)))))
    dir))

(defun read-file-from-disk (id)
  (let ((path (make-id-file-pathname id)))
    (with-open-file (s path :direction :input
                       :element-type 'character
                       :if-does-not-exist :error)
      (with-standard-io-syntax
        (let* ((*read-eval* nil)
               (filetype (read s))
               (version (read s)))
          (ecase filetype
            (:regular-file
             (ecase version
               (1 (read-regular-file-from-stream-1 s))))
            (:directory
             (ecase version
               (1 (read-directory-from-stream-1 s))))))))))

(defun write-fsdata-to-disk ()
  (let ((path (merge-pathnames *fsdata-filename* *fsdata-dir*)))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output
                       :element-type 'character
                       :if-exists :supersede)
      (with-standard-io-syntax
        ;; file-format-version, id-counter, root-id, free-ids (list)
        (format s "~S ~S ~S~%~S~%"
                1
                (gethash :id-counter *fsdata*)
                (gethash :root-id *fsdata*)
                (queue-list (gethash :free-ids *fsdata*)))))))

(defun read-fsdata-from-disk ()
  (let ((path (merge-pathnames *fsdata-filename* *fsdata-dir*)))
    (with-open-file (s path :direction :input
                       :element-type 'character
                       :if-does-not-exist :error)
      (with-standard-io-syntax
        (let* ((*read-eval* nil)
               (version (read s)))
          (ecase version
            (1 ;; id-counter, root-id, free-ids (list)
             (clrhash *fsdata*)
             (setf (gethash :id-counter *fsdata*) (read s)
                   (gethash :root-id *fsdata*) (read s)
                   (gethash :free-ids *fsdata*) (make-instance 'queue))
             (loop :for id :in (read s)
                   :do (queue-add (gethash :free-ids *fsdata*) id)))))))))

(defun from-ptr-to-target (ptr)
  (or (gethash (id ptr) *fsdata*)
      (setf (gethash (id ptr) *fsdata*)
            (read-file-from-disk (id ptr)))))

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

(defun init-empty-fsdata ()
  (clrhash *fsdata*)
  (setf (gethash :id-counter *fsdata*) 0
        (gethash :free-ids *fsdata*) (make-instance 'queue)
        (gethash :root-id *fsdata*) 0
        (gethash 0 *fsdata*) (make-empty-root-dir)))

(defun get-root-dir ()
  (let ((id (gethash :root-id *fsdata*)))
    (or (gethash id *fsdata*)
        (setf (gethash id *fsdata*) (read-file-from-disk id)))))

(defun next-id ()
  (let ((free (queue-pop (gethash :free-ids *fsdata*))))
    (prog1 (or free (incf (gethash :id-counter *fsdata*)))
      (write-fsdata-to-disk))))

(defun valid-name-p (object)
  (and (stringp object)
       (plusp (length object))
       (not (find #\/ object))
       (every #'graphic-char-p object)))

(defun parse-path-string (string)
  (let ((path (split-sequence #\/ string)))
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

;;; Modify, create, move etc. files

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
              changed t)
        (update-atime file)
        content))))

(defun delete-from-regular-file (file nth)
  (assert (typep file 'regular-file))
  (with-slots (lock content changed) file
    (bt:with-lock-held (lock)
      (let ((new (remove-nth nth (first content))))
        (setf content (cons new content)
              content (subseq content 0 (min *max-file-versions*
                                             (length content)))
              changed t)
        (update-atime file)
        content))))

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
            changed t)
      (update-atime file)
      content)))

(defun read-regular-file (file &optional (version 0))
  (assert (typep file 'regular-file))
  (bt:with-lock-held ((lock file))
    (update-atime file))
  (if (<= 0 version (1- (length (content file))))
      (nth version (content file))
      (error 'file-system-error
             :message (format nil "Available versions 0-~D."
                              (1- (length (content file)))))))

(defun delete-file-permanently (dir name)
  ;; This does not check subdirectories.
  (bt:with-lock-held ((lock dir))
    (update-atime dir)
    (let ((ptr (gethash name (files dir))))
      (if (typep ptr 'pointer)
          (progn
            (remhash name (files dir))
            (ignore-errors
              (delete-file (make-id-file-pathname (id ptr))))
            (release-id (id ptr)))
          (error-file-not-found name)))))

(defun copy-regular-file (file destdir newname)
  (if (gethash newname (files destdir))
      (error-file-exists name)
      (bt:with-lock-held ((lock destdir))
        (let ((newfile (make-instance 'regular-file
                                      :id (next-id)
                                      :content (content file))))
          (update-atime destdir)
          (setf (gethash newname (files destdir)) (make-instance
                                                   'regular-file-ptr
                                                   :id (id newfile))
                (changed destdir) t
                (gethash (id newfile) *fsdata*) newfile)))))

(defun move-file (srcdir oldname destdir
                  &optional (newname oldname))
  ;; Move directory or regular file
  (if (gethash newname (files destdir))
      (error-file-exists name)
      (progn
        (bt:with-lock-held ((lock destdir))
          (update-atime destdir)
          (setf (gethash newname (files destdir)) (gethash oldname
                                                           (files srcdir))
                (changed destdir) t))
        (bt:with-lock-held ((lock srcdir))
          (remhash oldname (files srcdir))
          (update-atime srcdir)
          (setf (changed srcdir) t)))))


;;; Description of access control
;;; =============================
;;;
;;; In the filesystem's access control there are four concepts:
;;; "directory's owner", "public directory", "directory's editor" and
;;; "all users". Access control data is stored in directory objects and
;;; there an IRC user can be an owner of a directory and she can be an
;;; editor in a directory. The access concepts are described below.
;;;
;;; Owners
;;; ------
;;;
;;; Owners own a directory which means that they fully control the
;;; directory. They can create, move and delete subdirectories in their
;;; own directory. They can also delete or move the owned directory
;;; itself, but moving a directory requires owner access to the
;;; destination directory too (or the destination directory must be a
;;; public directory). Owners can create, modify, move and delete files
;;; in their own directory. Owners can add or remove other owners and
;;; add or remove editors for their directory. Only owners can list
;;; directory object's owner and editor data.
;;;
;;; Ownership is inherited to subdirectories. If owner of a directory
;;; creates a subdirectory she is automatically owner of the
;;; subdirectory. The new subdirectory has ownership set to "INHERIT"
;;; which means that ownership is controlled in its parent directory (or
;;; its parent or its parent or...). The owner of the root directory is
;;; owner of all directories.
;;;
;;; Owner can delete anybody, even herself, from the list of directory's
;;; owners. If all owners are deleted from a directory the directory
;;; becomes public.
;;;
;;; Public directory
;;; ----------------
;;;
;;; In a public directory everybody can create subdirectories. User who
;;; creates a subdirectory is automatically owner of the subdirectory.
;;; In a public directory everybody has also editor access.
;;;
;;; Editors
;;; -------
;;;
;;; If user has an editor access to a directory she can add, modify and
;;; delete in that directory. She can also move files, though moving
;;; requires an editor or owner access to the destination directory too
;;; (or the destination must be a public directory).
;;;
;;; All users
;;; ---------
;;;
;;; All users can list directories' content everywhere. All users can
;;; also print files' content everywhere. There's no restrictions for
;;; read access.

(defun public-dir-p (dir)
  (eql :public (owners dir)))

(defun make-dir-public (dir)
  (bt:with-lock-held ((lock dir))
    (update-atime dir)
    (setf (changed dir) t
          (owners dir) :public)))

(defun make-dir-inherit-ownership (dir)
  (bt:with-lock-held ((lock dir))
    (update-atime dir)
    (setf (changed dir) t
          (owners dir) :inherit)))

(defun add-to-owners (dir user)
  (with-slots (owners lock changed) dir
    (bt:with-lock-held (lock)
      (update-atime dir)
      (unless (listp owners)
        ;; Probably :PUBLIC or :INHERIT
        (setf owners nil))
      (setf changed t)
      (pushnew user owners :test #'string-equal))))

(defun remove-from-owners (dir nth)
  (with-slots (owners lock changed) dir
    (bt:with-lock-held (lock)
      (update-atime dir)
      (setf changed t
            owners (remove-nth nth owners)))))

(defun add-to-editors (dir user)
  (with-slots (editors lock changed) dir
    (bt:with-lock-held (lock)
      (update-atime dir)
      (setf changed t)
      (pushnew user editors :test #'string-equal))))

(defun remove-from-editors (dir nth)
  (with-slots (editors lock changed) dir
    (bt:with-lock-held (lock)
      (update-atime dir)
      (setf changed t
            editors (remove-nth nth editors)))))

(defun match-user (pattern user)
  (let* ((patterns (split-sequence #\* pattern))
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

(defun inherit-ownership-p (dir)
  (eql :inherit (owners dir)))

(defun ownerp (dir user)
  (labels ((ownp (dir)
             (cond ((and (listp (owners dir))
                         (some (lambda (pat)
                                 (match-user pat user))
                               (owners dir)))
                    (return-from ownerp t))
                   ((and (inherit-ownership-p dir)
                         (parent dir))
                    (ownp (from-ptr-to-target (parent dir))))
                   (t (return-from ownerp nil)))))
    (bt:with-lock-held ((lock dir))
      (ownp dir))))

(defun editor-access-p (dir user)
  (or (public-dir-p dir)
      (some (lambda (pat)
              (match-user pat user))
            (editors dir))
      (ownerp dir user)))

(defun mkdir-access-p (dir user)
  (or (public-dir-p dir)
      (ownerp dir user)))

(defun list-files (dir)
  (bt:with-lock-held ((lock dir))
    (update-atime dir))
  (loop :for name :being :each :hash-key :in (files dir)
        :using (hash-value ptr)
        :collect (cons name ptr) :into all-files
        :finally (return (sort all-files #'string-lessp :key #'car))))

(defun create-directory (dir name)
  (if (gethash name (files dir))
      (error-file-exists name)
      (with-slots (files id changed lock) dir
        (let ((new (make-instance 'directory
                                  :parent (make-instance 'directory-ptr :id id)
                                  :id (next-id))))
          (bt:with-lock-held (lock)
            (update-atime dir)
            (setf (gethash name files) (make-instance 'directory-ptr
                                                      :id (id new))
                  changed t))
          (setf (gethash (id new) *fsdata*) new)))))

(defun create-regular-file (dir name)
  (if (gethash name (files dir))
      (error-file-exists name)
      (with-slots (files changed lock) dir
        (let ((file (make-instance 'regular-file :id (next-id))))
          (bt:with-lock-held (lock)
            (update-atime dir)
            (setf (gethash name files) (make-instance 'regular-file-ptr
                                                      :id (id file))
                  changed t))
          (setf (gethash (id file) *fsdata*) file)))))

(defun find-target (dir path)
  (cond
    ((null path) dir)
    ((eql :root (first path))
     (find-target (get-root-dir) (rest path)))
    ((eql :parent (first path))
     (let ((parent-ptr (parent dir)))
       (if (typep parent-ptr 'directory-ptr)
           (find-target (from-ptr-to-target parent-ptr) (rest path))
           (error 'file-system-error :message "Parent directory not found."))))
    ((eql :current (first path))
     (find-target dir (rest path)))
    ((stringp (first path))
     (let ((ptr (gethash (first path) (files dir))))
       (if ptr
           (find-target (from-ptr-to-target ptr) (rest path))
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
             :if (typep value 'directory-ptr)
             :collect (from-ptr-to-target value))))

(defmacro do-subdirs ((var dir &optional result-form) &body body)
  `(progn (map-subdirs (lambda (,var) ,@body) ,dir)
          ,result-form))

;;; Description of print/read format of files
;;;
;;; Files' print format consists of three or more Lisp objects which are
;;; readable with CL:READ function: type, version and content object(s).
;;;
;;; The first object identifies the type of the file. It must be a
;;; keyword symbol (e.g. :DIRECTORY or :REGULAR-FILE).
;;;
;;; The second object is the version number (integer) of the file
;;; format. It's a hint for the reader how the rest of the objects
;;; should be read.
;;;
;;; The third and the rest of the objects are the content of the file.
;;; Their format is generally unspecified but the printer and reader
;;; must agree that a specific file type and version number (see above)
;;; means a specific content format.

(defgeneric print-file (file &optional stream))

(defmethod print-file ((dir directory)
                       &optional (stream *standard-output*))
  (with-standard-io-syntax
    ;; If the output format changes increase the version number and add
    ;; a compatible reader function.
    (format stream "~S ~S~%" :directory 1)
    ;; Slots in order: id, parent, owners, editors, files.
    (format stream "~S ~S ~S ~S~%~S~%"
            (id dir)
            (if (parent dir) (id (parent dir)))
            (owners dir)
            (editors dir)
            (let (alist)
              (maphash (lambda (key value)
                         (push (etypecase value
                                 (regular-file-ptr (list key :r (id value)))
                                 (directory-ptr (list key :d (id value))))
                               alist))
                       (files dir))
              alist))))

(defmethod print-file ((file regular-file)
                       &optional (stream *standard-output*))
  (with-standard-io-syntax
    ;; If the output format changes increase the version number and add
    ;; a compatible reader function.
    (format stream "~S ~S~%" :regular-file 1)
    ;; Slots in order: id, content.
    (format stream "~S ~S~%" (id file) (content file))))
