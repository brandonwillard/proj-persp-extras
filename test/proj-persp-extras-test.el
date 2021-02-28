;;; proj-persp-extras-test.el --- Custom projectile + persp-mode interactions tests.
;;
;; Copyright (c) 2018-2021 Brandon T. Willard
;;
;; Author: Brandon T. Willard
;; URL: https://github.com/brandonwillard/proj-persp-extras
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(require 'persp-mode)
(require 'projectile)
(require 'cl-lib)
(require 'ert)
(require 'proj-persp-extras)

(when (> emacs-major-version 26)
  (defalias 'ert--print-backtrace 'backtrace-to-string))

;;; Test Utilities

(defmacro projectile-test-with-sandbox (&rest body)
  "Evaluate BODY in an empty temporary directory.

Taken from `projectile's `projectile-test.el'.
"
  (declare (indent 0) (debug (&rest form)))
  `(let ((sandbox (expand-file-name
                   (convert-standard-filename "test/sandbox/")
                   (file-name-directory (locate-library "proj-persp-extras.el" t)))))
     (when (file-directory-p sandbox)
       (delete-directory sandbox t))
     (make-directory sandbox t)
     (let ((default-directory sandbox))
       ,@body)))

(defmacro projectile-test-with-files (files &rest body)
  "Evaluate BODY in the presence of FILES.
You'd normally combine this with `projectile-test-with-sandbox'.

Taken from `projectile's `projectile-test.el'.
"
  (declare (indent 1) (debug (sexp &rest form)))
  `(progn
     ,@(mapcar (lambda (file)
                 (if (string-suffix-p "/" file)
                     `(make-directory ,file t)
                   `(with-temp-file ,file)))
               files)
     ,@body))

(defmacro persp-test-with (&rest body)
  (declare (indent 0))
  `(progn
     (persp-mode 1)
     (proj-persp-extras-mode +1)
     (projectile-mode +1)
     ,@body
     (persp-mode -1)
     (proj-persp-extras-mode -1)
     (projectile-mode -1)))

;;; Tests

(ert-deftest test-switch-persp ()
    (persp-test-with
     (projectile-test-with-sandbox
      (projectile-test-with-files
       ("project-1/"
        "project-1/.projectile"
        "project-1/file-1"
        "project-2/"
        "project-2/.projectile"
        "project-2/file-2")

       (let ((project-1-dir (file-name-as-directory (expand-file-name "project-1")))
             (project-2-dir (file-name-as-directory (expand-file-name "project-2"))))

         (projectile-add-known-project project-1-dir)
         (projectile-add-known-project project-2-dir)

         ;; Switch to the first project
         (cd "project-1")

         (should (equal (projectile-project-name) "project-1"))
         (should (equal (projectile-project-root) project-1-dir))

         ;; This will create a perspective and associate it with project-1
         (persp-switch project-1-dir)

         ;; Switch to the second project
         (cd "../project-2")

         ;; This will create a second perspective and associate it with project-2
         (persp-switch project-2-dir)

         (should (equal (safe-persp-name (get-frame-persp)) project-2-dir))
         (should (equal (projectile-project-name) "project-2"))
         (should (equal (projectile-project-root) project-2-dir))

         ;; Switch back to the first perspective while we're still in project-2
         (persp-switch  project-1-dir)

         (should (equal (safe-persp-name (get-frame-persp)) project-1-dir))

         ;; Projectile should report that we're in project-1 now
         (should (equal (projectile-project-name) "project-1"))
         (should (equal (projectile-project-root) project-1-dir)))))))

;;; proj-persp-extras-test.el ends here
