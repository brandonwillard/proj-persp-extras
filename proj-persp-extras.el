;;; proj-persp-extras.el --- An Emacs package providing a mode that makes projectile commands operate conditional on the current perspective
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
;; An Emacs package providing a mode that makes projectile commands operate conditional on the current perspective

;;; Code:

(require 'cl-lib)
(require 'projectile)
(require 'persp-mode)
(require 's)
(require 'f)

(defun proj-persp-extras//projectile-switch-project-by-name (project-to-switch &optional arg)
    "This version *doesn't* pre-load the dir-locals."
    (unless (projectile-project-p project-to-switch)
      (projectile-remove-known-project project-to-switch)
      (error "Directory %s is not a project" project-to-switch))
    (let ((switch-project-action (if arg
                                      'projectile-commander
                                    projectile-switch-project-action)))
      (run-hooks 'projectile-before-switch-project-hook)
      (let* ((default-directory project-to-switch)
              (projectile-project-name (funcall projectile-project-name-function
                                                project-to-switch)))
        (funcall switch-project-action)
        ;; Default to VCS when/if `helm-projectile' is aborted.
        (if (and (string-prefix-p "helm"
                                  (symbol-name switch-project-action))
                  (eq helm-exit-status 1))
            (projectile-vc project-to-switch)))
      (run-hooks 'projectile-after-switch-project-hook)))

(defun proj-persp-extras//persp-assign-projectile-root (persp persp-hash)
  "Add a variable to the perspective tracking the projectile project name
 (if any).

 This is run before the buffer is created, so we need to get the project name
 from this perspective's path.  We assume the perspective's name is the project
 path"
  (let* ((persp-name (safe-persp-name persp))
         (persp-projectile-dir (when (and (f-dir? persp-name)
                                          (funcall projectile-project-name-function
                                                   persp-name))
                                 persp-name)))
    (set-persp-parameter 'projectile-project-root
                         persp-projectile-dir
                         persp)))

(defun proj-persp-extras//proj-persp-project-root (oldfun &rest r)
  "Use the persp project name and regular `projectile-project-root' as a
 fallback."
  (let* ((persp-name (safe-persp-name (get-frame-persp)))
         (persp-projectile-dir (when (and (f-dir? persp-name)
                                          (funcall projectile-project-name-function
                                                   persp-name))
                                 persp-name)))
    ;; If the persp name is a directory and is mapped to a projectile project,
    ;; return the directory; otherwise, use the fallback.
    (or persp-projectile-dir
        (persp-parameter 'projectile-project-root)
        (apply oldfun r))))

(defun proj-persp-extras//proj-persp-project-name (oldfun &rest r)
  "Query the persp layout for the projectile project name and use projectile
 for the fallback."
  (let* ((persp-name (safe-persp-name (get-frame-persp)))
         (persp-projectile-name (if (f-dir? persp-name)
                                    (funcall projectile-project-name-function
                                             persp-name)
                                  (persp-parameter 'projectile-project-root))))
    (or persp-projectile-name (apply oldfun r))))

;;;###autoload
(define-minor-mode proj-persp-tracking-mode
  "Toggle proj-persp-tracking-mode mode."
  :require 'persp-mode
  :init-value nil
  :global t
  (if proj-persp-tracking-mode
      (progn
        (add-hook 'persp-created-functions #'proj-persp-extras//persp-assign-projectile-root)
        (advice-add #'projectile-project-root :around #'proj-persp-extras//proj-persp-project-root)
        (advice-add #'projectile-project-name :around #'proj-persp-extras//proj-persp-project-name))
    (progn
      (remove-hook 'persp-created-functions #'proj-persp-extras//persp-assign-projectile-root)
      (advice-remove #'projectile-project-root #'proj-persp-extras//proj-persp-project-root)
      (advice-remove #'projectile-project-name #'proj-persp-extras//proj-persp-project-name))))

(defun proj-persp-extras//enable ()
  (proj-persp-tracking-mode +1)

  ;; Don't bother us with unsaved files in other projects when we try to compile.
  ;; TODO: This should probably be set in `projectile--run-project-cmd'.
  (setq compilation-save-buffers-predicate
        (lambda ()
          (when-let ((project-root (projectile-project-root)))
            (projectile-project-buffer-p (current-buffer) project-root))))

  (advice-add #'projectile-switch-project-by-name
              :override #'proj-persp-extras//projectile-switch-project-by-name))

(defun proj-persp-extras//disable ()
  (proj-persp-tracking-mode -1)
  (advice-remove #'projectile-switch-project-by-name #'proj-persp-extras//projectile-switch-project-by-name))

;;;###autoload
(define-minor-mode proj-persp-extras-mode
  "Toggle proj-persp-extras mode."
  :require 'projectile ; 'persp-mode
  :init-value nil
  :global t
  (if proj-persp-extras-mode
      (proj-persp-extras//enable)
    (proj-persp-extras//disable)))

(provide 'proj-persp-extras)
;;; proj-persp-extras.el ends here
