;;; init.el --- Spacemacs Initialization File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

;; Increase gc-cons-threshold, depending on your system you may set it back to a
;; lower value in your dotfile (function `dotspacemacs/user-config')
(setq gc-cons-threshold 100000000)

(defconst spacemacs-version         "0.200.13" "Spacemacs version.")
(defconst spacemacs-emacs-min-version   "24.4" "Minimal version of Emacs.")

(if (not (version<= spacemacs-emacs-min-version emacs-version))
    (error (concat "Your version of Emacs (%s) is too old. "
                   "Spacemacs requires Emacs version %s or above.")
           emacs-version spacemacs-emacs-min-version)
  (load-file (concat (file-name-directory load-file-name)
                     "core/core-load-paths.el"))
  (require 'core-spacemacs)
  (spacemacs/init)
  (configuration-layer/sync)
  (spacemacs-buffer/display-startup-note)
  (spacemacs/setup-startup-hook)
  (add-to-list 'auto-mode-alist '("\\.\\(pl\\|pro\\|lgt\\)" . prolog-mode))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (require 'server)
  (unless (server-running-p) (server-start)))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(modify-syntax-entry ?_ "w")
(global-set-key (kbd "C-&") 'windmove-left)          ; move to left window
(global-set-key (kbd "C-<left>") 'windmove-left)          ; move to left window
(global-set-key (kbd "C-)") 'windmove-right)        ; move to right window
(global-set-key (kbd "C-<right>") 'windmove-right)        ; move to right window
(global-set-key (kbd "C-<up>") 'windmove-up)              ; move to upper window
(global-set-key (kbd "C-<down>") 'windmove-down)          ; move to lower window

;;;;;;;;;;;
;; ORG MODE
;;;;;;;;;;;
;; add python support to org mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (python . t)))

;; do not ask for confirmation when executing code cells
(setq org-confirm-babel-evaluate nil)

;; word files
(setq org-odt-preferred-output-format "doc")

;; strikethrough done elements
(setq org-fontify-done-headline t)
(custom-set-faces
 '(org-done ((t (:foreground "PaleGreen"
                             :weight normal
                             :strike-through t))))
 '(org-headline-done
   ((((class color) (min-colors 16) (background dark))
     (:foreground "light gray" :strike-through t)))))

;; insert new bullet
(use-package org
   :bind (:map org-mode-map
	       ("<C-=>" . 'org-insert-heading-respect-content-below)))

;; latex size
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.75))
(setq org-startup-with-latex-preview t)

(desktop-save-mode 1)
