;;; init.el --- Spacemacs Initialization File -*- no-byte-compile: t -*-
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)
;; Avoid garbage collection during startup.
;; see `SPC h . dotspacemacs-gc-cons' for more info
(defconst emacs-start-time (current-time))
(setq gc-cons-threshold 402653184 gc-cons-percentage 0.6)
(load (concat (file-name-directory load-file-name)
              "core/core-versions")
      nil (not init-file-debug))
(load (concat (file-name-directory load-file-name)
              "core/core-load-paths")
      nil (not init-file-debug))
(load (concat spacemacs-core-directory "core-dumper")
      nil (not init-file-debug))

;; Remove compiled core files if they become stale or Emacs version has changed.
(load (concat spacemacs-core-directory "core-compilation")
      nil (not init-file-debug))
(load spacemacs--last-emacs-version-file t (not init-file-debug))
(when (or (not (string= spacemacs--last-emacs-version emacs-version))
          (spacemacs//dir-contains-stale-byte-compiled-files-p
           spacemacs-core-directory))
  (spacemacs//remove-byte-compiled-files-in-dir spacemacs-core-directory))
;; Update saved Emacs version.
(unless (string= spacemacs--last-emacs-version emacs-version)
  (spacemacs//update-last-emacs-version))

(if (not (version<= spacemacs-emacs-min-version emacs-version))
    (error (concat "Your version of Emacs (%s) is too old. "
                   "Spacemacs requires Emacs version %s or above.")
           emacs-version spacemacs-emacs-min-version)
  ;; Disable file-name-handlers for a speed boost during init
  (let ((file-name-handler-alist nil))
    (require 'core-spacemacs)
    (spacemacs/dump-restore-load-path)
    (configuration-layer/load-lock-file)
    (spacemacs/init)
    (configuration-layer/stable-elpa-init)
    (configuration-layer/load)
    (spacemacs-buffer/display-startup-note)
    (spacemacs/setup-startup-hook)
    (spacemacs/dump-eval-delayed-functions)
    (when (and dotspacemacs-enable-server (not (spacemacs-is-dumping-p)))
      (require 'server)
      (when dotspacemacs-server-socket-dir
        (setq server-socket-dir dotspacemacs-server-socket-dir))
      (unless (server-running-p)
        (message "Starting a server...")
        (server-start)))))

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
 '(
   (emacs-lisp . t)
   (shell . t)
   (python . t)
   ))

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

(defun set-font-size ()
  "Set the font size."
  (interactive)
  (set-face-attribute
   'default nil :height
   (string-to-number
    (read-string "Font size: " (number-to-string (face-attribute 'default :height nil))))))

(desktop-save-mode 1)
