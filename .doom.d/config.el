;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Projects/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "/path/to/org-files/"))
  (org-roam-directory (file-truename "~/Projects/org/roam/"))
  (org-roam-index-file (file-truename"~/Projects/org/roam/index.org")

                       :bind (("C-c n l" . org-roam-buffer-toggle)
                              ("C-c n f" . org-roam-node-find)
                              ("C-c n g" . org-roam-graph)
                              ("C-c n i" . org-roam-node-insert)
                              ("C-c n c" . org-roam-capture)
                              ;; Dailies
                              ("C-c n j" . org-roam-dailies-capture-today))
                       :config
                       ;; If you're using a vertical completion framework, you might want a more informative completion interface
                       (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
                       (org-roam-db-autosync-mode)
                       ;; If using org-roam-protocol
                       (require 'org-roam-protocol)))
(after! org)


(defun rename-async-buffer-with-truncated-lines (buffer-name)
  (with-current-buffer "*Async Shell Command*"
    (progn
      (rename-buffer buffer-name)
      (toggle-truncate-lines))))


(defun ask-chatgpt ()
  "ask chatgpt using https://github.com/mmabrouk/chatgpt-wrapper"
  (interactive)
  (let ((buffer-name "ChatGPT"))
    (if (not (get-buffer buffer-name))
        ;; run chatgpt in new buffer if it does not exist
        (progn
          (async-shell-command "killall firefox; chatgpt install")
          (rename-async-buffer-with-truncated-lines "ChatGPT"))
        (switch-to-buffer-other-window buffer-name))))


(defun docker-compose-all-impl (down build daemon)
  "runs docker-compose"
  (let*
      ((buffer-name "Docker compose")
       (as-daemon-str (if daemon "; -d" ""))
       (down-str (if down "docker-compose down;" ""))
       (build-str (if build "docker-compose build;" ""))
       (command (concat down-str build-str "docker-compose up" as-daemon-str)))
    (progn
      (when (get-buffer buffer-name) (kill-buffer buffer-name))
      (async-shell-command command)
      (rename-async-buffer-with-truncated-lines buffer-name))))

(map! "C-<left>" #'windmove-left
      "C-<right>" #'windmove-right
      "C-<up>" #'windmove-up
      "C-<down>" #'windmove-down)

(defun dup ()
  "docker compose up"
  (interactive) (docker-compose-all-impl nil nil nil))
(defun dupd ()
  "docker compose down then up in daemon"
  (interactive) (docker-compose-all-impl nil nil t))
(defun ddup ()
  "docker compose down; build then up"
  (interactive) (docker-compose-all-impl t t nil))
(defun ddupd ()
  "docker compose down; build then up in daemon"
  (interactive) (docker-compose-all-impl t t t))


(after! flycheck-mode
  (setq flycheck-disabled-checkers (cl-pushnew python-pylint flycheck-disabled-checkers)))
