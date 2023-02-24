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

;;;;;;;;
;; navigation
;;;;;;;;
(map! "C-<left>" #'windmove-left
      "C-<right>" #'windmove-right
      "C-<up>" #'windmove-up
      "C-<down>" #'windmove-down)

;;;;;;;;
;; clipboard copy-paste
;;;;;;;;
(defun copy-to-clipboard ()
   "Copies selection to x-clipboard."
   (interactive)
   (if (display-graphic-p)
       (progn
         (message "Yanked region to x-clipboard!")
         (call-interactively 'clipboard-kill-ring-save))

     (if (region-active-p)
         (progn
           (shell-command-on-region (region-beginning) (region-end) "xsel     -i -b")
           (message "Yanked region to clipboard!")
           (deactivate-mark))
       (message "No region active; can't yank to clipboard!"))))


(defun paste-from-clipboard ()
  "Pastes from x-clipboard."
  (interactive)
  (if (display-graphic-p)
      (progn
        (clipboard-yank)
        (message "graphics active"))

    (insert (shell-command-to-string "xsel -o -b"))))
(map! :leader "o y" #'copy-to-clipboard)
(map! :leader "o p" #'paste-from-clipboard)

;;;;;;;;
;; magit
;;;;;;;;
(map! :leader "m s" #'magit-status)
(map! :leader "m c" #'magit-checkout)
(map! :map 'override "M-s o" #'smerge-keep-other)
(map! :map 'override "M-s m" #'smerge-keep-mine)

;;;;;;;;
;; roam
;;;;;;;;
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


;;;;;;;;;;;;;;;
;;; org-present
;;;;;;;;;;;;;;;


(defun org-present-start ()
  ;; Center the presentation and wrap lines
  (setq visual-fill-column-width 150
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1)
  (visual-line-mode 1)
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (scroll-bar-mode 0))

 

(defun org-present-end ()
  ;; Stop centering the document
  (visual-fill-column-mode 0)
  (visual-line-mode 0)
  (menu-bar-mode 1)
  (tool-bar-mode 1)
  (scroll-bar-mode 1))


(add-hook 'org-present-mode-hook 'display-line-numbers-mode)
(add-hook 'org-present-mode-hook 'org-present-start)
(add-hook 'org-present-mode-quit-hook 'display-line-numbers-mode)
(add-hook 'org-present-mode-quit-hook 'org-present-end)


;;;;;;;;
;; babel
;;;;;;;;
(defvar ipython-code-block-args "ipython :session :results raw drawer :exports both")

(defun insert-org-mode-code-block (code-block-args)
  (let* (
         (code-block-end "#+END_SRC")
         (n-backward (+ 1 (length code-block-end))))
    (progn
      (insert
       (concat "#+BEGIN_SRC " code-block-args "\n\n" code-block-end))
      (backward-char n-backward))))

(defun insert-ipython-org-mode-code-block ()
  (interactive)
  (insert-org-mode-code-block ipython-code-block-args))

(map! "C-c i" #'insert-ipython-org-mode-code-block)

(after! org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ipython . t)
     (python . t))))


(defun rename-async-buffer-with-truncated-lines (buffer-name)
  (with-current-buffer "*Async Shell Command*"
    (progn
      (rename-buffer buffer-name)
      (toggle-truncate-lines))))
;; chatgpt

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

;;;;;;;;
;; docker-compose
;;;;;;;;

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

;;;;;;;;
;; flycheck
;;;;;;;;
(after! flycheck-mode
  (setq flycheck-disabled-checkers (cl-pushnew python-pylint flycheck-disabled-checkers)))

;;;;;;;;
;; emacs ipython notebook
;;;;;;;;
(map! "C-c C-d" #'ein:worksheet-kill-cell)

(defun elpy-ein-enable (&optional _ignored)
  "Enable Elpy in all future Python buffers."
  (interactive)
  (unless elpy-enabled-p
    (when _ignored
      (warn "The argument to `elpy-enable' is deprecated, customize `elpy-modules' instead"))
    (elpy-modules-global-init)
    (define-key inferior-python-mode-map (kbd "C-c C-z") 'elpy-shell-switch-to-buffer)
    (add-hook 'ein:notebook-mode-hook 'elpy-mode)
    (add-hook 'pyvenv-post-activate-hooks 'elpy-rpc--disconnect)
    (add-hook 'pyvenv-post-deactivate-hooks 'elpy-rpc--disconnect)
    (add-hook 'inferior-python-mode-hook 'elpy-shell--enable-output-filter)
    (add-hook 'python-shell-first-prompt-hook 'elpy-shell--send-setup-code t)
    ;; Add codecell boundaries highligting
    (font-lock-add-keywords
     'ein:notebook-mode
     `((,(replace-regexp-in-string "\\\\" "\\\\"
                                   elpy-shell-cell-boundary-regexp)
        0 'elpy-codecell-boundary prepend)))
    ;; Enable Elpy-mode in the opened python buffer
    (setq elpy-enabled-p t)
    (dolist (buffer (buffer-list))
      (and (not (string-match "^ ?\\*" (buffer-name buffer)))
           (with-current-buffer buffer
             (when (string= major-mode 'ein:notebook-mode)
               (ein:notebook-mode)  ;; update codecell fontification
               (elpy-mode t)))))))
