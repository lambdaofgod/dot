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
(setq lisp-indent-offset 4)

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

;; helper functions
;; clipboard functions
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

;; shell functions
;; rename buffer used to run async shell command with 'buffer-name'
;; this is useful when running shell commands in the background like docker-compose

(defun rename-async-buffer-with-truncated-lines (buffer-name)
    (with-current-buffer "*Async Shell Command*"
        (progn
            (rename-buffer buffer-name)
            (toggle-truncate-lines))))


;; buffer manipulation for sending stuff to repl
(defun get-buffer-contents-up-to-cursor ()
    (buffer-substring (point-min) (point)))

(defun shell-eval-before-cursor (shell-eval)
    "eval contents up to current cursor position using 'shell-eval function'"
    (funcall shell-eval
        (get-buffer-contents-up-to-cursor)))


(defun goto-messages-buffer ()
    "Switch to the *Messages* buffer."
    (interactive)
    (switch-to-buffer "*Messages*"))

;; make interactive function from a function
(defmacro mklambdai (expr)
    `(lambda () (interactive) ,expr))

(defun dump-buffer-to-logfile ()
    (interactive)
   (let ((filename (concat "~/" (downcase (buffer-name)) ".log")))
    (set-visited-file-name filename)
    (save-buffer)
    (set-visited-file-name nil)))

;;;;;;;;
;; navigation
;; go to this config

(map! :leader
    :prefix
    "o"
    :desc "go to doom config"
    "c" #'doom/goto-private-config-file
    :desc "go to doom init"
    "i" #'doom/goto-private-init-file)

;; window navigation
(map!
    "C-<left>" #'windmove-left
    "C-<right>" #'windmove-right
    "C-<up>" #'windmove-up
    "C-<down>" #'windmove-down)

;; buffers
(map!
    :desc "Buffer viewing utils"
    :leader
    :prefix "v"
    :desc "View message buffer"
    "m" #'goto-messages-buffer
    :desc "View buffers"
    "b" #'view-buffer
    :desc "elisp repl"
    "r" #'+emacs-lisp/open-repl)

;; truncation
(map! :leader "t t" #'toggle-truncate-lines)

;; commenting
(map!
    :leader "r c" #'comment-region
    :leader "r u" #'uncomment-region)

;; hyperbole
(after! hyperbole
    (define-key hyperbole-mode-map (kbd "M-]") #'action-key)
    (define-key hyperbole-mode-map (kbd "M-[") #'other-window))



;;;;;;;;
;; medsi azure
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "brave-browser")

(map!
    :leader
    :desc "go to project git repository"
    "g g" (mklambdai (browse-url "https://dev.azure.com/medsi/_git/apteki")))



;;;;;;;;;;;;;;;;;;;;;;;
;; clipboard copy-paste
;;;;;;;;;;;;;;;;;;;;;;;
(map! :leader "o y" #'copy-to-clipboard)
(map! :leader "o p" #'paste-from-clipboard)

;;;;;;;;;;;;;;;;
;; python
;;;;;;;;;;;;;;;;

(defun python-shell-eval-before-cursor ()
    (interactive)
    (shell-eval-before-cursor #'python-shell-send-region))

(map! :after python-mode
    :map python-mode-map 'override "C-c C-r" #'python-shell-eval-before-cursor
    :map python-mode-map "C-c C-v" #'python-shell-send-region)

;; black
(use-package! python-black
    :demand t
    :after python)
(add-hook! 'python-mode-hook #'python-black-on-save-mode)
(map! :leader :desc "Blacken Buffer" "m =" #'python-black-buffer)
(map! :leader :desc "Blacken Region" "m - r" #'python-black-region)
(map! :leader :desc "Blacken Statement" "m - s" #'python-black-statement)


;;;;;;;;
;; magit
;;;;;;;;
;;;;;;;;
(map!
    :leader
    :desc "magit"
    :prefix
    "m"
    :desc "status"
    "s" #'magit-status
    "c" #'magit-checkout)

(map! :map 'override "M-s n" #'smerge-next)
(map! :map 'override "M-s p" #'smerge-prev)
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
        :bind (
                  ("C-c n l" . org-roam-buffer-toggle)
                  ("C-c n f" . org-roam-node-find)
                  ("C-c n g" . org-roam-graph)
                  ("C-c n i" . org-roam-node-insert)
                  ("C-c n c" . org-roam-capture)
                  ("C-c n j" . org-roam-dailies-capture-today))
        :config
        ;; If you're using a vertical completion framework, you might want a more informative completion interface
        (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
        (org-roam-db-autosync-mode)
        ;; If using org-roam-protocol
        (require 'org-roam-protocol)))
(after! org)

(defun org-mode-sync ()
    (interactive)
    (async-shell-command (concat "bash " org-directory "/scripts/run_autocommit_loop.sh"))
    (rename-async-buffer-with-truncated-lines "org-sync"))


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


(defun insert-org-mode-code-block (code-block-args)
    (let* (
              (code-block-end "#+END_SRC")
              (n-backward (+ 1 (length code-block-end))))
        (progn
            (insert
                (concat "#+BEGIN_SRC " code-block-args "\n\n" code-block-end))
            (backward-char n-backward))))
(defvar default-code-block-args " :session :results raw drawer :exports both")



(defun insert-lang-org-mode-code (lang-name)
    (insert-org-mode-code-block (concat lang-name default-code-block-args)))


(map!
    :map 'override
    :prefix "C-c i"
    :desc "insert ipython code block"
    "i" (mklambdai (insert-lang-org-mode-code "ipython"))
    :desc "insert python code block"
    "p" (mklambdai (insert-lang-org-mode-code "python")))



(after! org-babel
    (org-babel-do-load-languages
        'org-babel-load-languages
        '((ipython . t) (python . t) (hy . t))))

;;;;;;;;;;
;; chatgpt
;;;;;;;;;;

;; TODO: send messages to stop chatgpt response?
;; currently communication only uses text, maybe there is a better way to run it

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

;;;;;;;;;;;;;;;;;
;; docker-compose
;;;;;;;;;;;;;;;;;

;; mnemonics for running/building docker compose

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

;;;;;;;;;;;
;; flycheck
;;;;;;;;;;;
(after! flycheck-mode
    (setq flycheck-disabled-checkers (cl-pushnew python-pylint flycheck-disabled-checkers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ein (emacs ipython notebook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
                    elpy-shell-cell-boundary-regexp
                    0 'elpy-codecell-boundary prepend))))
        ;; Enable Elpy-mode in the opened python buffer
        (setq elpy-enabled-p t)
        (dolist (buffer (buffer-list))
            (and (not (string-match "^ ?\\*" (buffer-name buffer)))
                (with-current-buffer buffer
                    (when (string= major-mode 'ein:notebook-mode)
                        (ein:notebook-mode)  ;; update codecell fontification
                        (elpy-mode t)))))))

(add-hook 'ein:notebook-mode-hook
    (lambda () (local-set-key (kbd "C-c b") #'ein:worksheet-insert-cell-below)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hylang
;; hy is supported in doom :lang section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hy-shell-eval-before-cursor ()
    (interactive)
    (shell-eval-before-cursor #'hy-shell--send))

(after! hy-mode
    (map! :map hy-mode-map
        "C-c C-r" #'hy-shell-eval-before-cursor
        "C-c C-v" #'hy-shell-eval-region))

