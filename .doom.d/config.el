;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(load! "paths.el")
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name ""
    user-mail-address "")

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
(setq projectile-project-se :service anthropic :model claude-3-opus-20240229arch-path '("~/Projects"))


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

(defun get-from-clipboard (quote-char)
    (let ((clipboard-text
              (if
                  (display-graphic-p)
                  (substring-no-properties (gui-get-selection 'CLIPBOARD))
                  (shell-command-to-string "xsel -o -b"))))
        (concat quote-char clipboard-text quote-char)))

(defun paste-from-clipboard (quote-char)
    "Pastes from x-clipboard."
    (insert (get-from-clipboard quote-char)))

(defun paste-from-clipboard (quote-char)
    "Pastes from x-clipboard."
        (insert (get-from-clipboard quote-char)))
;; shell functions
;; rename buffer used to run async shell command with 'buffer-name'
;; this is useful when running shell commands in the background like docker-compose

(defun fd (expr)
    (let (
             (fd-string-res (shell-command-to-string (format "fd %s --base-directory %s" expr (doom-modeline--project-root)))))
        (split-string fd-string-res split-string-default-separators)))


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

;; dump buffer contents to
(defun dump-buffer-to-logfile ()
    (interactive)
   (let ((filename (concat "~/" (downcase (buffer-name)) ".log")))
    (set-visited-file-name filename)
    (save-buffer)
    (set-visited-file-name nil)))

(defun get-associated-buffer-name (name)
    "associated Python buffer for org mode file"
    (format "*%s*" name))


(defun view-associated-buffer ()
    (interactive)
    (when (= 1 (length (window-list))) (split-window-right))
    (switch-to-buffer-other-window (get-associated-buffer-name (buffer-name))))

(defun kill-associated-buffer ()
    (interactive)
    (kill-buffer (get-associated-buffer-name (buffer-name))))


(defun get-file-dirname (file-path)
    (-> file-path
        (file-name-directory)
        (directory-file-name)
        (file-name-nondirectory)))

(defun get-buffer-dirname ()
    (interactive)
    (->> (buffer-name)
        (file-name-directory)
        (directory-file-name)
        (file-name-nondirectory)))

;;;;
;;;;
;;;;
(use-package! org-ai
  :ensure
  :commands (org-ai-mode)
  :custom
  (org-ai-openai-api-token "<ENTER YOUR API TOKEN HERE>")
  :init
  (add-hook 'org-mode-hook #'org-ai-mode)
  :config
  ;; if you are on the gpt-4 beta:
  (setq org-ai-default-chat-model "gpt-4"))
  ;; if you are using yasnippet and want `ai` snippets
  ;(org-ai-install-yasnippets))

;;;;;;;;
;; navigation
;; go to this config

(map! :leader
    :map 'override
    :prefix
    "d"
    :desc "go to doom config"
    "c" #'doom/goto-private-config-file
    :desc "go to doom init"
    "i" #'doom/goto-private-init-file)

(map! :leader
    :prefix
    "o"
    "b" (mklambdai (switch-to-buffer (find-file-noselect "~/.bashrc")))
    :desc "goto project docker compose"
    "d" #'doom-open-project-docker-compose
    :desc "goto chatgpt conversations"
    "c" (mklambdai  (switch-to-buffer (find-file-noselect "~/Projects/org/chatgpt_conversations.org")))
    :desc "goto tangle file"
    "t" #'org/goto-tangle-file)


;; window navigation
(map!
    "C-<left>" #'windmove-left
    "C-<right>" #'windmove-right
    "C-<up>" #'windmove-up
    "C-<down>" #'windmove-down)

;; buffers
(defun doom-open-file-in-project (filename)
  "Find FILENAME in project and open it in a new buffer."
  (interactive "sFilename: ")
  (let ((default-directory (doom-modeline--project-root)))
    (if (file-exists-p filename)
        (switch-to-buffer (find-file-noselect filename))
      (message "File %s not found in project" filename))))

(defun doom-open-project-docker-compose ()
  (interactive)
  (doom-open-file-in-project "docker-compose.yml"))


(map!
    :desc "Buffer viewing utils"
    :leader
    :prefix "v"
    :desc "View message buffer"
    "m" #'goto-messages-buffer
    :desc "View buffers"
    "v" #'view-buffer
    :desc "elisp repl"
    "r" #'+emacs-lisp/open-repl
    :desc "org babel local python buffer"
    "p" #'view-associated-buffer
    :desc "kill org babel local python buffer"
    "k" #'kill-associated-buffer
    :desc "go to org babel tangled file"
    "t" #'org/goto-tangle-filename)

    

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

;;
;; search-replace
;;
(map!
    :leader "s r" #'counsel-projectile-rg)


(defun swiper-replace ()
  "Swiper replace with mc selction."
  (interactive)
  (run-at-time nil nil (lambda ()
                         (ivy-wgrep-change-to-wgrep-mode)))
  (ivy-occur))


(map! :map ivy-minibuffer-map "C-c C-e" 'swiper-replace)

;;;;;;;;
;; medsi azure
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")


;;;;;;;;;;;;;;;;;;;;;;;
;; clipboard copy-paste
;;;;;;;;;;;;;;;;;;;;;;;
(map! :leader "o y" #'copy-to-clipboard)
(map! :leader "o p" (mklambdai (paste-from-clipboard "")))
(map! :leader "o q" (mklambdai (paste-from-clipboard "\"")))
(map! :leader "o c" (mklambdai (paste-from-clipboard "```\n")))

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


;; org

(use-package! ox-hugo
    :after ox)

(after! org
    (load! "util/org.el")
    (map! :map org-mode-map
        "M-<up>" #'org-babel-previous-src-block
        "M-<down>" #'org-babel-next-src-block)
    (setq org-capture-templates '(
                                       ("t" "Personal todo" entry (file+headline +org-capture-todo-file "Inbox")
                                           "* [ ] %U %?\n%i\n%a" :prepend t)
                                       ("n" "Personal notes" entry
                                               (file+headline +org-capture-notes-file "Inbox")
                                               "* %u %?\n%i\n%a" :prepend t)
                                       ("j" "Journal" entry
                                               (file+olp+datetree +org-capture-journal-file)
                                               "* %U %?\n%i\n%a")
                                       ("p" "Templates for projects")
                                       ("pt" "Project-local todo" entry
                                               (file+headline +org-capture-project-todo-file "Inbox")
                                               "* TODO %?\n%i\n%a" :prepend t)
                                       ("pn" "Project-local notes" entry
                                               (file+headline +org-capture-project-notes-file "Inbox")
                                               "* %U %?\n%i\n%a" :prepend t)
                                       ("pc" "Project-local changelog" entry
                                               (file+headline +org-capture-project-changelog-file "Unreleased")
                                               "* %U %?\n%i\n%a" :prepend t)
                                       ("o" "Centralized templates for projects")
                                       ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
                                       ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?\n %i\n %a" :prepend t :heading "Notes")
                                       ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a" :prepend t :heading "Changelog")))
    (add-to-list 'org-export-backends 'hugo)
    ;;(add-to-list 'org-latex-packages-alist '("" "buss" t))
    (setq org-agenda-custom-commands
      '(("d" "Deadlines"
         ((agenda ""
                  ((org-agenda-span 'day)
                   (org-agenda-time-grid nil)
                   (org-deadline-warning-days 365)
                   (org-agenda-entry-types '(:deadline)))))))))


(after! org-ref
    (setq shared-root "~/Projects")
    (defun get-path-in-shared-root (fname) (f-join shared-root fname))
    (defun get-path-in-org-root (fname) (f-join shared-root "org" fname))
    (setq bibtex-completion-bibliography (mapcar #'get-path-in-org-root ["refs.bib" "mgr_refs.bib"])))

;;;;;;;;
;; roam
;;;;;;;;
(use-package org-roam
    :ensure t
    :custom
    (org-roam-directory (file-truename "/path/to/org-files/"))
    (org-roam-directory (file-truename "~/Projects/org/roam/"))
    (org-roam-index-file (file-truename"~/Projects/org/roam/index.org"))
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
          (require 'org-roam-protocol)
          (add-to-list 'display-buffer-alist
              '("\\*org-roam\\*"
                (display-buffer-in-side-window)
                (side . bottom)
                (slot . 0)
                (window-height . 0.25))))
 
   



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
;; org-babel
;;;;;;;;
(load! "util/blocks.el")

(map!
    :map 'override
    :prefix "C-c i"
    :desc "insert sage code block"
    "s" (mklambdai (insert-babel-code-block "sage" (buffer-name)))
    :desc "insert ipython code block"
    "i" (mklambdai (insert-babel-code-block "ipython" (buffer-name)))
    :desc "insert Julia code block"
    "j" (mklambdai (insert-babel-code-block "julia" (buffer-name)))
    :desc "insert python code block"
    "p" (mklambdai (insert-babel-code-block "python" (buffer-name) ""))
    :desc "insert async python code block"
    "P" (mklambdai (insert-babel-code-block "python" (buffer-name) "" t))
    :desc "insert Rust code block"
    "r" (mklambdai (insert-babel-code-block "rust" (buffer-name) "" t))
    :desc "insert elisp code block"
    "e" (mklambdai (insert-babel-code-block "elisp" (buffer-name)))
    :desc "insert elixir code block"
    "x" (mklambdai (insert-babel-code-block "elixir" (buffer-name)))
    :desc "latex"
    "l" (mklambdai (insert-babel-code-block "latex" (buffer-name)))
    :desc "bash"
    "b" (mklambdai (insert-babel-code-block "bash" (buffer-name)))
    :desc "quote"
    "q" (mklambdai (insert-org-mode-block-with-content "" "" "QUOTE"))
    :desc "chatgpt"
    "C" #'chatgpt/insert-response-code-block
    :desc "cypher"
    "c" (mklambdai (insert-babel-code-block "cypher" (buffer-name)))
    :desc "org-ai"
    "a" (mklambdai (insert-org-mode-block-with-content "" "\n" "AI")))


(map!
    :map 'override
    :prefix "C-c"
    "j" #'org-babel-next-src-block
    "k" #'org-babel-previous-src-block
    "n" (mklambdai (progn (org-ctrl-c-ctrl-c) (org-babel-next-src-block)))
    "r" #'org-babel-execute-buffer
    "t" #'org-babel-execute-subtree
    "l s" #'org/store-link-to-current-line
    "l i" #'org/insert-stored-link)

(after! org-babel
    (org-babel-do-load-languages
        'org-babel-load-languages
        '((ipython . t) (python . t) (hy . t) (latex . t) (mermaid . t))))
    
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
                (rename-async-buffer-with-truncated-lines "*ChatGPT*"))
            (switch-to-buffer-other-window buffer-name))))



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


(load! "util/docker.el")
(load! "util/roam.el")
(load! "util/browsin.el")

;; dap
;; (use-package! dap-mode
;;     (dap-register-debug-template "Rust::GDB Run Configuration"
;;                                  (list :type "gdb"
;;                                        :request "launch"
;;                                        :name "GDB::Run")
;;                                :gdbpath "rust-gdb"
;;                                  :target nil
;;                                  :cwd nil))
;; (use-package dap-mode
;;   ;; Uncomment the config below if you want all UI panes to be hidden by default!
;;   ;; :custom
;;   ;; (lsp-enable-dap-auto-configure nil)
;;   ;; :config
;;   ;; (dap-ui-mode 1)

;;   :config
;;   ;; Set up Node debugging
;;   (require 'dap-node)
;;   (dap-node-setup) ;; Automatically installs Node debug adapter if needed

;;   ;; Bind `C-c l d` to `dap-hydra` for easy access
;;   (general-define-key
;;     :keymaps 'lsp-mode-map
;;     :prefix lsp-keymap-prefix
;;       "d" '(dap-hydra t :wk "debugger")))

(setq lsp-disabled-clients '(eslint))

(load! "util/ai.el")
(load! "util/python.el")

;;
;; autocomplete
;;

(defun add-codeium-completion ()
  (interactive)
  (setq completion-at-point-functions
        (cons 'codeium-completion-at-point
              completion-at-point-functions))
  (setq-local company-frontends
              '(company-pseudo-tooltip-frontend
                company-preview-frontend))
  (setq company-minimum-prefix-length 0))

(defun remove-codeium-completion ()
  (interactive)
  (setq completion-at-point-functions
        (delete 'codeium-completion-at-point
                completion-at-point-functions))
  (setq company-frontends
        '(company-box-frontend company-preview-frontend))
  (setq company-minimum-prefix-length 2))

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :custom (copilot-node-executable "/home/kuba/.volta/bin/node")
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

;;
;; priorities
;;
;;
(use-package! org-fancy-priorities
    :config
    (setq org-fancy-priorities-list '("MUST" "SHOULD" "COULD" "WONT")))


(use-package! conda)

(use-package! lsp-julia
  :config
  (setq lsp-julia-flags '("--project=~/.julia/environments/v1.9" "--startup-file=no" "--history-file=no"))
  (setq lsp-julia-default-environment "~/.julia/environments/v1.9")
  (setq lsp-julia-default-environment "~/.julia/environments/v1.9"))

(after! julia-mode
  (add-hook! 'julia-mode-hook
    (setq-local lsp-enable-folding t
                lsp-folding-range-limit 100)))

(use-package! lsp-mode
    :config
    (lsp-register-custom-settings
        '(("rust-analyzer.cargo.extraEnv"
           (("LIBTORCH" . "/root/miniconda/lib/python3.10/site-packages/torch")
            ("LD_LIBRARY_PATH" . "/root/miniconda/lib/python3.10/site-packages/torch/lib")
            ("LIBTORCH_CXX11_ABI" . "0")))))
    (lsp-register-client
      (make-lsp-client
       :new-connection
       (lsp-stdio-connection (list "swipl"
                                   "-g" "use_module(library(lsp_server))."
                                   "-g" "lsp_server:main"
                                   "-t" "halt"
                                   "--" "stdio"))
       :major-modes '(prolog-mode)
       :priority 1
       :multi-root t
       :server-id 'prolog-ls))
    :hook (elixir-mode . lsp)
    :init (add-to-list 'exec-path "/home/kuba/.lsp/elixir"))
 

(load! "util/mojo.el")
(use-package eglot
  :ensure t
    :defer t
    :hook (
           (mojo-mode . eglot-ensure))
    :config
    (add-to-list 'eglot-server-programs '(mojo-mode . ("/home/kuba/.modular/pkg/packages.modular.com_max/bin/mojo-lsp-server")))
    (add-to-list 'eglot-server-programs '(python-mode . ("ruff" "server"))))

(after! eglot
    (add-hook 'elixir-mode-hook 'eglot-ensure)
    (add-to-list 'eglot-server-programs '(elixir-mode  "/home/kuba/.lsp/elixir")))

(map!
  :map lsp-mode-map
   :prefix "C-c"
      "c" (mklambdai (lsp-rust-analyzer--common-runner lsp)))

(load! "util/exercism.el")
(require 'dap-python)
(after! dap-mode
  (setq dap-python-debugger 'debugpy))

(after! elfeed
    (setq elfeed-search-filter "@2-week-ago")
    (setq elfeed-feeds
     '(("https://huggingface.co/blog/feed.xml" ml)
       ("https://news.mit.edu/topic/mitartificial-intelligence2-rss.xml" ml)
       ("https://nitter.ktachibana.party/GregKamradt/rss" ml llms)
       ("https://fetchrss.com/rss/65b2319bfa815b18a45b679265b231bafa815b18a45b6793.xml" nlp))))

(defcustom lsp-ruff-executable "ruff-lsp"
  "Command to start the Ruff language server."
  :group 'lsp-python
  :risky t
  :type 'file)

;; Register ruff-lsp with the LSP client.
(lsp-register-client
    (make-lsp-client
        :new-connection (lsp-stdio-connection (lambda () (list lsp-ruff-executable)))
        :activation-fn (lsp-activate-on "python")
        :add-on? t
        :server-id 'ruff
        :initialization-options (lambda ()
                                    (list :settings
                                        (cl-list*
                                          (when
                                            poetry-project-venv
                                                (list
                                                 :interpreter (vector (f-join (f-long poetry-project-venv) "bin" "python3"))
                                                 :workspace (f-long poetry-project-venv)
                                                 :path (vector (f-join (f-long poetry-project-venv) "bin" "ruff")))))))))
(require 'flycheck)
(flycheck-define-checker python-ruff
  "A Python syntax and style checker using the ruff utility.
To override the path to the ruff executable, set
`flycheck-python-ruff-executable'.
See URL `http://pypi.python.org/pypi/ruff'."
  :command ("ruff"
            "--format=text"
            (eval (when buffer-file-name
                    (concat "--stdin-filename=" buffer-file-name)))
            "-")
  :standard-input t
  :error-filter (lambda (errors)
                  (let ((errors (flycheck-sanitize-errors errors)))
                    (seq-map #'flycheck-flake8-fix-error-level errors)))
  :error-patterns
  ((warning line-start
            (file-name) ":" line ":" (optional column ":") " "
            (id (one-or-more (any alpha)) (one-or-more digit)) " "
            (message (one-or-more not-newline))
            line-end))
  :modes python-mode)

(add-to-list 'flycheck-checkers 'python-ruff)

(require 'poetry)

(after! ob-mermaid
    (setq ob-mermaid-cli-path paths/mmdc-path (shell-command-to-string)))

(after! lean4-mode
    (setq lean4-rootdir "/home/kuba/.elan/"))

;; prolog
(load! "modules/ediprolog/ediprolog.el")
(setq ediprolog-system 'swi)

;; latex

;; sage
(use-package! ob-sagemath
    :config
    (setq sage-shell:sage-executable "/home/kuba/micromamba/envs/sage/bin/sage")
    (setq org-babel-default-header-args:sage '((:session . t) (:results . "output")))
    (setq org-confirm-babel-evaluate nil)
    (setq org-export-babel-evaluate nil)
    (setq org-startup-with-inline-images t))
 
