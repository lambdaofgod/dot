;;; util/ai.el -*- lexical-binding: t; -*-

;; org-ai
(setq openai-api-key-path "~/.keys/openai_key.txt")
(setq anthropic-api-key-path "~/.keys/anthropic_key.txt")
(setq anthropic-api-key-path "~/.keys/anthropic_key.txt")
(setq gemini-api-key-path "~/.keys/gemini_api_key.txt")
(setq perplexity-api-key-path "~/.keys/perplexity_key.txt")

(defvar org-ai-openai-api-token "")

(defun get-key-from-file (api-key-path)
    (with-temp-buffer
        (insert-file-contents api-key-path)
        (string-trim (buffer-string))))


(defun org-ai/set-openai-key ()
    (interactive)
    (let ((openai-key-path (read-string "openai key path, default:" openai-api-key-path)))
        (progn
            (setq org-ai-openai-api-token (get-key-from-file openai-key-path))
            (message (concat "loaded org-openai-api-token"))
            (org-ai-switch-chat-model))))

(defun org-ai/set-anthropic-key-envvar ()
    (interactive)
    (let ((anthropic-api-key (get-key-from-file anthropic-api-key-path)))
        (setenv "ANTHROPIC_API_KEY" anthropic-api-key)
        ))


(defun org-ai/set-anthropic-key ()
    (interactive)
    (let ((anthropic-key-path (read-string "anthropic key path, default:" anthropic-api-key-path)))
        (progn
            (setq org-ai-anthropic-api-token (get-key-from-file anthropic-key-path))
            (message (concat "set anthropic api token"))
            (setq org-ai-openai-api-token org-ai-anthropic-api-token)
            (setq org-ai-service "anthropic")
            (org-ai-switch-chat-model))))


(defun org-ai/set-perplexity-key ()
    (interactive)
    (let ((perplexity-key-path (read-string "perplexity key path, default:" perplexity-api-key-path)))
        (progn
            (setq org-ai-perplexity-api-token (get-key-from-file perplexity-key-path))
            (message (concat "set perplexity api token"))
            (setq org-ai-openai-api-token org-ai-perplexity-api-token)
            (setq org-ai-service 'perplexity.ai)
            (org-ai-switch-chat-model))))


;; gptel
(defun gptel/setup-gemini ()
    (interactive)
    (let ((gemini-key-path (read-string "gemini key path, default:" gemini-api-key-path)))
        (setq
            gptel-default-mode 'org-mode
            gptel-model 'gemini-2.0-flash-exp
            gptel-backend (gptel-make-gemini "Gemini"
                              :key (get-key-from-file gemini-key-path)
                              :stream t))))
