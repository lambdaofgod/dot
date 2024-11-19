;;; util/ai.el -*- lexical-binding: t; -*-


(setq org-ai/openai-api-key-path "~/.keys/openai_key.txt")
(setq org-ai/anthropic-api-key-path "~/.keys/anthropic_key.txt")
(setq org-ai/service "anthropic")


(defvar org-ai-openai-api-token "")

(defun get-key-from-file (api-key-path)
    (with-temp-buffer
        (insert-file-contents api-key-path)
        (string-trim (buffer-string))))


(defun org-ai/set-openai-key ()
    (interactive)
    (let ((openai-key-path (read-string "openai key path, default:" org-ai/openai-api-key-path)))
        (progn
            (setq org-ai-openai-api-token (get-key-from-file openai-key-path))
            (message (concat "loaded org-openai-api-token"))
            (org-ai-switch-chat-model))))


(defun org-ai/set-anthropic-key ()
    (interactive)
    (let ((anthropic-key-path (read-string "anthropic key path, default:" org-ai/anthropic-api-key-path)))
        (progn
            (setq org-ai-anthropic-api-token (get-key-from-file anthropic-key-path))
            (message (concat "set anthropic api token"))
            (setq org-ai-openai-api-token org-ai-anthropic-api-token)
            (setq org-ai-service "anthropic")
            (org-ai-switch-chat-model))))
