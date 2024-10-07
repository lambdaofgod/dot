;;; util/ai.el -*- lexical-binding: t; -*-


(setq openai-api-key-path "~/.keys/openai_key.txt")
(setq anthropic-api-key-path "~/.keys/anthropic_key.txt")

(defvar org-ai-openai-api-token "")

(defun get-key-from-file (api-key-path)
  (with-temp-buffer
    (insert-file-contents api-key-path)
    (string-trim (buffer-string))))
  

(defun set-openai-key ()
    (interactive)
    (let ((openai-key-path (read-string "openai key path, default:" openai-api-key-path)))
        (progn
            (setq org-ai-openai-api-token (get-key-from-file openai-key-path))
            (message (concat "loaded org-openai-api-token"))
            (org-ai-switch-chat-model))))


(defun set-anthropic-key ()
    (interactive)
    (let ((anthropic-key-path (read-string "openai key path, default:" anthropic-api-key-path)))
        (progn
            (setq org-ai-anthropic-api-token (get-key-from-file anthropic-key-path))
            (message (concat "loaded org-anthropic-api-token"))
            (setq claude-shell-api-token org-ai-anthropic-api-token))))
