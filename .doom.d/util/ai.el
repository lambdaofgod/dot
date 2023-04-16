;;; util/ai.el -*- lexical-binding: t; -*-


(setq openai-api-key-path "~/.keys/openai_key.txt")
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
            (message (concat "loaded org-openai-api-token")))))
