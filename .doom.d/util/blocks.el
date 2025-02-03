;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; babel code blocks in org mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar default-code-block-args " :exports both")

(defun get-block-begin-end (&optional maybe-block-type)
    (let ((block-type (or maybe-block-type "SRC")))
        (list
            (format "#+BEGIN_%s" block-type)
            (format "#+END_%s" block-type))))



(defun insert-org-mode-block-with-content (code-block-args code-block-content &optional maybe-block-type)
    (pcase (get-block-begin-end maybe-block-type)
        (
            `(,code-block-start ,code-block-end)
            (let*
                (
                    (n-backward (+ 1 (length code-block-end))))
                (progn
                    (insert
                        (concat code-block-start " " code-block-args "\n" code-block-content "\n" code-block-end))
                    (backward-char n-backward))))))


(defun insert-org-ai-block ()
    (interactive)
    (insert-org-mode-block-with-content (concat ":service " (symbol-name org-ai-service)) "" "AI"))



(defvar default-code-block-args " :exports both")



(defun insert-babel-code-block (lang-name &optional maybe-session-name maybe-code-block-args async)
    (let
        ((code-block-args (or maybe-code-block-args default-code-block-args))
            (session-name (or maybe-session-name (buffer-name)))
            (use-async (if async " :async" "")))
        (insert-org-mode-block-with-content
            (concat lang-name " :session " session-name " " default-code-block-args use-async) "")))

