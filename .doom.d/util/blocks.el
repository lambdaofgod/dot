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
    (insert-org-mode-block-with-content "" "" "AI"))



(defvar default-code-block-args " :exports both")



(defun insert-babel-code-block (lang-name &optional maybe-session-name maybe-code-block-args)
    (let
        ((code-block-args (or maybe-code-block-args default-code-block-args))
         (session-name (or maybe-session-name (buffer-name))))
      (insert-org-mode-block-with-content
        (concat lang-name " :session " session-name " " default-code-block-args) "")))

;;;;;;;;;;
;; ChatGPT
;;;;;;;;;;

(defvar chatgpt/init-python-code (format "
from mlutil import chatgpt_api 
api_key_path = '%s' # specify file path if OPENAI_API_KEY is not in env

chatgpt_client = chatgpt_api.ChatGPTClient(api_key_path)
'chatgpt ready'" "~/.keys/openai_key.txt"))
(defvar chatgpt/args (concat "python :session chatgpt " default-code-block-args))


(defun chatgpt/initialize-api ()
    (interactive)
    (insert-org-mode-block-with-content chatgpt/args chatgpt/init-python-code))


(defun chatgpt/insert-response-code-block (chatgpt-query)
    (interactive "sAsk ChatGPT: ")
    (insert-org-mode-block-with-content chatgpt/args
        (format "chatgpt_client.get_chatgpt_response_from_text('%s')" chatgpt-query)))

