;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; babel code blocks in org mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-org-mode-code-block-with-content (code-block-args code-block-content)
    (let* (
              (code-block-start "#+BEGIN_SRC")
              (code-block-end "#+END_SRC")
              (n-backward (+ 1 (length code-block-end))))
        (progn
            (insert
                (concat code-block-start " " code-block-args "\n" code-block-content "\n" code-block-end))
            (backward-char n-backward))))
(defvar default-code-block-args " :results both drawer :exports both")



(defun insert-lang-org-mode-code (lang-name session-name)
    (insert-org-mode-code-block-with-content
        (concat lang-name " :session " session-name " " default-code-block-args) ""))

;;;;;;;;;;
;; ChatGPT
;;;;;;;;;;

(defvar chatgpt-init-python-code (format "
from mlutil import chatgpt_api 
api_key_path = '%s' # specify file path if OPENAI_API_KEY is not in env

chatgpt_client = chatgpt_api.ChatGPTClient(api_key_path, logger=logging.info)
" org-directory))
(defvar chatgpt-args (concat "python :session chatgpt " default-code-block-args))


(defun initialize-chatgpt-code-block ()
    (interactive)
    (insert-org-mode-code-block-with-content chatgpt-args chatgpt-init-python-code))


(defun insert-chatgpt-response-code-block (chatgpt-query)
    (interactive "sAsk ChatGPT: ")
    (insert-org-mode-code-block-with-content chatgpt-args
        (format "chatgpt_client.get_chatgpt_response_from_text('%s')" chatgpt-query)))
