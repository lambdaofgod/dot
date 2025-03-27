;;;;;;;;;;
;; agents
;;;;;;;;;;

(defvar agents/init-python-code (format "
from mlutil import chatgpt_api
api_key_path = '%s' # specify file path if OPENAI_API_KEY is not in env

chatgpt_client = chatgpt_api.ChatGPTClient(api_key_path)
'chatgpt ready'" "~/.keys/openai_key.txt"))
(defvar agents/args (concat "python :session agents :async" default-code-block-args))


(defun agents/initialize-api ()
    (interactive)
    (insert-org-mode-block-with-content chatgpt/args chatgpt/init-python-code))


(defun agents/insert-response-code-block (chatgpt-query)
    (interactive "sAsk ChatGPT: ")
    (insert-org-mode-block-with-content chatgpt/args
        (format "chatgpt_client.get_chatgpt_response_from_text('%s')" chatgpt-query)))
