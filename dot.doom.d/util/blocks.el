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
import openai
openai.api_key = open('%s/openai_key.txt').read().strip()

def get_chatgpt_response(text):

    completion = openai.ChatCompletion.create(
    model='gpt-3.5-turbo',
    messages=[{'role': 'user', 'content': text}]
    )
    return completion['choices'][0]['message']['content']
" org-directory))
(defvar chatgpt-args (concat "python :session chatgpt " default-code-block-args))


(defun initialize-chatgpt-code-block ()
    (interactive)
    (insert-org-mode-code-block-with-content chatgpt-args chatgpt-init-python-code))


(defun insert-chatgpt-response-code-block (chatgpt-query)
    (interactive "sAsk ChatGPT: ")
    (insert-org-mode-code-block-with-content chatgpt-args
        (format "get_chatgpt_response('%s')" chatgpt-query)))
