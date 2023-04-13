(require 'eww)

(defun eww-vertical-split (url)
  "Browse the URL with EWW in a vertical split window."
  (interactive "sEnter URL: ")
  (let ((buffer (url-encode-url url)))
    (setq eww-vertical-split-buffer (get-buffer-create (concat "*eww-" buffer "*")))
    (with-current-buffer eww-vertical-split-buffer
      (eww-mode)
      (eww url))
    (display-buffer eww-vertical-split-buffer
                    '((display-buffer-reuse-window
                       display-buffer-pop-up-window
                       display-buffer-at-bottom)
                      (reusable-frames . visible)
                      (split-window-sensibly . t)
                      (window-width . 0.5)))
    (select-window (get-buffer-window eww-vertical-split-buffer))))

(global-set-key (kbd "C-x w") 'eww-vertical-split)
