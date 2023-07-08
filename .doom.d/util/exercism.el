(cl-defun open-exercism-exercise-impl (&optional (test-command "mix test"))
  "Vertically split the window, open README and run tests."
  (let* ( (file-name (buffer-file-name)) (dir (file-name-directory file-name)))
    (progn);; If there is no window on the right, split the window vertically
    ;; and then horizontally on the right window
    (unless (window-in-direction 'right)
        (split-window-right)

        ;; Switch to the right window
        (windmove-right)

        ;; Split the right window horizontally
        (split-window-below)
        (windmove-left))

    (windmove-right)
    ;; Open the README in the right-top window
    (find-file (concat dir "../README.md"))

    ;; Switch to the right-bottom window
    (windmove-down)

    ;; Start a shell in the bottom window
    (shell)

    ;; Change the shell directory
    ;; Run the tests
    (comint-send-string nil (concat "cd ..; while inotifywait -e modify " file-name "; do " test-command "; done \n"))

    ;; Switch back to the left window
    (windmove-left)))

(defun open-exercism-exercise ()
     (interactive)
     (open-exercism-exercise-impl))
