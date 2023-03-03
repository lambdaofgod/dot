;;;;;;;;;;;;;;;;;
;; docker-compose
;;;;;;;;;;;;;;;;;

;; mnemonics for running/building docker compose

(defun docker-compose-all-impl (down build daemon maybe-service)
    "runs docker-compose"
    (princ (format "maybe-service %s" maybe-service))
    (let*
        ((buffer-name "Docker compose")
         (as-daemon-str (if daemon "; -d" ""))
         (down-str (if down "docker-compose down;" ""))
         (build-str (if build "docker-compose build;" ""))
         (service-str (if (eq maybe-service "") maybe-service (concat " ; " maybe-service)))
         (command (concat down-str build-str "docker-compose up" as-daemon-str)))
        (progn
            (when (get-buffer buffer-name) (kill-buffer buffer-name))
            (async-shell-command command)
            (rename-async-buffer-with-truncated-lines buffer-name))))

(defun dup (maybe-service)
    "docker compose up"
    (interactive "sService name: ")
    (docker-compose-all-impl nil nil nil maybe-service))
(defun dupd (maybe-service)
    "docker compose down then up in daemon"
    (interactive "sService name: ")
    (docker-compose-all-impl nil nil t maybe-service))
(defun ddup (maybe-service)
    "docker compose down; build then up"
    (interactive "sService name: ")
    (docker-compose-all-impl t t nil maybe-service))
(defun ddupd (maybe-service)
    "docker compose down; build then up in daemon"
    (interactive "sService name: ")
    (docker-compose-all-impl t t t maybe-service))
