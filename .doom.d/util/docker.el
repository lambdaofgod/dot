;;;;;;;;;;;;;;;;;
;; docker-compose
;;;;;;;;;;;;;;;;;

;; mnemonics for running/building docker compose

(defvar docker-compose-buffer-name "*Docker Compose*")
(defun docker-compose-all-impl (down build daemon maybe-service)
    "runs docker-compose"
    (princ (format "maybe-service %s" maybe-service))
    (let*
        ((service-set (not (eq maybe-service "")))
         (buffer-name (if service-set (format "*Docker Compose-%s*" maybe-service) docker-compose-buffer-name))
         (as-daemon-str (if daemon "; -d" ""))
         (down-str (if down (format "docker-compose down %s;" maybe-service) ""))
         (build-str (if build (format "docker-compose build %s;" maybe-service) ""))
         (service-str (if service-set maybe-service (concat " ; " maybe-service)))
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


(defun build-project-dockerfiles ()
    (interactive)
    (let ((project-name (downcase (get-file-dirname (doom-modeline--project-root))))
          (dockerfiles (fd "Dockerfile")))
        (mapcar #'build-dockerfile dockerfiles)))


(defun build-dockerfile (path)
    (let* (
              (project-name (downcase (get-file-dirname doom-modeline--project-root)))
              (docker-dirname (get-file-dirname path))
              (tag (format "%s_%s" project-name docker-dirname))
              (command (format "cd %s; docker build -t %s ." docker-dirname tag))
              (shell-buffer-name (format "*Docker %s*" tag)))
        (when (get-buffer shell-buffer-name) (kill-buffer shell-buffer-name))
        (async-shell-command command shell-buffer-name)))
    


(defun build-docker-buffer ()
    (interactive)
    (build-dockerfile (buffer-file-name)))
