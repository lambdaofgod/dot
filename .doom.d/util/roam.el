(require 'org-roam)
(require 'org-element)
(require 'org-id)


(defun concatenate-org-roam-files-recursively (file-id visited-ids current-depth &optional max-expanded-depth)
  "Return concatenated contents of the file with FILE-ID and its org-roam linked files recursively up to MAX-EXPANDED-DEPTH.
VISITED-IDS is a list of IDs visited so far to avoid infinite loops.
CURRENT-DEPTH is the current depth in the recursion."
  (unless max-expanded-depth (setq max-expanded-depth 2))
  (when (and file-id (not (member file-id visited-ids)))
    (with-temp-buffer
      (insert-file-contents (org-roam-node-file (org-roam-node-from-id file-id)))
      (let* ((file-contents (buffer-string))
             (linked-ids (org-roam-linked-ids))
             (visited-ids (cons file-id visited-ids))
             (linked-files-contents ""))
        (when (< current-depth max-expanded-depth)
          (dolist (linked-id linked-ids)
            (setq linked-files-contents
                  (concat linked-files-contents
                          (concatenate-org-roam-files-recursively linked-id visited-ids (1+ current-depth) max-expanded-depth)))))
        (concat file-contents "\n\n" linked-files-contents)))))

(defun my/org-roam-concatenate-files (&optional max-expanded-depth)
  "Return concatenated contents of the current buffer and all org-roam linked files recursively up to MAX-EXPANDED-DEPTH."
  (interactive "P")
  (let* ((current-buffer-contents (buffer-string))
         (current-file-id (org-roam-id-at-point))
         (concatenated-contents (concatenate-org-roam-files-recursively current-file-id nil 0 max-expanded-depth)))
    (with-current-buffer (get-buffer-create "*org-roam-concatenated*")
      (erase-buffer)
      (insert concatenated-contents)
      (goto-char (point-min))
      (org-mode)
      (display-buffer (current-buffer)))))
