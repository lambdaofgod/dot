(defun org/associated-tangle-filename ()
    (cdr (assoc :tangle (org-babel-parse-header-arguments (buffer-string)))))

(defun org/goto-tangle-filename ()
    (interactive)
    (find-file (org/associated-tangle-filename)))


(defun org/insert-heading-with-name (name)
    (progn (org-insert-heading-respect-content)
        (insert name)))

(defun org/insert-named-sections (section-names)
    (progn
        (org-insert-subheading "")
        (insert (car section-names))
        (mapcar #'org/insert-heading-with-name (cdr section-names))))


(defvar org/paper-report-heading-names
    (list
        "TL;DR"
        "Evaluation"
        "Datasets"
        "Results"
        "Methods"))


(defvar org/roam-heading-names
    (list
        "TL;DR"
        "Links"
        "Datasets"
        "Results"
        "Methods"))

(defun org/insert-paper-report-template ()
    (interactive)
    (org/insert-named-sections org/paper-report-heading-names))

(defun org/insert-roam-template ()
    (interactive)
    (org/insert-named-sections org/roam-heading-names))


(defun org/insert-template (separator heading-names)
    (interactive
        (list
            (read-string "Separator (default \"+\"): " "+" nil "+")
            (read-string "Heading names:" "TL;DR" nil "TL;DR")))
    (org/insert-named-sections (split-string heading-names separator)))


(defun org/goto-tangle-file ()
  "open the file specified in the `tangle' header property in a new buffer."
  (interactive)
  (let* ((header-args-str (org-entry-get (point) "header-args" :inherit))
         (tangled-file (nth 1 (s-split " " header-args-str))))
    (if tangled-file
        (find-file tangled-file))
    (print tangled-file)))


(defun org/store-link-to-current-line ()
  (interactive)
  (let* ((current-file (buffer-file-name))
         (current-line (line-number-at-pos))
         (org-link (format "[[%s::%s][%s - Line %s]]"
                          current-file
                          current-line
                          (file-name-nondirectory current-file)
                          current-line)))
    (setq tmp/org-link org-link)
    (message "Org link stored: %s" org-link)))

(defun org/insert-stored-link ()
  (interactive)
  (insert tmp/org-link))
