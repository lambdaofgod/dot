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


(defun org/insert-template (heading-names)
    (interactive (list (read-string "Heading names " "TL;DR ")))
    (org/insert-named-sections (split-string heading-names)))
