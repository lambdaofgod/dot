


(defun insert-heading-with-name (name)
    (progn (org-insert-heading-respect-content)
        (insert name)))

(defun insert-named-sections (section-names)
    (progn
        (org-insert-subheading "")
        (insert (car section-names))
        (mapcar #'insert-heading-with-name (cdr section-names))))


(defvar paper-report-heading-names
    (list
        "TL;DR"
        "Evaluation"
        "Datasets"
        "Results"
        "Methods"))


(defun insert-paper-report-template ()
    (interactive)
    (insert-named-sections paper-report-heading-names))
