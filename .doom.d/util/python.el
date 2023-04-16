;;; util/python.el -*- lexical-binding: t; -*-


(defun make-setup-content (project-name user-name author)
    (format "from setuptools import setup, find_packages

with open(\"requirements.txt\") as f:
    requirements = f.read().splitlines()

setup(
    name=\"%s\",
    version=\"0.0.1\",
    %s
    %s
    packages=find_packages(),
    install_requires=requirements,
)"
                                          project-name
                                          (if user-name (format "url=\"https://github.com/%s/%s\"," user-name project-name) "")
                                          (if author (format "author=\"%s\"," author) "")))


(defun create-setup-file (project-name)
    (let* (
              (user-name (read-string "" nil))
              (author (read-string "" nil))
              (setup-contents (make-setup-content project-name user-name author))))
    (with-temp-buffer
        (insert setup-contents)
        (write-file "setup.py")))
