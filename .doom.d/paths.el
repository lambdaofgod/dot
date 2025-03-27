;;; paths.el -*- lexical-binding: t; -*-

(if (eq system-type 'darwin)
    (setq paths/mmdc-path "/opt/homebrew/bin/mmdc")
    (setq paths/mmdc-path "/home/kuba/.volta/bin/mmdc"))
