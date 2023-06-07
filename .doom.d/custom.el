(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(counsel-projectile-org-capture-templates
      '(("t" "[${name}] Task" entry
            (file+headline "${root}/notes.org" "Tasks")
            "* TODO %?
  %u
  %a" :tree-type week)))
 '(org-agenda-files
      '("/home/kuba/Projects/org/chatgpt_conversations.org" "/home/kuba/Projects/org/MLOps.org" "/home/kuba/Projects/org/all.org" "/home/kuba/Projects/org/books.org" "/home/kuba/Projects/org/buddyzm.org" "/home/kuba/Projects/org/clearml.org" "/home/kuba/Projects/org/ddd.org" "/home/kuba/Projects/org/december.org" "/home/kuba/Projects/org/dziennik.org" "/home/kuba/Projects/org/dziennik.sync-conflict-20230220-194237-RWPYY6S.org" "/home/kuba/Projects/org/dziennik_projektow.org" "/home/kuba/Projects/org/feels.org" "/home/kuba/Projects/org/filozofia.org" "/home/kuba/Projects/org/grammar.org" "/home/kuba/Projects/org/grudzien.org" "/home/kuba/Projects/org/ideas.org" "/home/kuba/Projects/org/inbox.org" "/home/kuba/Projects/org/inne.org" "/home/kuba/Projects/org/it_stuff.org" "/home/kuba/Projects/org/jedzenie.org" "/home/kuba/Projects/org/kanban.org" "/home/kuba/Projects/org/life.org" "/home/kuba/Projects/org/ling.org" "/home/kuba/Projects/org/links.org" "/home/kuba/Projects/org/magisterka_tmp.org" "/home/kuba/Projects/org/math.org" "/home/kuba/Projects/org/networks.org" "/home/kuba/Projects/org/notebook.org" "/home/kuba/Projects/org/notes.org" "/home/kuba/Projects/org/odds_ends.org" "/home/kuba/Projects/org/pomysły.org" "/home/kuba/Projects/org/pomysły_na_książki.org" "/home/kuba/Projects/org/projects_notes.org" "/home/kuba/Projects/org/projekty.org" "/home/kuba/Projects/org/przepisy.org" "/home/kuba/Projects/org/python_architecture.org" "/home/kuba/Projects/org/różne.org" "/home/kuba/Projects/org/smart_notes.org" "/home/kuba/Projects/org/statystyka.org" "/home/kuba/Projects/org/styczen.org" "/home/kuba/Projects/org/tmp.org" "/home/kuba/Projects/org/tools.org" "/home/kuba/Projects/org/tutorial.org" "/home/kuba/Projects/org/unipipe.org" "/home/kuba/Projects/org/work.org"))
 '(org-capture-templates
      '(("t" "Personal todo" entry
            (file+headline +org-capture-todo-file "Inbox")
            "* [ ] %U %?
%i
%a" :prepend t)
           ("n" "Personal notes" entry
               (file+headline +org-capture-notes-file "Inbox")
               "* %u %?
%i
%a" :prepend t)
           ("j" "Journal" entry
               (file+olp+datetree +org-capture-journal-file)
               "* %U %?
%i
%a" :tree-type week)
           ("p" "Templates for projects")
           ("pt" "Project-local todo" entry
               (file+headline +org-capture-project-todo-file "Inbox")
               "* TODO %?
%i
%a" :prepend t)
           ("pn" "Project-local notes" entry
               (file+headline +org-capture-project-notes-file "Inbox")
               "* %U %?
%i
%a" :prepend t)
           ("pc" "Project-local changelog" entry
               (file+headline +org-capture-project-changelog-file "Unreleased")
               "* %U %?
%i
%a" :prepend t)
           ("o" "Centralized templates for projects")
           ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?
 %i
 %a" :heading "Tasks" :prepend nil)
           ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?
 %i
 %a" :prepend t :heading "Notes")
           ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?
 %i
 %a" :prepend t :heading "Changelog")))
 '(org-safe-remote-resources '("\\`https://fniessen\\.github\\.io\\(?:/\\|\\'\\)"))
 '(package-selected-packages
      '(conda org-roam-timestamps mmm-mode org-fancy-priorities company-tabnine org-roam-ui pyenv-mode org-ai nix-mode nixpkgs-fmt ob-async helm-youtube request hyperbole xterm-color docker-tramp org-ref python-black aggressive-indent elisp-format blacken)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
