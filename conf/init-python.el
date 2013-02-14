(require 'pymacs)
(require 'python)

(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

(setq python-shell-interpreter "python2")

(pymacs-load "ropemacs" "rope-")

(setq ropemacs-codeassist-maxfixes 5
      ropemacs-guess-project t
      ropemacs-enable-autoimport t
      ropemacs-completing-read-function 'ido-completing-read)

(remove-hook 'python-mode-hook 'ac-ropemacs-setup)
(setq ac-ropemacs-completions-cache nil)
(setq ac-source-ropemacs
  '((init
     . (lambda ()
         (setq ac-ropemacs-completions-cache
               (delete-duplicates
                (mapcar
                 (lambda (completion)
                   (concat ac-prefix
                           (replace-regexp-in-string
                            "^[\r\n\t ]+\\|[\r\n\t ]+$" ""
                            (nth 0 (split-string completion ":")))))
                 (ignore-errors
                   (rope-completions)))
                :test 'string=))))
    (symbol . "p")
    (candidates . ac-ropemacs-completions-cache)))

(remove-hook 'python-mode-hook 'wisent-python-default-setup)

(add-hook 'python-mode-hook 'jedi:setup)

(setq pdb-path '/usr/lib/python2.7/pdb.py
      gud-pdb-command-name (symbol-name pdb-path))

(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline
          pdb-path (file-name-nondirectory buffer-file-name)))))
