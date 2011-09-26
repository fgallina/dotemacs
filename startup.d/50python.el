(setenv "PYMACS_PYTHON" "python2")

(require 'pymacs)
(require 'python)
(require 'flymake)

(setq python-shell-interpreter "python2")

(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

(pymacs-load "ropemacs" "rope-")
(setq ropemacs-codeassist-maxfixes 5
      ropemacs-guess-project t
      ropemacs-enable-autoimport t
      ropemacs-completing-read-function 'completing-read)

(defun try-complete-ropemacs (old)
  (save-excursion
    (unless old
      (he-init-string (he-dabbrev-beg) (point))
      (when (not (equal he-search-string ""))
        (setq he-expand-list
              (sort (all-completions
                     he-search-string
                     (delete-duplicates
                      (mapcar
                       (lambda (completion)
                         (concat he-search-string
                                 (replace-regexp-in-string
                                  "^[\r\n\t ]+\\|[\r\n\t ]+$" ""
                                  (nth 0 (split-string completion ":")))))
                       (ignore-errors
                         (rope-completions)))
                      :test 'string=))
                    'string-lessp))))
    (while (and he-expand-list
                (he-string-member (car he-expand-list) he-tried-table))
      (setq he-expand-list (cdr he-expand-list)))
    (if (null he-expand-list)
        (progn (if old (he-reset-string)) ())
      (progn
        (he-substitute-string (car he-expand-list))
        (setq he-tried-table (cons (car he-expand-list)
                                   (cdr he-tried-table)))
        t))))


(remove-hook 'python-mode-hook 'ac-ropemacs-setup)
(defvar ac-ropemacs-completions-cache nil)
(defvar ac-source-ropemacs
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

(defun flymake-python-init ()
  (let* ((process-environment (python-shell-calculate-process-environment))
         (exec-path (python-shell-calculate-exec-path))
         (checker (executable-find "python-check")))
    (when checker
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        (list checker (list local-file))))))

(add-to-list 'flymake-allowed-file-name-masks
             '("\\.py\\'" flymake-python-init))

(defun python-flymake-setup ()
  (interactive)
  (flymake-mode)
  (local-set-key [S-up]
                 (lambda ()
                   (interactive)
                   (flymake-goto-prev-error)
                   (message "%s"
                            (flymake-ler-text
                             (caar (flymake-find-err-info
                                    flymake-err-info
                                    (flymake-current-line-no)))))))
  (local-set-key [S-down]
                 (lambda ()
                   (interactive)
                   (flymake-goto-next-error)
                   (message "%s"
                            (flymake-ler-text
                             (caar (flymake-find-err-info
                                    flymake-err-info
                                    (flymake-current-line-no))))))))

(defadvice flymake-start-syntax-check-process (around python-flymake-start-syntax-check-process
                                                      (cmd args dir))
  "`flymake-start-syntax-check-process' with virtualenv support."
  (if (eq major-mode 'python-mode)
      (let* ((process-environment (python-shell-calculate-process-environment))
             (exec-path (python-shell-calculate-exec-path)))
        ad-do-it)
    ad-do-it))
(ad-activate 'flymake-start-syntax-check-process)

(defun python-setup ()
  (set (make-local-variable 'hippie-expand-try-functions-list)
       '(yas/hippie-try-expand
         try-complete-file-name
         try-complete-ropemacs))
  (setq ac-sources '(ac-source-ropemacs ac-source-yasnippet ac-source-filename)))

(add-hook 'python-mode-hook 'python-flymake-setup)
(add-hook 'python-mode-hook 'python-setup)

(setq pdb-path '/usr/lib/python2.7/pdb.py
      gud-pdb-command-name (symbol-name pdb-path))

(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline pdb-path (file-name-nondirectory
				      buffer-file-name)))))
