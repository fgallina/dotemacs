(require 'python)
(require 'flymake)

(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)


;;; Flymake
(defun python-flymake-init ()
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

(defadvice flymake-start-syntax-check-process
  (around python-flymake-start-syntax-check-process (cmd args dir)
          activate compile)
  "`flymake-start-syntax-check-process' with virtualenv support."
  (if (eq major-mode 'python-mode)
      (let* ((process-environment (python-shell-calculate-process-environment))
             (exec-path (python-shell-calculate-exec-path)))
        ad-do-it)
    ad-do-it))

(add-to-list 'flymake-allowed-file-name-masks
             '("\\.py\\'" python-flymake-init))


;;; Hooks
(remove-hook 'python-mode-hook 'wisent-python-default-setup)
(add-hook 'python-mode-hook 'jedi:setup)
