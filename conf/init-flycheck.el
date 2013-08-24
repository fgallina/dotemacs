(require 'flycheck)
(require 'python)


;; Add virtualenv support for checkers
(defadvice flycheck-check-executable
  (around python-flycheck-check-executable (checker)
          activate compile)
  "`flycheck-check-executable' with virtualenv support."
  (if (eq major-mode 'python-mode)
      (let* ((process-environment (python-shell-calculate-process-environment))
             (exec-path (python-shell-calculate-exec-path)))
        ad-do-it)
    ad-do-it))

(defadvice flycheck-start-checker
  (around python-flycheck-start-checker (checker)
          activate compile)
  "`flycheck-start-checker' with virtualenv support."
  (if (eq major-mode 'python-mode)
      (let* ((process-environment (python-shell-calculate-process-environment))
             (exec-path (python-shell-calculate-exec-path)))
        ad-do-it)
    ad-do-it))


(global-flycheck-mode 1)
