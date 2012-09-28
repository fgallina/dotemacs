(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
      (list (append
             (cons "default"
                   ;; Generate filters by major modes from the
                   ;; auto-mode-alist
                   (let ((mode-filters))
                     (dolist (element auto-mode-alist)
                       (when (ignore-errors (fboundp (cdr element)))
                         (let* ((mode (cdr element))
                                (name (if (string-match "\\(-mode\\)?\\'"
                                                        (symbol-name mode))
                                          (capitalize
                                           (substring (symbol-name mode)
                                                      0 (match-beginning 0)))
                                        (symbol-name mode)))
                                (previous-value ))
                           (when (not (assoc-string name mode-filters))
                             (setq mode-filters
                                   (cons (list name (cons 'mode mode))
                                         mode-filters))))))
                     mode-filters))
             ;; Custom added filters.
             '(("Magit" (name . "^\\*magit"))
               ("Irc" (mode . rcirc-mode))
               ("Css" (mode . scss-mode))
               ("W3m" (name . "^\\*w3m"))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))
