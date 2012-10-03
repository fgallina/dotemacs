(defadvice er/prepare-for-more-expansions-internal
  (after my:er/prepare-for-more-expansions-internal activate)
  "Add additional expand-region fast commands.
Namely `mark-previous-like-this', `mark-next-like-this'
`mark-all-like-this'."
  (let ((msg (concat
              (car ad-return-value)
              ", p/P to mark/unmark prev, n/P to mark/unmark next, a to mark all" ))
        (bindings (cdr ad-return-value)))
    (push (cons "p" '(mark-previous-like-this 1)) bindings)
    (push (cons "n" '(mark-next-like-this 1)) bindings)
    (push (cons "P" '(mark-previous-like-this -1)) bindings)
    (push (cons "N" '(mark-next-like-this -1)) bindings)
    (push (cons "a" '(mark-all-like-this)) bindings)
    (setq ad-return-value (cons msg bindings))))
