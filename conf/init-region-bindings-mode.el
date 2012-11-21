(define-key region-bindings-mode-map "a" 'mc/mark-all-like-this)
(define-key region-bindings-mode-map "p" 'mc/mark-previous-like-this)
(define-key region-bindings-mode-map "n" 'mc/mark-next-like-this)
(define-key region-bindings-mode-map "P" 'mc/unmark-previous-like-this)
(define-key region-bindings-mode-map "N" 'mc/unmark-next-like-this)
(define-key region-bindings-mode-map "m" 'mc/mark-more-like-this-extended)
(setq region-bindings-mode-disabled-modes '(term-mode))
(setq region-bindings-mode-disable-predicates
      (list
       (lambda ()
         buffer-read-only)))
(region-bindings-mode-enable)
