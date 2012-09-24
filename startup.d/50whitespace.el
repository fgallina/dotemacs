(require 'whitespace)
(setq whitespace-style '(trailing tabs indentation::space face))
(setq whitespace-global-modes
      '(c-mode c++-mode clojure-mode emacs-lisp-mode js-mode php-mode
               python-mode lisp-mode))
(global-whitespace-mode 1)
