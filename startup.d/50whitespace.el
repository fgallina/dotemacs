(require 'whitespace)
(set 'whitespace-style '(trailing tabs lines-tail indentation::space face))
(setq whitespace-global-modes
      '(c-mode c++-mode clojure-mode emacs-lisp-mode js-mode php-mode
               python-mode lisp-mode))
(global-whitespace-mode 1)
