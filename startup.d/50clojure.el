(autoload 'clojure-mode "clojure-mode" nil t)
(autoload 'clojure-jack-in "clojure-mode" nil t)
(autoload 'clojure-enable-slime-on-existing-buffers "clojure-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-hook 'slime-connected-hook 'clojure-enable-slime-on-existing-buffers)
(add-to-list 'interpreter-mode-alist '("cake" . clojure-mode))
