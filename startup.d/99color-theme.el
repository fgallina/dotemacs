;; Color Theme
;; Light themes:
;; + color-theme-bharadwaj
;; + color-theme-gray30
;; + color-theme-gtk-ide
;; Dark themes:
;; + color-theme-clarity
;; + color-theme-charcoal-black
;; + color-theme-dark-blue2
;; + color-theme-late-night
;; + color-theme-subtle-hacker

(setq color-theme-load-all-themes nil)
(require 'color-theme)
(require 'color-theme-tangotango)

(defun color-theme-my-setup (module &optional variant)
  (let ((theme (if variant
                   (intern (format "%s-%s" module variant))
                 module)))
    ;; select theme - first list element is for windowing system, second
    ;; is for console/terminal. Source:
    ;; http://www.emacswiki.org/emacs/ColorTheme#toc9
    (setq color-theme-choices `(,theme ,theme))
    ;; test for each additional frame or console
    (require 'cl)
    (fset 'test-win-sys
          (funcall (lambda (cols)
                     (lexical-let ((cols cols))
                       (lambda (frame)
                         (let ((color-theme-is-global nil))
                           ;; must be current for local ctheme
                           (select-frame frame)
                           ;; test winsystem
                           (eval
                            (append '(if (window-system frame))
                                    (mapcar (lambda (x) (cons x nil))
                                            cols)))))))
                   color-theme-choices))
    ;; hook on after-make-frame-functions
    (add-hook 'after-make-frame-functions 'test-win-sys)
    (funcall theme)))

(color-theme-my-setup 'color-theme-tangotango)
