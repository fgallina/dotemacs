(add-to-list 'load-path "~/.emacs.d/vendor/org-mode/lisp/")
(setq org-export-docbook-xsl-fo-proc-command "fop %s %s")
(setq org-export-docbook-xslt-proc-command
      "xsltproc --output %s /usr/share/xml/docbook/xsl-stylesheets-1.76.1/xhtml/docbook.xsl %s")

