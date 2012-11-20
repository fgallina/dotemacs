(setq emerge-diff-ok-lines-regexp
      (concat
       "^\\("
       "[0-9,]+[acd][0-9,]+\C-m?$"
       "\\|[<>] "
       "\\|---"
       "\\|.*Warning *:"
       "\\|.*No +newline"
       "\\|.*missing +newline"
       "\\|^\C-m?$"
       "\\)"))

(setq emerge-diff3-ok-lines-regexp
      "^\\([1-3]:\\|====\\|  \\|.*Warning *:\\|.*No newline\\|.*missing newline\\|^\C-m$\\)")
