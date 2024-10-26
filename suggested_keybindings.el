;;; suggested_keybindings.elk -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024
;;
;; Author:  <elle@etude>
;; Maintainer:  <elle@etude>
;; Created: October 25, 2024
;; Modified: October 25, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/elle/suggested_keybindings
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

;;;; Key bindings:

(map! (:mode org-mode
       :n "<S-return>" #'run-cell-and-advance
       :n "SPC S" #'jupyter-org-split-src-block
       :n "SPC M" #'jupyter-org-merge-blocks
       :n "g SPC" #'org-babel-execute-buffer
       :n "C-c C-k" #'interrupt-org-babel-session))

(map! (:mode org-mode
       :n "SPC f i"  #'org-toggle-inline-images
       :n "SPC f I"  #'org-display-inline-images
       :n "SPC f t" #'my-align-tables
       :n "g s"  #'org-edit-special
       :n "] c" 'evil-next-flyspell-error
       :n "[ c" 'evil-prev-flyspell-error
       ))

(provide 'suggested_keybindings)
;;; suggested_keybindings.el ends here
