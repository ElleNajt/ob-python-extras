;;; ob-python-extras-alerts-keybindings.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 
;;
;; Author:  <elle@etude>
;; Maintainer:  <elle@etude>
;; Created: February 17, 2025
;; Modified: February 17, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/elle/ob-python-extras-alerts-keybindings
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  
;;
;;; Code:


(require 'evil)
(require 'org)

(after! (evil org)
  (defun ob-python-extras/map-suggested-keyindings ()
    "Map suggested keybindings for ob-python."
    (interactive)
    ;; (require 'evil)

    (advice-add #'+org--insert-item :around #'ob-python-extras/+org-insert-item)

    (evil-define-key* '(normal visual) org-mode-map
      (kbd "<S-return>") #'ob-python-extras/run-cell-and-advance
      (kbd "SPC S") #'ob-python-extras/split-block
      (kbd "SPC M k") #'join-source-block-to-previous
      (kbd "SPC M j") #'join-source-block-to-next
      (kbd "g SPC") #'org-babel-execute-buffer
      (kbd "g d") #'ob-python-extras/goto-definition-dispatcher
      (kbd "C-c C-k") #'ob-python-extras/interrupt-org-babel-session
      (kbd "SPC f i") #'org-toggle-inline-images
      (kbd "SPC f I") #'org-display-inline-images
      (kbd "C-c C-c") #'org-dispatch-C-c-C-c
      (kbd "SPC o s") #'ob-python-extras/open-session-buffer
      (kbd "SPC o g f") 'ob-python-extras/gptel-fix-block
      (kbd "SPC o g s") 'ob-python-extras/send-block-to-gptel
      (kbd "SPC o g p") 'ob-python-extras/patch-gptel-blocks
      (kbd "g s") #'org-edit-special)

    (map! 
     :map org-mode-map
     [remap +lookup/documentation] #'ob-python-extras/help-dispatcher   )))

(provide 'ob-python-extras-alerts-keybindings)
;;; ob-python-extras-alerts-keybindings.el ends here
