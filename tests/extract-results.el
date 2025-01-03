;;; extract-results.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025
;;
;; Author:  <elle@etude>
;; Maintainer:  <elle@etude>
;; Created: January 03, 2025
;; Modified: January 03, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/elle/extract-results
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'org)
(require 'json)
(require 'ob-core)

;; TODO Implement expect skip lines, and skip based on property of cells

(require 'org)
(require 'json)
(require 'ob-core)

(defun normalize-result-string (result-string)
  "Normalize result string, handling PNG file references specially."
  (if (and result-string
           (string-match "\\[\\[file:\\(.*\\.png\\)\\]\\]" result-string))
      ;; For PNG results, just keep the filename part
      (match-string 1 result-string)
    result-string))

(defun org-babel-get-src-block-result ()
  (interactive)
  (let ((result-pos (org-babel-where-is-src-block-result)))
    (when result-pos
      (save-excursion
        (goto-char result-pos)
        (forward-line)
        (normalize-result-string
         (buffer-substring-no-properties
          (point)
          (org-babel-result-end)))))))

(defun extract-named-results ()
  "Extract all named results blocks as an alist of name -> result content"
  (let (results)
    (org-babel-map-src-blocks nil
      (let ((name (org-element-property :name (org-element-context))))
        (when name
          (push (cons name (org-babel-get-src-block-result)) results))))
    (prin1-to-string results)))

(with-current-buffer (find-file-noselect (car command-line-args-left))
  (print (extract-named-results)))

(provide 'extract-results)
;;; extract-results.el ends here
