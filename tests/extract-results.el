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


(defun normalize-result-string (result-string)
  "Normalize result string, preserving all content."
  (when result-string
    (string-trim result-string)))

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

(defun split-result-into-segments (result-string)
  "Split result into list of (type . content) pairs with numbered keys."
  (when result-string
    (let ((segments nil)
          (counter 1))
      (dolist (line (split-string result-string "\n"))
        (unless (string-prefix-p "%expect_skip" line)
          (if (string-match "\\[\\[file:\\(.*\\.png\\)\\]\\]" line)
              (push (cons (format "L%d_png" counter) (match-string 1 line)) segments)
            (push (cons (format "L%d_text" counter) line) segments))
          (setq counter (1+ counter))))
      (nreverse segments))))

(defun extract-named-results ()
  "Extract all named results blocks as an alist of name -> segmented content"
  (let (results)
    (org-babel-map-src-blocks nil
      (let ((name (org-element-property :name (org-element-context))))
        (when name
          (let ((result (org-babel-get-src-block-result)))
            (push (cons name (split-result-into-segments result))
                  results)))))
    (json-encode results)))

(with-current-buffer (find-file-noselect (car command-line-args-left))
  (print (extract-named-results)))

(provide 'extract-results)
;;; extract-results.el ends here
