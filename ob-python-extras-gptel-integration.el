;;; ob-python-extras-gptel-integration.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024
;;
;; Author:  <elle@etude>
;; Maintainer:  <elle@etude>
;; Created: November 29, 2024
;; Modified: November 29, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/elle/ob-python-extras-gptel-integration
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defun copy-org-babel-block-and-results ()
  "Copy the current org babel source block and its results."
  (interactive)
  (when (org-in-src-block-p)
    (let* ((src-block (org-element-context))
           (results (org-babel-where-is-src-block-result))
           (end (if results
                    (save-excursion
                      (goto-char results)
                      (forward-line)
                      (org-babel-result-end))
                  (org-element-property :end src-block))))
      (copy-region-as-kill
       (org-element-property :begin src-block)
       end)
      (message "Copied source block and results"))))

(defun init-cell-errors-gptel ()
  "Create a new gptel buffer for cell errors."
  (interactive)
  (funcall-interactively #'gptel "*CELL ERRORS*"))

(setq custom-block-failure-prompt "Please explain the issue, and please provide a corrected version of this, in a python block.\n")

(defun get-second-python-block ()
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (and (< count 2)
                  (re-search-forward "^[ \t]*#\\+begin_src python" nil t))
        (setq count (1+ count)))
      (when (= count 2)
        (let ((start (point)))
          (re-search-forward "^[ \t]*#\\+end_src" nil t)
          (buffer-substring-no-properties start (match-beginning 0)))))))

(defun 2nd-block ()
  (interactive)
  (message (get-second-python-block)))


(defun org-babel-get-src-block-end ()
  "Get the end position of the current source block."
  (save-excursion
    (org-babel-goto-src-block-head)
    (re-search-forward "#\\+end_src" nil t)
    (line-beginning-position)))

(defun send-block-to-gptel ()
  "Send org babel block with results to new gptel buffer without affecting current buffer."
  (interactive)
  (when (org-in-src-block-p)
    (let* ((src-block (org-element-context))
           (results (org-babel-where-is-src-block-result))
           (end (if results
                    (save-excursion
                      (goto-char results)
                      (forward-line)
                      (org-babel-result-end))
                  (org-element-property :end src-block)))
           (content (buffer-substring-no-properties
                     (org-element-property :begin src-block)
                     end))

           (buf (or (get-buffer "*CELL ERRORS*")
                    (progn (init-cell-errors-gptel)
                           (get-buffer "*CELL ERRORS*")))))

      (with-current-buffer buf
        (erase-buffer)
        (insert custom-block-failure-prompt)
        (insert content)
        (gptel-send)
        (pop-to-buffer buf)))))

(defun diff-strings (str1 str2)
  "Return a diff between STR1 and STR2 as a string."
  (with-temp-buffer
    (let ((file1 (make-temp-file "diff1"))
          (file2 (make-temp-file "diff2")))
      (with-temp-file file1 (insert str1))
      (with-temp-file file2 (insert str2))
      (call-process "diff" nil t nil "-u" file1 file2)
      (delete-file file1)
      (delete-file file2)
      (buffer-string))))

(defun patch-gptel-blocks ()
  "Send block to gptel and show diff with accept option."
  (interactive)
  (message "patching!!!")
  (let* ((original-buffer (current-buffer))
         (original-point (point))
         (original-content (when (org-in-src-block-p)
                             (org-babel-get-src-block-info 'light)
                             (let ((start (save-excursion
                                            (org-babel-goto-src-block-head)
                                            (forward-line 1)
                                            (point)))
                                   (end (org-babel-get-src-block-end)))
                               (buffer-substring-no-properties start end))))
         (gpt-block (with-current-buffer "*CELL ERRORS*"
                      (get-second-python-block)))
         (diff-buffer (get-buffer-create "*GPT Block Diff*"))
         (map (make-sparse-keymap)))
    (with-current-buffer diff-buffer
      (erase-buffer)
      (insert (diff-strings original-content gpt-block))
      (diff-mode)
      (define-key map (kbd "C-c C-c")
                  (lambda ()
                    (interactive)
                    (with-current-buffer original-buffer
                      (goto-char original-point)
                      (org-babel-goto-src-block-head)
                      (org-babel-remove-result)
                      (re-search-forward "#\\+begin_src.*$")
                      (forward-line 1)
                      (delete-region (point) (org-babel-get-src-block-end))
                      (insert gpt-block)
                      (quit-window)
                      (pop-to-buffer original-buffer)
                      )))
      (define-key map (kbd "C-c C-k")
                  (lambda ()
                    (interactive)
                    (quit-window)
                    (pop-to-buffer original-buffer)))
      (use-local-map (make-composed-keymap map (current-local-map)))
      (display-buffer diff-buffer)
      (pop-to-buffer diff-buffer))))

(defvar gptel-fix-block-buffer nil
  "Buffer to patch after GPT response.")

(defun gptel-fix-block-response (beg end)
  "Handle GPT response for block fixing."
  (when gptel-fix-block-buffer
    (with-current-buffer gptel-fix-block-buffer
      (patch-gptel-blocks))
    (pop-to-buffer "*GPT Block Diff*")
    (setq gptel-fix-block-buffer nil)))

(add-hook 'gptel-post-response-functions #'gptel-fix-block-response)

(defun gptel-fix-block ()
  "Send block to GPT and patch when response is received."
  (interactive)
  (setq gptel-fix-block-buffer (current-buffer))
  (send-block-to-gptel))


(provide 'ob-python-extras-gptel-integration)
;;; ob-python-extras-gptel-integration.el ends here
