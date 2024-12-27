;;; ob-python-extras-alerts.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024
;;
;; Author:  <elle@etude>
;; Maintainer:  <elle@etude>
;; Created: October 28, 2024
;; Modified: October 28, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/elle/ob-python-extras-alerts
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;;;; Alerts
(require 'doom-keybinds)


(define-derived-mode ob-python-extras/cell-alerts-mode special-mode "Cell Alerts"
  "Major mode for displaying cell completion alerts.")

(defun ob-python-extras/my-cell-finished-alert ()
  "Create an alert with an Emacs-native clickable link in a pop-up buffer when a code cell finishes."
  (let* ((buffer-name (buffer-name))
         (buffer-file (buffer-file-name))
         (line-number (line-number-at-pos))
         (link-text (if buffer-file
                        (format "%s:%d" buffer-file line-number)
                      buffer-name))
         (alerts-buffer-name "*Cell Completion Alerts*"))

    (with-current-buffer (get-buffer-create alerts-buffer-name)
      (unless (eq major-mode 'ob-python-extras/cell-alerts-mode)
        (ob-python-extras/cell-alerts-mode))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (let ((start (point)))
          (insert "\n\n")
          (insert (format-time-string "[%Y-%m-%d %H:%M:%S]\n"))
          (insert "A code cell finished at:\n")
          (shell-command (format  "notify-send \"An org cell in %s finished!\"" buffer-name))
          (insert-text-button link-text
                              'action (lambda (_)
                                        (if buffer-file
                                            (find-file-other-window buffer-file)
                                          (switch-to-buffer-other-window buffer-name))
                                        (when buffer-file
                                          (goto-char (point-min))
                                          (forward-line (1- line-number))))
                              'follow-link t
                              'help-echo "Click to go to the cell location")
          (put-text-property start (point) 'read-only t)))
      (let ((window (display-buffer-in-side-window (current-buffer) '((side . bottom)))))
        (when window
          (with-selected-window window
            (goto-char (point-max))
            (recenter -1))))))
  (message "Finished cell!"))

;; Doom Emacs specific configuration
(add-to-list 'evil-escape-excluded-major-modes 'ob-python-extras/cell-alerts-mode)
(evil-set-initial-state 'ob-python-extras/cell-alerts-mode 'normal)

(map! :map ob-python-extras/cell-alerts-mode-map
      :n "q" #'quit-window
      :n [escape] #'quit-window)

;; Function to close the alerts buffer
(defun ob-python-extras/close-cell-alerts-buffer ()
  "Close the Cell Completion Alerts buffer from anywhere."
  (interactive)
  (when-let ((buffer (get-buffer "*Cell Completion Alerts*")))
    (when-let ((window (get-buffer-window buffer t)))
      (quit-window nil window))))

;; ESC key handling
(defadvice! my-universal-esc-handler (&rest _)
  :before #'keyboard-quit
  (when (get-buffer-window "*Cell Completion Alerts*" t)
    (ob-python-extras/close-cell-alerts-buffer)))

;; Set up the display rules for the alerts buffer
(set-popup-rule! "^\\*Cell Completion Alerts\\*$"
  :side 'bottom
  :size 0.3
  :select nil
  :quit t)

;;;;; Alerts for long running cells

(defun ob-python-extras/notify-if-took-a-while (alert-threshold)
  "Scan through a results block to find a 'Cell Timer:' line and parse the time in seconds."
  (interactive)
  (save-excursion
    (let ((case-fold-search t))
      (if (search-forward-regexp "^[ \t]*#\\+RESULTS:" nil t)
          (let ((end (save-excursion
                       (if (search-forward-regexp "^[ \t]*#\\:END:" nil t)
                           (match-beginning 0)
                         (point-max)))))
            (when (search-forward-regexp "^Cell Timer:\\s-*\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)" end t)
              (let ((hours (string-to-number (match-string 1)))
                    (minutes (string-to-number (match-string 2)))
                    (seconds (string-to-number (match-string 3))))
                (+ (* hours 3600) (* minutes 60) seconds)
                (if (>= seconds alert-threshold)
                    (ob-python-extras/my-cell-finished-alert)
                  ()))))
        (message "No results block found.")
        nil))))

(defun ob-python-extras/alert-advice-after-org-babel-results (orig-fun params &rest args)
  (let* ((options (nth 2 (car args)))
         (alert-finish (if (string= "yes" (cdr (assq :alert options))) t nil))
         (src-block (org-element-at-point))
         (results-start (org-babel-where-is-src-block-result))
         (results-end (ob-python-extras/org-src-block-results-end src-block)))

    (when results-start
      (save-excursion
        (goto-char results-start)
        (if (re-search-forward "^[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}$" results-end t)
            ()
          (progn
            (when alert-finish
              (ob-python-extras/my-cell-finished-alert))
            (ob-python-extras/notify-if-took-a-while 10)))))))


(advice-add 'org-babel-insert-result :after #'ob-python-extras/alert-advice-after-org-babel-results)
;; (setq debug-on-message "Code block evaluation complete\\.")




(provide 'ob-python-extras-alerts)
;;; ob-python-extras-alerts.el ends here
