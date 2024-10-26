;;; ob-python-extras.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024
;;
;; Author: Elle Najt <LNAJT4@gmail.com>
;; Maintainer: Elle Najt <LNAJT4@gmail.com>
;; Created: October 25, 2024
;; Modified: October 25, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/ElleNajt/ob-python-extras
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

;;;; Navigation


(defun org-babel-goto-src-block-results ()
  (interactive)
  (goto-char (org-babel-where-is-src-block-result))
  )

(defun org-src-block-end-header (&optional element)
  (let ((element (or element (org-element-at-point))))
    (save-excursion
      (goto-char (org-element-end element))
      (re-search-backward (rx (and bol "#+END_SRC")))
      (point))))

(defun org-src-block-results-end (src-block)
  (save-excursion
    (goto-char (org-element-begin src-block))
    (when-let (results-loc (org-babel-where-is-src-block-result))
      (goto-char results-loc)
      (goto-char (org-element-end (org-element-at-point)))
      (skip-chars-backward " \t\n")
      (point))))

(defun insert-new-src-block ()
  (interactive)
  (let* ((current-src-block (org-element-at-point))
         (point-to-insert
          (or (org-src-block-results-end current-src-block)
              (save-excursion
                (goto-char (org-element-end current-src-block))
                (skip-chars-backward " \t\n")
                (point))))
         (src-block-head (save-excursion
                           (goto-char (org-element-property
                                       :begin current-src-block))
                           (let ((line (thing-at-point 'line t)))
                             (if (not (s-starts-with? "#+NAME:" (s-trim line)))
                                 line
                               (forward-line)
                               (thing-at-point 'line t))))))
    (goto-char point-to-insert)
    (insert "\n\n")
    (insert src-block-head)
    (let ((contents (point-marker)))
      (insert "\n#+end_src\n")
      (goto-char contents))))

(defun run-cell-and-advance () (interactive) (org-babel-execute-src-block) (org-babel-next-src-block) )

;;;; Cell timing and error handling


(defun wrap-org-babel-execute-python (orig body params &rest args)
  (let* ( (exec-file (make-temp-file "execution-code"))
          (timer-show (not (equal "no" (cdr (assq :timer-show params)))))
          (timer-string (cdr (assq :timer-string params)))
          (timer-string-formatted (if (not timer-string) "Cell Timer:" timer-string))
          (timer-rounded (not (equal "no" (cdr (assq :timer-rounded params)))))
          )
    (with-temp-file exec-file (insert body))
    (let* ((body (format "\
exec_file = \"%s\"
import time
# since this can cause collisions if something else in the python script gets named datetime
from datetime import datetime as org_babel_wrapper_datetime
start = org_babel_wrapper_datetime.now()
try:
    with open(exec_file, 'r') as file:
        exec(compile(file.read(), '<org babel source block>', 'exec'))
except:
    import traceback
    print(traceback.format_exc())
finally:
    if %s:
        timerstring = \"%s\"
        if %s:
            print(timerstring, str((org_babel_wrapper_datetime.now() - start)).split('.')[0], \"\\n\")
        else:
            print(timerstring, str((org_babel_wrapper_datetime.now() - start)), \"\\n\")
    import os
    try:
        os.remove(exec_file)
    except:
        pass" exec-file
        (if  timer-show "True" "False")
        timer-string-formatted
        (if  timer-rounded "True" "False")))
           (result (apply orig body params args)))
      result)))

(advice-add
 'org-babel-execute:python
 :around
 #'wrap-org-babel-execute-python)


;;;; Interruption

(defun org-babel-get-session ()
  (interactive)
  (let* ((src-info (org-babel-get-src-block-info))
         (headers (nth 2 src-info))
         (session (cdr (assoc :session headers))))
    session))


(defun interrupt-org-babel-session ()
  (interactive)
  (let* ((current-session (org-babel-get-session))
         (session-buffer (and current-session
                              (concat "*" current-session "*"))))
    (when session-buffer
      (let ((proc (get-buffer-process (get-buffer session-buffer))))
        (when proc
          (interrupt-process proc)
          (message "Interrupted session: %s" current-session))))))

;;;; mix printing images and text
;; TODO add handling for if python environment doesnt have matplotlib


(defun wrap-org-babel-execute-python-mock-plt (orig body &rest args)
  (let* ( (exec-file (make-temp-file "execution-code"))
          (pymockbabel-script-location (concat doom-user-dir "/python/pymockbabel")))
    (with-temp-file exec-file (insert body))
    (let* ((body (format "\
exec_file = \"%s\"
pymockbabel_script_location = \"%s\"
import sys
sys.path.append(pymockbabel_script_location)
import pymockbabel
outputs_and_file_paths, output_types, list_writer = pymockbabel.setup(\"%s\")
try:
    with open(exec_file, 'r') as file:
        exec(compile(file.read(), '<org babel source block>', 'exec'))
except:
    import traceback
    print(traceback.format_exc())
finally:
    pymockbabel.display(outputs_and_file_paths, output_types, list_writer)
    try:
        os.remove(exec_file)
    except:
        pass" exec-file pymockbabel-script-location (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
           (result (apply orig body args)))
      result)))

(advice-add
 'org-babel-execute:python
 :around
 #'wrap-org-babel-execute-python-mock-plt)

;;;;;; Garbage collection

(defun find-org-file-references ()
  "Find all file names referenced within [[]] in the current org buffer and return them as a list."
  (let (file-references)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[file:\\([^]]+\\)\\]\\]" nil t)
        (let ((file-name (match-string 1)))
          (push file-name file-references))))
    file-references))

(defun delete-unused-pngs-in-buffer (buffer)
  "Delete .png files in the /plots/ directory that are not referenced in the org file corresponding to BUFFER."
  (with-current-buffer buffer
    (when (and (eq major-mode 'org-mode) (buffer-file-name))
      (let* ((org-file (buffer-file-name))
             (org-file-name (file-name-sans-extension (file-name-nondirectory org-file)))
             (plots-dir (concat (file-name-directory org-file) "plots/" org-file-name))
             (referenced-files (find-org-file-references))
             (png-files (when (and (file-directory-p plots-dir) (file-exists-p plots-dir))
                          (directory-files plots-dir t "\\.png$"))))
        (when png-files
          (dolist (png-file png-files)
            (let ((relative-png-file (file-relative-name png-file (file-name-directory org-file))))
              (unless (member relative-png-file referenced-files)
                (delete-file png-file)
                (message "Deleted: %s" png-file)))))))))

(defun delete-unused-pngs-in-all-org-files ()
  "Delete unused .png files in all open org files."
  (interactive)
  (dolist (buffer (buffer-list))
    (delete-unused-pngs-in-buffer buffer)))

(run-at-time 300 300 'delete-unused-pngs-in-all-org-files)

;;;; Pandas dataframe plotting

(defun wrap-org-babel-execute-python-mock-table (orig body &rest args)
  (let* ( (exec-file (make-temp-file "execution-code"))
          (pymockbabel-script-location (concat straight-base-dir straight-build-dir "/ob-python-extras/python")))
    (with-temp-file exec-file (insert body))
    (let* ((body (format "\
exec_file = \"%s\"
pymockbabel_script_location = \"%s\"
import sys
sys.path.append(pymockbabel_script_location)
import print_org_df
print_org_df.enable()
try:
    with open(exec_file, 'r') as file:
        exec(compile(file.read(), '<org babel source block>', 'exec'))
except:
    import traceback
    print(traceback.format_exc())
finally:
    try:
        os.remove(exec_file)
    except:
        pass" exec-file pymockbabel-script-location (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
           (result (apply orig body args)))
      result)))

(advice-add
 'org-babel-execute:python
 :around
 #'wrap-org-babel-execute-python-mock-table)

;; (advice-remove 'org-babel-execute:python nil)

(defun my-align-advice-function (args)
  (let ((top-of-src-block (nth 5 args)))
    (save-excursion
      (with-current-buffer (current-buffer)
        (goto-char top-of-src-block)
        (forward-line 1)
        (let ((in-table nil))
          (while (and (= (forward-line) 0)
                      (not (looking-at "^[ \t]*:END:[ \t]*$")))
            (let ((at-table-line (looking-at "^[ \t]*|")))
              (when (and (not in-table) at-table-line)
                (org-table-align))
              (setq in-table at-table-line))))))))

;;;; Timestamps

;; Adding a space because otherwise it breaks org mode

(defun org-time-stamp-advice (orig-fun &rest args)
  "Ensure a space is inserted before the Org timestamp."
  (unless (or (bolp) (eq (char-before) ?\s))
    (insert " "))
  (apply orig-fun args))

(advice-add 'org-time-stamp :around #'org-time-stamp-advice)


;;;; Alerts


;;;;; alerts
;; Define a major mode for our alerts buffer
(define-derived-mode cell-alerts-mode special-mode "Cell Alerts"
  "Major mode for displaying cell completion alerts.")

(defun my-cell-finished-alert ()
  "Create an alert with an Emacs-native clickable link in a pop-up buffer when a code cell finishes."
  (let* ((buffer-name (buffer-name))
         (buffer-file (buffer-file-name))
         (line-number (line-number-at-pos))
         (link-text (if buffer-file
                        (format "%s:%d" buffer-file line-number)
                      buffer-name))
         (alerts-buffer-name "*Cell Completion Alerts*"))

    (with-current-buffer (get-buffer-create alerts-buffer-name)
      (unless (eq major-mode 'cell-alerts-mode)
        (cell-alerts-mode))
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
(after! evil
  (add-to-list 'evil-escape-excluded-major-modes 'cell-alerts-mode)
  (evil-set-initial-state 'cell-alerts-mode 'normal))

(after! (:and (:or evil-collection evil-integration) which-key)
  (map! :map cell-alerts-mode-map
        :n "q" #'quit-window
        :n [escape] #'quit-window))

;; Function to close the alerts buffer
(defun close-cell-alerts-buffer ()
  "Close the Cell Completion Alerts buffer from anywhere."
  (interactive)
  (when-let ((buffer (get-buffer "*Cell Completion Alerts*")))
    (when-let ((window (get-buffer-window buffer t)))
      (quit-window nil window))))

;; ESC key handling
(defadvice! my-universal-esc-handler (&rest _)
  :before #'keyboard-quit
  (when (get-buffer-window "*Cell Completion Alerts*" t)
    (close-cell-alerts-buffer)))

;; Set up the display rules for the alerts buffer
(set-popup-rule! "^\\*Cell Completion Alerts\\*$"
  :side 'bottom
  :size 0.3
  :select nil
  :quit t)

;;;;; cell timer based

(defun notify-if-took-a-while (alert-threshold)
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
                    (my-cell-finished-alert)
                  ()


                  )


                )))
        (message "No results block found.")
        nil))))

(defun cell-timer-above (params args lower_bound)
  "Display an alert if the cell timer is above lower_bound")

(defun alert-advice-after-org-babel-results (orig-fun params &rest args)
  (let*
      (( options (nth 2 (car args)))
       ( alert-finish (if (string= "yes" (cdr (assq :alert options))) t nil)))
    ;; this is a terrible hack
    ;; it happens to be that this argument is populated for the hash insert
    ;; and not for the content insert
    ;; I should refactor this to depend on hooks instead

    ;; if cell took a while always alert
    (if (not (nth 2 args))
        (if alert-finish
            (my-cell-finished-alert)
          ;; (message "alert finish!")

          ;; always alerts if the cell took a while (more than 30 seconds)
          (notify-if-took-a-while 10))
      ()
      ()) ()))

(advice-add 'org-babel-insert-result :after #'alert-advice-after-org-babel-results)
;; (setq debug-on-message "Code block evaluation complete\\.")


;;;; Post execution


;;;;; aligning and displaying iamges after output



(defun adjust-org-babel-results (orig-fun params &rest args)
  (let*

      (
       ( options (nth 2 (car args)))
       ( auto-align (if (string= "no" (cdr (assq :tables-auto-align options))) nil t))
       )

    ;; this is a terrible hack
    ;; it happens to be that this argument is populated for the hash insert
    ;; and not for the content insert
    ;; I should refactor this to depend on hooks instead
    (if (not (nth 2 args))
        (progn
          (if auto-align
              (my-align-advice-function (nth 0 args))
            ;; this is a built in that accomplishes the same task
            ;; but it operates on the entire org file,
            ;; which is slow and breaks hermeticism in a way I don't like
            ;; I tried narrowing the buffer, but it didn't work
            ;; (org-table-map-tables 'org-table-align)
            )
          )
      ())
    ())


  (org-display-inline-images))

(advice-add 'org-babel-insert-result :after #'adjust-org-babel-results)
;; (setq debug-on-message "Code block evaluation complete\\.")
;; (setq debug-on-message nil)
;; does async have a hook?


(provide 'ob-python-extras)
;;; ob-python-extras.el ends here
