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
(require 'doom-keybinds)

;;;; Helpers
(defun ob-python-extras/find-python-scripts-dir ()
  "Find the directory containing Python scripts for ob-python-extras."
  (or
   ;; Check if path explicitly set (e.g. for tests/batch mode)
   (bound-and-true-p ob-python-extras-python-path)

   ;; Check straight.el installation
   (when (bound-and-true-p straight-base-dir)
     (let ((straight-python-dir
            (expand-file-name (concat straight-build-dir "/ob-python-extras/python")
                              straight-base-dir)))
       (when (file-directory-p straight-python-dir)
         straight-python-dir)))

   ;; Check package.el installation
   (when-let ((lib-path (locate-library "ob-python-extras")))
     (let ((package-python-dir
            (expand-file-name "python"
                              (file-name-directory lib-path))))
       (when (file-directory-p package-python-dir)
         package-python-dir)))

   ;; Error if nothing found
   (error "Cannot find ob-python-extras Python scripts directory. Please set ob-python-extras-python-path")))

;;;; Navigation

(defun ob-python-extras/org-babel-goto-src-block-results ()
  "Go to the results of the current source block."
  (interactive)
  (goto-char (org-babel-where-is-src-block-result)))

(defun ob-python-extras/org-src-block-end-header (&optional element)

  (let ((element (or element (org-element-at-point))))
    (save-excursion
      (goto-char (org-element-end element))
      (re-search-backward (rx (and bol "#+END_SRC")))
      (point))))

(defun ob-python-extras/org-src-block-results-end (src-block)
  (save-excursion
    (goto-char (org-element-begin src-block))
    (when-let (results-loc (org-babel-where-is-src-block-result))
      (goto-char results-loc)
      (goto-char (org-element-end (org-element-at-point)))
      (skip-chars-backward " \t\n")
      (point))))

(defun ob-python-extras/insert-new-src-block ()
  "Create a new org babel source block beow the current one."
  (interactive)
  (let* ((current-src-block (org-element-at-point))
         (point-to-insert
          (or (ob-python-extras/org-src-block-results-end current-src-block)
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

(defun ob-python-extras/run-cell-and-advance () (interactive) (org-babel-execute-src-block) (org-babel-next-src-block) )

;;;; Cell timing and error handling


(defun ob-python-extras/wrap-org-babel-execute-python (orig body params &rest args)
  (let* ( (exec-file (make-temp-file "execution-code"))
          (timer-show (not (equal "no" (cdr (assq :timer-show params)))))
          (timer-string (cdr (assq :timer-string params)))
          (timer-string-formatted (if (not timer-string) "Cell Timer:" timer-string))
          (error-options (when-let ((err (cdr (assq :errors params))))
                           (split-string err " " t)))
          (use-rich (member "rich" error-options))
          (show-locals (not (member "no-locals" error-options)))
          (show-full-paths (member "full-paths" error-options))

          (extra-lines (or (and-let* ((extra (member "extra" error-options))
                                      (num (cadr extra)))
                             (string-to-number num))
                           3))
          (max-frames (or (and-let* ((max (member "frames" error-options))
                                     (num (cadr max)))
                            (string-to-number num))
                          100))
          (timer-rounded (not (equal "no" (cdr (assq :timer-rounded params))))))
    (with-temp-file exec-file (insert body))
    (message "extra lines %s" extra-lines)
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
    if %s:
        try:
            from rich.console import Console as Rich_Console
            from rich.traceback import Traceback as Rich_Traceback
            rich_console = Rich_Console()
            rich_console.print(Rich_Traceback(
                show_locals=%s,
                max_frames=%d,
                extra_lines=%d,
                word_wrap=False,
            ))
        except ImportError:
            import traceback
            print(traceback.format_exc())
    else:
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
        (if use-rich "True" "False")

        (if show-locals "True" "False")
        max-frames
        extra-lines
        (if  timer-show "True" "False")
        timer-string-formatted
        (if  timer-rounded "True" "False")))
           (result (apply orig body params args)))
      result)))

(advice-add 'org-babel-execute:python
            :around #'ob-python-extras/wrap-org-babel-execute-python
            '((depth . -100)))


;;;; Interruption

(defun ob-python-extras/org-babel-get-session ()
  (interactive)
  (let* ((src-info (org-babel-get-src-block-info))
         (headers (nth 2 src-info))
         (session (cdr (assoc :session headers))))
    session))


(defun ob-python-extras/interrupt-org-babel-session ()
  (interactive)
  (let* ((current-session (ob-python-extras/org-babel-get-session))
         (session-buffer (and current-session
                              (concat "*" current-session "*"))))
    (when session-buffer
      (let ((proc (get-buffer-process (get-buffer session-buffer))))
        (when proc
          (interrupt-process proc)
          (message "Interrupted session: %s" current-session))))))


;;;; Better output handling
;;;;; mix printing images and text

(defun ob-python-extras/wrap-org-babel-execute-python-mock-plt (orig body &rest args)
  (let* ((exec-file (make-temp-file "execution-code"))
         (pymockbabel-script-location (ob-python-extras/find-python-scripts-dir)))
    (let* ((body (format "\
exec_file = \"%s\"
pymockbabel_script_location = \"%s\"
import sys
sys.path.append(pymockbabel_script_location)
import pymockbabel
outputs_and_file_paths, output_types, list_writer = pymockbabel.setup(\"%s\")
with open(exec_file, 'r') as file:
    exec(compile(file.read(), '<org babel source block>', 'exec'))
pymockbabel.display(outputs_and_file_paths, output_types, list_writer)"
                         exec-file
                         pymockbabel-script-location
                         (file-name-sans-extension (file-name-nondirectory buffer-file-name)))))
      (apply orig body args))))

;;;;;; Image Garbage collection

(defun ob-python-extras/find-org-file-references ()
  "Find all file names referenced within [[]] in the current org buffer and return them as a list."
  (let (file-references)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[file:\\([^]]+\\)\\]\\]" nil t)
        (let ((file-name (match-string 1)))
          (push file-name file-references))))
    file-references))

(defun ob-python-extras/delete-unused-pngs-in-buffer (buffer)
  "Delete .png files in the /plots/ directory that are not referenced in the org file corresponding to BUFFER."
  (with-current-buffer buffer
    (when (and (eq major-mode 'org-mode) (buffer-file-name))
      (let* ((org-file (buffer-file-name))
             (org-file-name (file-name-sans-extension (file-name-nondirectory org-file)))
             (plots-dir (concat (file-name-directory org-file) "plots/" org-file-name))
             (referenced-files (ob-python-extras/find-org-file-references))
             (png-files (when (and (file-directory-p plots-dir) (file-exists-p plots-dir))
                          (directory-files plots-dir t "\\.png$"))))
        (when png-files
          (dolist (png-file png-files)
            (let ((relative-png-file (file-relative-name png-file (file-name-directory org-file))))
              (unless (member relative-png-file referenced-files)
                (delete-file png-file)
                (message "Deleted: %s" png-file)))))))))

(defun ob-python-extras/delete-unused-pngs-in-all-org-files ()
  "Delete unused .png files in all open org files."
  (interactive)
  (dolist (buffer (buffer-list))
    (ob-python-extras/delete-unused-pngs-in-buffer buffer)))

(run-at-time 300 300 'ob-python-extras/delete-unused-pngs-in-all-org-files)

;;;;; Pandas dataframe printing


(defun ob-python-extras/wrap-org-babel-execute-python-mock-table (orig body &rest args)
  (let* ((exec-file (make-temp-file "execution-code"))
         (pymockbabel-script-location (ob-python-extras/find-python-scripts-dir)))
    (with-temp-file exec-file (insert body))
    (let* ((body (format "\
exec_file = \"%s\"
pymockbabel_script_location = \"%s\"
import sys
sys.path.append(pymockbabel_script_location)
import print_org_df
print_org_df.enable()
with open(exec_file, 'r') as file:
     exec(compile(file.read(), '<org babel source block>', 'exec')) " exec-file pymockbabel-script-location (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
           (result (apply orig body args)))
      result)))

(advice-add
 'org-babel-execute:python
 :around
 #'ob-python-extras/wrap-org-babel-execute-python-mock-table)

(defun ob-python-extras/my-align-advice-function (args)
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




;;;;; aligning and displaying iamges after output

(defun ob-python-extras/adjust-org-babel-results (orig-fun params &rest args)
  (let*

      (( options (nth 2 (car args)))
       ( auto-align (if (string= "no" (cdr (assq :tables-auto-align options))) nil t)))

    ;; this is a terrible hack
    ;; it happens to be that this argument is populated for the hash insert
    ;; and not for the content insert
    ;; I should refactor this to depend on hooks instead
    (if (not (nth 2 args))
        (progn
          (if auto-align
              (ob-python-extras/my-align-advice-function (nth 0 args))
            ;; this is a built in that accomplishes the same task
            ;; but it operates on the entire org file,
            ;; which is slow
            ;; I tried narrowing the buffer, but it didn't work
            ;; (org-table-map-tables 'org-table-align)
            ))
      ())
    ())


  (org-display-inline-images))

(advice-add 'org-babel-insert-result :after #'ob-python-extras/adjust-org-babel-results)

;;; Enlarging images

(defun org-view-image-full-size ()
  "Enlarge an image in an org buffer to a new buffer at full size."
  (interactive)
  (when (eq major-mode 'org-mode)
    (when-let* ((link (org-element-context))
                ((eq (org-element-type link) 'link))
                (path (org-element-property :path link))
                (full-path (expand-file-name path))
                ((file-exists-p full-path))
                ((string-match-p "\\.\\(png\\|jpeg\\|jpg\\|gif\\)$" full-path)))
      (find-file-other-window full-path)
      (image-mode)
      (image-transform-fit-to-height)
      t)))

(defun org-dispatch-C-c-C-c (arg)
  "Helper function that wraps the usual C-c-C-c behavior in org to add image viewing as well."
  (interactive "P")
  (if (eq major-mode 'org-mode)
      (unless (org-view-image-full-size)
        (org-ctrl-c-ctrl-c))))

;;;


;;; Keybindings



(defun ob-python-extras/+org-insert-item (orig direction)
  (interactive)
  (if (and (org-in-src-block-p)
           (equal direction 'below))
      (ob-python-extras/insert-new-src-block)
    (funcall orig direction)))

(defun ob-python-extras/map-suggested-keyindings ()
  "Map suggested keybindings for ob-python."
  (interactive)

  (advice-add #'+org--insert-item :around #'ob-python-extras/+org-insert-item)

  (map! :map org-mode-map
        :after org
        :n "<S-return>" #'ob-python-extras/run-cell-and-advance
        :n "SPC S" #'jupyter-org-split-src-block
        :n "SPC M" #'jupyter-org-merge-blocks
        :n "g SPC" #'org-babel-execute-buffer
        :n "C-c C-k" #'ob-python-extras/interrupt-org-babel-session
        :n "SPC f i"  #'org-toggle-inline-images
        :n "SPC f I"  #'org-display-inline-images
        :nvi "C-c C-c" #'org-dispatch-C-c-C-c
        :nv "SPC o g f" 'gptel-fix-block
        :nv "SPC o g s" 'send-block-to-gptel
        :nv "SPC o g p" 'patch-gptel-blocks
        :nv "SPC o g d" 'patch-gptel-blocks
        :n "g s"  #'org-edit-special))

(ob-python-extras/map-suggested-keyindings)


(provide 'ob-python-extras)
;;; ob-python-extras.el ends here
