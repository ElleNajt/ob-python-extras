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
          (last-executed (not (member (cdr (assq :last-executed params)) '(nil "no"))))
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
    (let* ((body (format "\
__exec_file = \"%s\"
import time
# since this can cause collisions if something else in the python script gets named datetime
from datetime import datetime as __org_babel_wrapper_datetime
__start = __org_babel_wrapper_datetime.now()
try:
    with open(__exec_file, 'r') as __file:
        exec(compile(__file.read(), '<org babel source block>', 'exec'))
except:
    if %s:
        try:
            from rich.console import Console as __Rich_Console
            from rich.traceback import Traceback as __Rich_Traceback
            rich_console = __Rich_Console()
            rich_console.print(__Rich_Traceback(
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
        __timer_string = \"%s\"
        if %s:
            print(f\"{__timer_string} {str((__org_babel_wrapper_datetime.now() - __start)).split('.')[0]}\")
        else:
            print(f\"{__timer_string} {str((__org_babel_wrapper_datetime.now() - __start))}\")
    if %s:
        print(f\"Last run at: {__org_babel_wrapper_datetime.now()}\")
    import os
    try:
        os.remove(__exec_file)
    except:
        pass" exec-file
        (if use-rich "True" "False")

        (if show-locals "True" "False")
        max-frames
        extra-lines
        (if  timer-show "True" "False")
        timer-string-formatted
        (if  timer-rounded "True" "False")
        (if last-executed "True" "False")))
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
  (if (bound-and-true-p org-capture-mode)
      (org-capture-kill)
    (let* ((current-session (ob-python-extras/org-babel-get-session))
           (session-buffer (and current-session
                                (concat "*" current-session "*"))))
      (when session-buffer
        (let ((proc (get-buffer-process (get-buffer session-buffer))))
          (when proc
            (interrupt-process proc)
            (message "Interrupted session: %s" current-session)))))))


;;; open session buffer

(defun ob-python-extras/open-session-buffer ()
  (interactive)
  (let ((session (ob-python-extras/org-babel-get-session)))
    (when session
      (pop-to-buffer (format "*%s*" session)))))

;;;; Better output handling
;;;;; mix printing images and text

(defun ob-python-extras/wrap-org-babel-execute-python-mock-plt (orig body params &rest args)
  (let* ((exec-file (make-temp-file "execution-code"))
         (pymockbabel-script-location (ob-python-extras/find-python-scripts-dir))
         (src-info (org-babel-get-src-block-info))
         (headers (nth 2 src-info))
         (transparent-header (assoc :transparent headers))
         (max-lines (cdr (assoc :max-lines params)))
         (max-lines (if (and (numberp max-lines) (> max-lines 0))
                        max-lines
                      
                      "None")))
    (with-temp-file exec-file (insert body))
    (let* ((body (format "\
__exec_file = \"%s\"
__pymock_babel_script_location = \"%s\"
import os
import sys
sys.path.append(__pymock_babel_script_location)
import pymockbabel as __pymockbabel 
__outputs_and_file_paths, __output_types, __list_writer = __pymockbabel.setup(\"%s\"%s)
with open(__exec_file, 'r') as __file:
    exec(compile(__file.read(), '<org babel source block>', 'exec'))
__pymockbabel.display(__outputs_and_file_paths, __output_types, __list_writer, max_lines = %s)
try:
    os.remove(__exec_file)
except:
    pass "
                         exec-file
                         pymockbabel-script-location
                         (file-name-sans-extension (file-name-nondirectory buffer-file-name))
                         (concat ", transparent="
                                 (if transparent-header (if (equal (cdr transparent-header) "nil") "False" "True")
                                   (if (and (boundp 'ob-python-extras/transparent-images) ob-python-extras/transparent-images) "True" "False")))

                         max-lines

                         )))
      (apply orig body params args))))



(advice-add 'org-babel-execute:python
            :around #'ob-python-extras/wrap-org-babel-execute-python-mock-plt
            '((depth . -5)))

;; TODO Refactor these advice so that there is a single one!


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
  "Delete unused .png files in all open org files if deletion is enabled."
  (interactive)
  (when ob-python-extras/allow-png-deletion
    (dolist (buffer (buffer-list))
      (ob-python-extras/delete-unused-pngs-in-buffer buffer))))

(setq ob-python-extras/allow-png-deletion nil) ; to disable deletion

(run-at-time 300 300 'ob-python-extras/delete-unused-pngs-in-all-org-files)

;;;;; Pandas dataframe printing


(defun ob-python-extras/wrap-org-babel-execute-python-mock-table (orig body &rest args)
  (let* ((exec-file (make-temp-file "execution-code"))
         (pymockbabel-script-location (ob-python-extras/find-python-scripts-dir)))
    (with-temp-file exec-file (insert body))
    (let* ((body (format "\
__exec_file = \"%s\"
__pymockbabel_script_location = \"%s\"
import sys
sys.path.append(__pymockbabel_script_location)
import print_org_df as __print_org_df
__print_org_df.enable()
with open(__exec_file, 'r') as __file:
     exec(compile(__file.read(), '''<%s: org babel source block> ''', 'exec')) " exec-file pymockbabel-script-location (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
           (result (apply orig body args)))
      result)))

(advice-add
 'org-babel-execute:python
 :around
 #'ob-python-extras/wrap-org-babel-execute-python-mock-table
 '((depth . -10)))

(defun ob-python-extras/my-align-advice-function (args)
  (let ((top-of-src-block (nth 5 args)))
    (save-excursion
      (with-current-buffer (current-buffer)
        (goto-char top-of-src-block)
        (if (re-search-forward "^#\\+end_src" nil t)
            (forward-line 1)
          ())
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

    (progn
      (if auto-align
          (ob-python-extras/my-align-advice-function (nth 0 args))
        ;; org-table-map-tables is a built in that accomplishes the same task
        ;; (org-table-map-tables 'org-table-align)
        ;; but it operates on the entire org file,
        ;; and it is slow
        ;; I tried narrowing the buffer, but it didn't work
        ()))
    ())

  (if (and (fboundp 'ob-python-extras/send-block-to-gptel) ob-python-extras/auto-send-on-traceback)
      (if (ob-python-extras/check-traceback)
          (ob-python-extras/send-block-to-gptel) ())
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
  "Helper function that wraps C-c C-c behavior.
In org-capture-mode, finalizes capture.
In regular org-mode, tries to view image or executes normal C-c C-c."
  (interactive "P")
  (if (bound-and-true-p org-capture-mode)
      (org-capture-finalize)
    (unless (org-view-image-full-size)
      (org-ctrl-c-ctrl-c))))


;;; Keybindings



(defun ob-python-extras/+org-insert-item (orig direction)
  (interactive)
  (if (and (org-in-src-block-p)
           (equal direction 'below))
      (ob-python-extras/insert-new-src-block)
    (funcall orig direction)))

(setq ob-python-extras/auto-send-on-traceback t)

(defun ob-python-extras/split-block ()
  "split block -- org-babel-demarcate-block does not work"
  (interactive)
  (let* ((elem (org-element-at-point))
         (lang (org-element-property :language elem))
         (params (org-element-property :parameters elem)))

    (when (eq (org-element-type elem) 'src-block)
      (let ((beg (org-element-property :begin elem))
            (end (org-element-property :end elem)))
        (insert (format "#+end_src\n\n#+begin_src %s %s\n" lang (or  params "")))
        (org-indent-region beg (point))))))

(defun join-source-block-to-previous ()
  (interactive)
  (save-excursion
    (org-babel-previous-src-block)
    (org-babel-remove-result)
    (forward-line 1)
    (let ((start (progn (search-forward "#+end_src")
                        (beginning-of-line)
                        (point)))
          (end (progn (search-forward "#+begin_src")
                      (end-of-line)
                      (forward-char)
                      (point))))
      (delete-region start end))))

(defun join-source-block-to-next ()
  (interactive)
  (save-excursion
    (org-babel-next-src-block)
    (join-source-block-to-previous)))

;;; dir-locals in org-special

;; Heavily inspired by this thread: https://github.com/joaotavora/eglot/issues/216#issuecomment-1052931508

(defun ob-python-extras/advice-setq-locals-python (orig-fun &rest args)
  "Advice to set Python-related local variables before running org-edit-src-code."
  (let* ((info (org-babel-get-src-block-info t))
         (headers (nth 2 info))
         (python-cmd (alist-get :python headers))
         (cmd-parts (split-string python-cmd))
         (temp-file (make-temp-file "org-python-" nil ".py"))
         (edit-buffer-name (format "*Org Src %s[ %s ]*" (buffer-name) (nth 0 info))))
    (prog1 (apply orig-fun args)
      (when python-cmd
        (with-current-buffer edit-buffer-name
          (when (eq major-mode 'python-mode)
            (setq-local buffer-file-name temp-file)
            (setq-local python-shell-interpreter (car cmd-parts))
            (setq-local python-shell-interpreter-args (string-join (cdr cmd-parts) " "))
            (setq-local eglot-server-programs
                        (list (list (list 'python-mode)
                                    (car cmd-parts)
                                    "--run"
                                    "python"
                                    "--run"
                                    "pyright-langserver --stdio")))
            (eglot-ensure)
            (message "Python settings updated with: %s" python-cmd)))))))


(defun ob-python-extras/add-org-edit-special-advice ()
  (interactive)
  (advice-add 'org-edit-special :around #'ob-python-extras/advice-setq-locals-python))

;;; Pandoc conversion script

(defun run-ipynb-to-org-conversion-script ()
  (interactive)
  (when (dired-mode-p)
    (let* ((current-dir (dired-current-directory))
           (ob-python-extras-dir (file-name-directory (locate-library "ob-python-extras")))
           (script-path (concat ob-python-extras-dir "bashscripts/convert_ipynb_to_org.sh" )))
      (compile (concat "cd " current-dir " && "script-path " -c")))))

(defun run-ipynb-to-org-conversion-script-recursively ()
  (interactive)
  (when (dired-mode-p)
    (let* ((current-dir (dired-current-directory))
           (ob-python-extras-dir (file-name-directory (locate-library "ob-python-extras")))
           (script-path (concat ob-python-extras-dir "bashscripts/convert_ipynb_to_org.sh" )))
      (compile (concat "cd " current-dir " && "script-path " -cr")))))

(defun run-org-to-ipynb-conversion-script ()
  (interactive)
  (when (dired-mode-p)
    (let* ((current-dir (dired-current-directory))
           (ob-python-extras-dir (file-name-directory (locate-library "ob-python-extras")))
           (script-path (concat ob-python-extras-dir "bashscripts/convert_org_to_ipynb.sh" )))
      (compile (concat "cd " current-dir " && "script-path)))))

;;; Load other packages

(defun ob-python-extras-load-gptel-integration ()
  "Load gptel integrations for ob-python-extras."
  (let* ((this-file (locate-library "ob-python-extras"))
         (this-dir (file-name-directory this-file)))
    (load (expand-file-name "ob-python-extras-gptel-integration" this-dir))))


(defun ob-python-extras-load-alerts ()
  "Load alerts integrations for ob-python-extras."
  (let* ((this-file (locate-library "ob-python-extras"))
         (this-dir (file-name-directory this-file)))
    (load (expand-file-name "ob-python-extras-alerts" this-dir))))

(defun ob-python-extras-load-keybindings ()
  "Load alerts integrations for ob-python-extras."
  (let* ((this-file (locate-library "ob-python-extras"))
         (this-dir (file-name-directory this-file)))
    (load (expand-file-name "ob-python-extras-keybindings" this-dir))))

;;; Auto formatting

;; TODO Get this to work in emacs batch
;; TODO Add ruff

(when (condition-case nil
          (require 'python-black nil t)
        (error nil))

  (defvar ob-python-extras/auto-format t
    "When non-nil, automatically format Python source blocks after execution.")

  (defun ob-python-extras--format-src-block ()
    "Format the current org babel Python source block using python-black.
Creates a temporary buffer, sets python-mode, applies formatting, and copies back."
    (interactive)

    (when ob-python-extras/auto-format
      (let* ((element (org-element-at-point))
             (language (org-element-property :language element))
             (orig-code (org-element-property :value element))
             (point-pos (point)))
        ;;  save excursion isn't sufficient to save the position
        (when (string= language "python")
          (let ((formatted-code
                 (with-temp-buffer
                   (insert orig-code)
                   (python-mode)
                   (python-black-buffer)
                   (buffer-string))))
            (save-excursion
              (goto-char (org-element-property :begin element))
              (org-babel-update-block-body formatted-code))
            (goto-char point-pos)
            )))))

  (add-hook 'org-babel-after-execute-hook 'ob-python-extras--format-src-block))


;;; help functionality
;; this is incredibly jank but useful for getting help at point


(defun ob-python-extras/help-dispatcher ()
  "Dispatches to the python one to not overwrite workin +lookup/definition in elisp blocks."
  (interactive)
  (if (and (org-in-src-block-p)
           (string= "python" (org-element-property :language (org-element-at-point))))
      (ob-python-extras/python-help-clean)
    ;; TODO This is pretty doom specific, I think.
    (call-interactively #'+lookup/documentation)))


(defun ob-python-extras/python-help-clean ()
  (interactive)
  (let* ((session-name (ob-python-extras/org-babel-get-session))
         (session-buffer (get-buffer (format "*%s*" session-name)))
         (python-process (when session-buffer
                           (get-buffer-process session-buffer)))
         (symbol (python-info-current-symbol))
         (help-command (format "
import sys
from io import StringIO
__help_output = StringIO()
sys.stdout = __help_output
help(%s)
sys.stdout = sys.__stdout__
print(__help_output.getvalue())
" symbol))
         ;; it would be better if I could get the *path* to the documentation
         ;; and open in a buffer with less
         ;;  TODO Maybe look at eldoc at point in python.el ?
         (output (when (and session-buffer python-process symbol)
                   (python-shell-send-string-no-output help-command python-process))))
    
    (when output
      (with-current-buffer (get-buffer-create "*Python Help*")
        (erase-buffer)
        (insert output)
        (goto-char (point-min))
        (while (re-search-forward "\\(.\\)\b\\1" nil t)
          (replace-match "\\1"))
        (pop-to-buffer (current-buffer))))))

(defun ob-python-extras/get-variables ()
  (interactive)
  (let* ((session-name (ob-python-extras/org-babel-get-session))
         (session-buffer (get-buffer (format "*%s*" session-name)))
         (python-process (when session-buffer
                           (get-buffer-process session-buffer)))
         
         (help-command "
__user_vars = {name: value for name, value in globals().items()
            if not (name.startswith('__') or 
                   isinstance(value, type(__builtins__)) or
                   isinstance(value, type))}
for __name, __value in __user_vars.items():
    print(f\"{__name}: {__value}\")
" )
         (output (when (and session-buffer python-process)
                   (python-shell-send-string-no-output help-command python-process))))
    
    (when output
      (with-current-buffer (get-buffer-create "*Python Variables*")
        (erase-buffer)
        (insert output)
        (goto-char (point-min))
        (while (re-search-forward "\\(.\\)\b\\1" nil t)
          (replace-match "\\1"))
        (pop-to-buffer (current-buffer))))))


(defun ob-python-extras/goto-definition-dispatcher ()
  "Dispatches to the python one to not overwrite workin +lookup/definition in elisp blocks."
  (interactive)
  (if (and (org-in-src-block-p)
           (string= "python" (org-element-property :language (org-element-at-point))))
      (ob-python-extras/python-goto-definition)

    ;; TODO This is pretty doom specific, I think.
    (call-interactively #'+lookup/definition)))

(defun ob-python-extras/python-goto-definition ()
  (interactive)
  (when (and (org-in-src-block-p)
             (string= "python" (org-element-property :language (org-element-at-point))))
    (let* ((session-name (ob-python-extras/org-babel-get-session))
           (session-buffer (get-buffer (format "*%s*" session-name)))
           (python-process (when session-buffer
                             (get-buffer-process session-buffer)))
           (symbol (python-info-current-symbol))
           (definition-command (format "
import sys
import inspect

try:
    if inspect.ismodule(%s):
        print(%s.__file__)
    else:
        print(inspect.getsourcefile(sys.modules[%s.__module__]))
except Exception as e:
    print(f'Traceback: {str(e)}')"
                                       symbol symbol symbol))
           (output (when (and session-buffer python-process symbol)
                     (python-shell-send-string-no-output definition-command python-process))))
      
      (when (and output (not (string-prefix-p "Traceback:" output)))
        (find-file (string-trim output))))))


;;;; completion at point

(defun my-python-completions-capf ()
  (let* ((session-buffer (ob-python-extras/org-babel-get-session))
         (python-process (when session-buffer
                           (or (get-buffer-process session-buffer)
                               (get-buffer-process (format "*%s*" session-buffer)))))
         ;; Look backwards for the start of the expression
         (start (save-excursion
                  ;;  getting the right object at point
                  ;;  may need more fiddling
                  (skip-chars-backward "[:alnum:]_.")
                  (point)))
         (end (point))
         (prefix (buffer-substring-no-properties start end)))
    (when (and session-buffer python-process prefix)
      (list start
            end
            (python-shell-completion-get-completions python-process prefix)
            :exclusive 'no))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'completion-at-point-functions
                      #'my-python-completions-capf nil t)))
;;; renamer

(defvar alternative-python-binary nil
  "Alternative Python binary path. If set, used instead of default Python command, which is nix-shell --pure -p balck --run 'python ...'.")

(defun dired-rename-python-vars ()
  "Run rename script on marked org file using marked json file."
  (interactive)
  (let* ((marked-files (dired-get-marked-files))
         (org-file (cl-find-if (lambda (f) (string-match "\\.org$" f)) marked-files))
         (json-file (cl-find-if (lambda (f) (string-match "\\.json$" f)) marked-files))
         (script-dir (ob-python-extras/find-python-scripts-dir))
         (rename-script (expand-file-name "renamer_utils.py" script-dir))
         (python-command (if alternative-python-binary
                             (format "%s %s %s %s"
                                     alternative-python-binary
                                     rename-script
                                     org-file
                                     json-file)
                           (format "nix-shell --pure -p black --run 'python %s %s %s'"
                                   rename-script
                                   org-file
                                   json-file))))
    (when (and org-file json-file)
      (shell-command python-command)
      (revert-buffer nil t))))

;;; html conversion
;; support for automatically converting html output via pandoc

(defun my/wrap-python-html-capture (orig body &rest args)
  (let ((wrapped-body (format "
import re
import subprocess
from IPython.display import HTML

def __is_likely_html(text):
    patterns = [
        r'<table.*?</table>',
        r'<div.*?</div>',
        r'<p>.*?</p>',
        r'<h[1-3]>.*?</h[1-3]>',
        r'<img\s+[^>]+>'
    ]
    return any(re.search(pattern, text, re.DOTALL | re.IGNORECASE) for pattern in patterns)

__original_print = print

def __smart_print(*args, **kwargs):
    text = ' '.join(str(arg) for arg in args)
    try:
        if __is_likely_html(text):
            proc = subprocess.Popen(['pandoc', '-f', 'html', '-t', 'org',
                                  '--extract-media=plots/html_outputs'],
                                 stdin=subprocess.PIPE,
                                 stdout=subprocess.PIPE)
            org_output, _ = proc.communicate(text.encode())
            org_text = org_output.decode()
            
            # Clean up the output
            cleaned_lines = []
            for line in org_text.splitlines():
                if not any(x in line for x in [':PROPERTIES:', ':CUSTOM_ID:', ':END:']):
                    if line.startswith('*'):
                        stars_count = len(line) - len(line.lstrip('*'))
                        if line[stars_count] == ' ':
                            line = ('-' * stars_count) + line[stars_count:]
                    cleaned_lines.append(\"DF_FLAG:\" + line)
                    # TODO find a better solution than this flag
            
            __original_print('\\n'.join(cleaned_lines), **kwargs)
        else:
            __original_print(*args, **kwargs)
    except:
        __original_print(*args, **kwargs)

print = __smart_print

%s

print = __original_print" body)))
    (apply orig wrapped-body args)))

(advice-add 'org-babel-execute:python :around #'my/wrap-python-html-capture)


(provide 'ob-python-extras)
;;; ob-python-extras.el ends here
