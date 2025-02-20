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
            print(f\"{timerstring} {str((org_babel_wrapper_datetime.now() - start)).split('.')[0]}\")
        else:
            print(f\"{timerstring} {str((org_babel_wrapper_datetime.now() - start))}\")
    if %s:
        print(f\"Last run at: {org_babel_wrapper_datetime.now()}\")
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
         (transparent-header (assoc :transparent headers)))
    (with-temp-file exec-file (insert body))
    (let* ((body (format "\
exec_file = \"%s\"
pymockbabel_script_location = \"%s\"
import os
import sys
sys.path.append(pymockbabel_script_location)
import pymockbabel
outputs_and_file_paths, output_types, list_writer = pymockbabel.setup(\"%s\"%s)
with open(exec_file, 'r') as file:
    exec(compile(file.read(), '<org babel source block>', 'exec'))
pymockbabel.display(outputs_and_file_paths, output_types, list_writer)
try:
    os.remove(exec_file)
except:
    pass "
                         exec-file
                         pymockbabel-script-location
                         (file-name-sans-extension (file-name-nondirectory buffer-file-name))
                         (concat ", transparent="
                                 (if transparent-header (if (equal (cdr transparent-header) "nil") "False" "True")
                                   (if (and (boundp 'ob-python-extras/transparent-images) ob-python-extras/transparent-images) "True" "False"))))))
      (apply orig body params args))))



(advice-add 'org-babel-execute:python
            :around #'ob-python-extras/wrap-org-babel-execute-python-mock-plt
            '((depth . -5)))



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


(defun select-full-word-with-dots ()
  "Select current word including any dots and connected words before/after it."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      ;; Go backward through dots
      (goto-char (car bounds))
      (while (and (> (point) (point-min))
                  (save-excursion
                    (backward-char)
                    (looking-at "\\.")))
        (backward-char)
        (backward-sexp))
      (set-mark (point))
      ;; Go forward through dots
      (goto-char (cdr bounds))
      (while (and (< (point) (point-max))
                  (looking-at "\\."))
        (forward-char)
        (forward-symbol 1)))))


(defun ob-python-extras/python-help-clean ()
  (interactive)
  (save-excursion
    (let* ((symbol (progn
                     (message "Getting symbol...")
                     (save-excursion
                       (select-full-word-with-dots)
                       (buffer-substring-no-properties (region-beginning) (region-end)))))
           (_ (message "Symbol is: '%s'" symbol))
           (body
            (format "
import sys
from io import StringIO
sys.path.append(\"%s\")
import pymockbabel
pymockbabel.EXTRAS_DO_REPLACEMENTS = False

# Capture help output
help_output = StringIO()
sys.stdout = help_output
help(%s)
# this still doesn't work ideally, it can get evaluated on the value of passed object,
# which is a problem for pd.options.display.max_rows
# it gets evaluated onthe int, not on the pandas option
sys.stdout = sys.__stdout__
print(help_output.getvalue())

pymockbabel.EXTRAS_DO_REPLACEMENTS = True
"(ob-python-extras/find-python-scripts-dir) symbol))
           (_ (message "Python body: %s" body))
           (result nil)
           (temp-start nil))
      ;; Find current source block end
      (unwind-protect
          (progn
            (org-babel-where-is-src-block-head)
            (search-forward "#+end_src")
            (forward-line)
            (setq temp-start (point))
            (insert (format "#+begin_src python :async no :results none :timer-show no\n%s\n#+end_src\n" body))
            (forward-line -1)
            (setq result (org-babel-execute-src-block)))
        (delete-region temp-start (save-excursion
                                    (goto-char temp-start)
                                    (search-forward "#+end_src")
                                    (forward-line)
                                    (point)))
        (with-current-buffer (get-buffer-create "*Python Help*")
          (erase-buffer)
          (insert result)
          (goto-char (point-min))

          (while (re-search-forward "\\(.\\)\b\\1" nil t)
            (replace-match "\\1"))
          (pop-to-buffer (current-buffer)))))))


(defun ob-python-extras/python-goto-definition ()
  (interactive)

  (if (and (org-in-src-block-p)
           (string= "python" (org-element-property :language (org-element-at-point))))
      (save-excursion
        (let* ((symbol (progn
                         (select-full-word-with-dots)
                         (buffer-substring-no-properties (region-beginning) (region-end))))
               (body (format "
import sys
sys.path.append(\"%s\")
import inspect

try:
    import inspect

    if inspect.ismodule(%s):
        print(%s.__file__)
    else:
        print(inspect.getsourcefile(sys.modules[%s.__module__]))
except Exception as e:
    print(f'Traceback: {str(e)}')"

                             (ob-python-extras/find-python-scripts-dir)
                             symbol

                             symbol
                             symbol))
               (temp-start nil))

          (unwind-protect
              (progn
                (org-babel-where-is-src-block-head)
                (search-forward "#+end_src")
                (forward-line)
                (setq temp-start (point))
                (insert (format "#+begin_src python :async no :results none :timer-show no\n%s\n#+end_src\n" body)))
            (setq output (string-trim (org-babel-execute-src-block)))

            (delete-region temp-start (save-excursion
                                        (forward-line 1)
                                        (point))))

          (unless (string-prefix-p "Traceback:" output)
            (find-file output))))))

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
         (rename-script (expand-file-name "rename_script.py" script-dir))
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

(provide 'ob-python-extras)
;;; ob-python-extras.el ends here
