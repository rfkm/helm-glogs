;;; helm-glogs.el --- Glogs with helm interface -*- lexical-binding: t -*-

;; Copyright (C) 2016 Ryo Fukumuro

;; Author: Ryo Fukumuro <rkworks@o0o.co>
;; URL: https://github.com/rfkm/helm-glogs
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3") (helm "1.7.7"))


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides a helm frontend of glogs.

;;; Code:

(require 'helm)

(defgroup helm-glogs nil
  "Glogs with helm interface"
  :group 'helm)

(defcustom helm-glogs-handle-space-as-and t
  "Handle spaces which appear in a pattern as AND.
e.g: \"foo bar baz\" will match the pattern \"foo bar\"."
  :group 'helm-glogs
  :type 'boolean)

(defcustom helm-glogs-case-insensitive t
  "Add a case-insensitive flag to each pattern."
  :group 'helm-glogs
  :type 'boolean)

(defcustom helm-glogs-base-command "glogs -1"
  "Base command of `glogs'."
  :group 'helm-glogs
  :type 'string)

(defcustom helm-glogs-custom-log-format nil
  "When non-nil, the value will be passed to `--format' option of `glogs'."
  :group 'helm-glogs
  :type 'string)

(defcustom helm-glogs-use-ansi-color t
  "Translate ansi codes in candidates to text properties."
  :group 'helm-glogs
  :type 'boolean)

(defvar helm-glogs--valid-regexp nil)
(defvar helm-glogs--default-log-format "[%rn] <%an> %B")
(defvar helm-glogs--default-colored-log-format "%C(red)[%rn]%C(reset) %C(green)<%an>%C(reset) %B")

(defun helm-glogs--split-pattern (pattern)
  (let (ret)
    (dolist (s (reverse (split-string pattern " " t)) ret)
      (if (string-suffix-p "\\" s)
          (setq ret (cons (concat (substring s 0 (- (length s) 1)) " " (car ret)) (cdr ret)))
        (setq ret (cons s ret))))))

(defun helm-glogs--with-case-flag (pattern)
  (if helm-glogs-case-insensitive
      (concat "(?i)" pattern)
    pattern))

(defun helm-glogs--format-pattern (pattern)
  (if helm-glogs-handle-space-as-and
      (let ((helm-glogs-handle-space-as-and nil))
        (mapconcat #'helm-glogs--format-pattern (helm-glogs--split-pattern pattern) " "))
    (format "-I %s" (shell-quote-argument (helm-glogs--with-case-flag
                                           (if (string-suffix-p "\\" pattern)
                                               (substring pattern 0 (- (length pattern) 1))
                                             pattern))))))

(defun helm-glogs--real-to-display (candidate)
  (replace-regexp-in-string "\0" "\n" candidate))

(defun helm-glogs--format-command (pattern)
  (concat helm-glogs-base-command " "
          "-f " (shell-quote-argument (if helm-glogs-custom-log-format
                                          helm-glogs-custom-log-format
                                        (if helm-glogs-use-ansi-color
                                            helm-glogs--default-colored-log-format
                                          helm-glogs--default-log-format))) " "
                                          (helm-glogs--format-pattern pattern)))

(defun helm-glogs--init ()
  (prog1
      (start-process-shell-command "glogs" helm-buffer
                                   (helm-glogs--format-command helm-pattern))
    (setq helm-glogs--valid-regexp (helm-glogs--validate-regexp helm-pattern))
    (set-process-sentinel
     (get-buffer-process helm-buffer)
     (lambda (_process event)
       (when (string= event "finished\n")
         (with-helm-window
           (setq mode-line-format
                 '(" " mode-line-buffer-identification " "
                   (:eval (format "L%s" (helm-candidate-number-at-point))) " "
                   (:eval (propertize
                           (format
                            "[%s process finished - (%s results)] "
                            (upcase "glogs")
                            (helm-get-candidate-number))
                           'face 'helm-grep-finish))))
           (force-mode-line-update)))))))

(defun helm-glogs--validate-regexp (regexp)
  (condition-case nil
      (progn
        (string-match-p regexp "")
        t)
    (invalid-regexp nil)))

(defun helm-glogs--action-save-kill-ring (candidate)
  (if (helm-marked-candidates)
      (progn
        (kill-new "")
        (dolist (c (helm-marked-candidates))
          (kill-append (concat c "\n") nil)))
    (kill-new candidate)))

(defun helm-glogs--action-insert (candidate)
  (if (helm-marked-candidates)
      (progn
        (dolist (c (helm-marked-candidates))
          (insert (concat c "\n"))))
    (insert candidate)))

(defvar helm-glogs--actions
  (helm-make-actions
   "Save message(s) to kill ring" #'helm-glogs--action-save-kill-ring
   "Insert message(s) to current buffer" #'helm-glogs--action-insert))

;;;###autoload
(defun helm-glogs ()
  "Helm for `glogs'."
  (interactive)
  (helm :sources (helm-build-async-source "Glogs"
                   :candidates-process #'helm-glogs--init
                   ;; :multiline t
                   ;; :real-to-display 'helm-glogs--real-to-display
                   :nohighlight t
                   :filtered-candidate-transformer (lambda (candidates source)
                                                     (if helm-glogs--valid-regexp
                                                         (helm-fuzzy-highlight-matches candidates source)
                                                       candidates))
                   :filter-one-by-one (lambda (c) (if helm-glogs-use-ansi-color
                                                      (helm--ansi-color-apply c)
                                                    c))
                   :action helm-glogs--actions
                   :requires-pattern 2
                   :candidate-number-limit 9999)))

(provide 'helm-glogs)

;;; helm-glogs.el ends here
