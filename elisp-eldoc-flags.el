;;; elisp-eldoc-flags.el --- Additional eldoc functions -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/elisp-eldoc-flags
;; Version: 0.1.0
;; Keywords: lisp
;; Package-Requires: ((emacs "26.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Additional eldoc functions

;;; Code:


(defcustom elisp-eldoc-flags-functions '(elisp-eldoc-flags-describe-interactive-flag
                                         elisp-eldoc-flags-describe-format-flag)
  "List of functions to add with `elisp-eldoc-flags-add-eldoc-functions'."
  :group 'autofix
  :type '(repeat
          (radio
           (function-item :tag "Describe interactive flags"
                          elisp-eldoc-flags-describe-interactive-flag)
           (function-item :tag "Describe format flags "
                          elisp-eldoc-flags-describe-format-flag)
           (function :tag "Custom function"))))

(defcustom elisp-eldoc-flags-completions-functions '(elisp-eldoc-flags-interactive-completion-at-point)
  "Additional functions to add in `completion-at-point-functions'."
  :group 'autofix
  :type '(repeat
          (radio
           (function-item :tag "Complete interactive flags"
                          elisp-eldoc-flags-interactive-completion-at-point)
           (function :tag "Custom function"))))

(defvar elisp-eldoc-flags-eldoc-format-flags-alist
  '(("%s" .
     "means produce a string argument. Actually, produces any object with princ")
    ("%d" . "means produce as signed number in decimal")
    ("%o" . "means produce a number in octal")
    ("%x" . "means produce a number in hex")
    ("%X" . "is like %x, but uses upper case")
    ("%e" . "means produce a number in exponential notation")
    ("%f" . "means produce a number in decimal-point notation")
    ("%g" .
     "means produce a number in exponential notation if the exponent would be less than -4 or greater than or equal to the precision (default: 6). otherwise it produces in decimal-point notation")
    ("%c" . "means produce a number as a single character")
    ("%S" . "means produce any object as an s-expression (using prin1)")))


(defun elisp-eldoc-flags-describe-format-flag (&optional _callback &rest
                                                         _ignored)
  "Show description for `format' flags inside string."
  (when-let* ((flag (cond ((looking-at "\\(%[SXc-gosx]\\)")
                           (buffer-substring-no-properties (point)
                                                           (+ (point) 2)))
                          ((looking-back "\\(%[SXc-gosx]\\)" 0)
                           (buffer-substring-no-properties (- (point) 2)
                                                           (point)))
                          ((and (< (point-min)
                                   (point))
                                (save-excursion
                                  (forward-char -1)
                                  (looking-at "\\(%[SXc-gosx]\\)")))
                           (buffer-substring-no-properties (- (point) 1)
                                                           (1+ (point))))))
              (descr (cdr
                      (assoc flag elisp-eldoc-flags-eldoc-format-flags-alist))))
    (message "%s: %s" (propertize flag 'face 'font-lock-doc-face)
             descr)))

(defvar elisp-eldoc-flags-interactive-flags-alist
  '(("a" . "Function name: symbol with a function definition")
    ("b" . "Name of existing buffer")
    ("B" . "Name of buffer, possibly nonexistent")
    ("c" . "Character (no input method is used")
    ("C" . "Command name: symbol with interactive function definition")
    ("d" . "Value of point as number.  Does not do I/O")
    ("D" . "Directory name")
    ("e" .
     "Parameterized event (i.e., one that's a list) that invoked this command")
    ("f" . "Existing file name")
    ("F" . "Possibly nonexistent file name")
    ("G" . "Possibly nonexistent file name, defaulting to just directory name")
    ("i" . "Ignored, i.e. always nil.  Does not do I/O")
    ("k" .
     "Key sequence (downcase the last event if needed to get a definition)")
    ("K" . "Key sequence to be redefined (do not downcase the last event)")
    ("m" . "Value of mark as number.  Does not do I/O")
    ("M" . "Any string.  Inherits the current input method")
    ("n" . "Number read using minibuffer")
    ("N" . "Numeric prefix arg, or if none, do like code n")
    ("p" . "Prefix arg converted to number.  Does not do I/O")
    ("P" . "Prefix arg in raw form.  Does not do I/O")
    ("r" .
     "Region: point and mark as 2 numeric args, smallest first.  Does no I/O")
    ("s" . "Any string.  Does not inherit the current input method")
    ("S" . "Any symbol")
    ("U" . "Mouse up event discarded by a previous k or K argument")
    ("v" . "Variable name: symbol that is custom-variable-p")
    ("x" . "Lisp expression read but not evaluated")
    ("X" . "Lisp expression read and evaluated")
    ("z" . "Coding system")
    ("Z" . "Coding system, nil if no prefix arg")
    ("*" . "an error is signaled if the buffer is read-only")
    ("@" .
     "appears at the beginning of the string, and if the key sequence\nused to invoke the command includes any mouse events, then the window associated with\nthe first of those events is selected before the command is run")
    ("^" .
     "If the string begins with `^' and `shift-select-mode' is non-nil,Emacs first calls the function `handle-shift-selection'")))

(defun elisp-eldoc-flags-split-to-interactive-flags (str)
  "Split string STR into interactive flags."
  (let* ((parts (split-string str "" t))
         (special-flags (seq-take-while (lambda (it)
                                          (member it '("*" "^" "@")))
                                        parts))
         (restflags (split-string (string-join (seq-drop parts (length
                                                                special-flags))
                                               "")
                                  "\n" t)))
    (append special-flags restflags)))

(defun elisp-eldoc-flags-describe-interactive-flag (&optional _callback &rest
                                                              _ignored)
  "Show description for interactive form at point."
  (pcase-let ((`(,sym ,str)
               (save-excursion
                 (ignore-errors
                   (unless (eq (symbol-at-point) 'interactive)
                     (let ((pps (syntax-ppss (point))))
                       (when (and (nth 3 pps)
                                  (nth 8 pps))
                         (goto-char (nth 8 pps))))
                     (backward-up-list nil t)
                     (sexp-at-point))))))
    (pcase sym
      ('interactive
       (when-let ((msg
                   (when (stringp str)
                     (mapconcat
                      (lambda (it)
                        (let ((flag (substring-no-properties it
                                                             0
                                                             1))
                              (prompt
                               (when (> (length it) 1)
                                 (substring-no-properties it 1))))
                          (concat
                           (propertize (format "%s" flag)
                                       'face
                                       'font-lock-function-name-face)
                           ": " (or
                                 (cdr
                                  (assoc flag
                                         elisp-eldoc-flags-interactive-flags-alist))
                                 "unknown flag")
                           (if prompt (concat (propertize " Prompt: "
                                                          'face
                                                          'font-lock-doc-face)
                                              prompt)
                             ""))))
                      (elisp-eldoc-flags-split-to-interactive-flags str)
                      "\n"))))
         (message msg))))))

(defun elisp-eldoc-flags-interactive-completion-at-point ()
  "Complete inside string in the `interactive' form.
Addional function for `completion-at-point-functions' in `emacs-lisp-mode'."
  (when-let* ((start (let ((pps (syntax-ppss (point))))
                       (when (and (nth 3 pps)
                                  (nth 8 pps))
                         (save-excursion
                           (goto-char (nth 8 pps))
                           (when (looking-back "[(]interactive[\s\t\n]?+" 0)
                             (1+ (nth 8 pps)))))))
              (end (point)))
    (let ((terminator (and
                       (> end start)
                       (not (looking-back "\\n" 0)))))
      (list
       end
       end
       (apply-partially 'completion-table-with-terminator
                        "\n"
                        (if terminator
                            (list "\\n")
                          (mapcar 'car
                                  elisp-eldoc-flags-interactive-flags-alist)))
       :predicate (lambda (a)
                    (if (= start end)
                        t
                      (not (member a '("@" "*" "^")))))
       :annotation-function
       (lambda (str)
         (or (cdr (assoc str elisp-eldoc-flags-interactive-flags-alist))
             " terminator"))))))


;;;###autoload
(defun elisp-eldoc-flags-add-completion-functions ()
  "Add `elisp-eldoc-flags-completions-functions' to completions at point."
  (interactive)
  (dolist (fn elisp-eldoc-flags-completions-functions)
    (add-hook 'completion-at-point-functions
              fn nil 'local)))

;;;###autoload
(defun elisp-eldoc-flags-remove-completion-functions ()
  "Remove `elisp-eldoc-flags-completions-functions' from completions at point."
  (interactive)
  (dolist (fn elisp-eldoc-flags-completions-functions)
    (remove-hook 'completion-at-point-functions
                 fn 'local)))


;;;###autoload
(defun elisp-eldoc-flags-add-eldoc-functions ()
  "Add `elisp-eldoc-flags-functions' to `eldoc-documentation-functions'."
  (interactive)
  (dolist (fn elisp-eldoc-flags-functions)
    (add-hook 'eldoc-documentation-functions
              fn
              nil t)))


;;;###autoload
(defun elisp-eldoc-flags-remove-eldoc-functions ()
  "Remove `elisp-eldoc-flags-functions' from `eldoc-documentation-functions'."
  (interactive)
  (dolist (fn elisp-eldoc-flags-functions)
    (remove-hook 'eldoc-documentation-functions
                 fn
                 t)))

(provide 'elisp-eldoc-flags)
;;; elisp-eldoc-flags.el ends here