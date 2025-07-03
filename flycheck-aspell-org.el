;;; flycheck-aspell-org.el --- Aspell checker for flycheck in Org documents  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024-2025 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; Created: September 1, 2019
;; Keywords: wp flycheck spell aspell
;; URL: https://github.com/okomestudio/flycheck-aspell-org
;; Package-Version: 0.2.1
;; Package-Requires: ((emacs "25.1") (flycheck "28.0") (flycheck-aspell "0.2.0"))
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This module provides a plugin to make `flycheck-aspell' work better in
;; documents written in Org mode. See README for further information.
;;
;;; Code:

(require 'flycheck-aspell)

(flycheck-aspell-define-checker "org"
  "Org" ("--add-filter" "url")
  (org-mode))

(defcustom fao-skip-blocks '("src" "html" "latex" "example")
  "Org block structures for flycheck to skip.")

(defun fao-enclosed-p (pos begin end &optional pos-min pos-max)
  "Return '(min max) when BEGIN and END encloses POS.
Optional POS-MIN and POS-MAX defines lower and upper bounds for
search."
  (goto-char pos)
  (let ((ml (re-search-backward begin pos-min t)))
    (when ml
      (forward-char 1)
      (let ((mh (re-search-forward end pos-max t)))
        (if (and mh (< pos mh))
            `(,ml ,mh))))))

(defun flycheck-aspell--parse-org (orig-func &rest rest)
  "Filter Aspell results in `org-mode'."
  (if (not (derived-mode-p 'org-mode))
      (apply orig-func rest)
    (let* ((pre "^[ \t]*")
           (regexp-block-begin
            (concat
             pre
             "#\\+begin_\\("
             (string-join flycheck-aspell-org-skip-blocks "\\|")
             "\\)"))
           (regexp-block-end
            (concat
             pre
             "#\\+end_\\("
             (string-join flycheck-aspell-org-skip-blocks "\\|")
             "\\)"))
           (retval '()))
      ;; Iterate over the error list items received from aspell and
      ;; skip the ones to not be considered as error based on their
      ;; position within the Org document.
      (dolist (it (apply orig-func rest))
        (let* ((inhibit-message t)
               (case-fold-search t)
               (type (type-of it))
               (line (cl-struct-slot-value type 'line it))
               (column (cl-struct-slot-value type 'column it))
               bol   ; beginning of the line in which the current error is found
               eol   ; end of the line in which the current error is found
               pos   ; position of error in the current document
               word  ; word at point
               result)
          (setq
           result
           (catch 'skip
             (save-mark-and-excursion
               (goto-char (point-min))
               (forward-line (1- line))
               (forward-char (1- column))
               (setq pos (point))
               (setq bol (pos-bol))
               (setq eol (pos-eol))
               (setq word (word-at-point t))

               (if (or
                    (string= "tag" (car (org-thing-at-point)))
                    (fao-enclosed-p pos "~" "~" bol eol)
                    (fao-enclosed-p pos "\\$" "\\$" bol eol)
                    (fao-enclosed-p pos "\\\\(" "\\\\)" bol eol)
                    (fao-enclosed-p pos "\\[\\[" "\\]\\(\\[.*\\]\\)?\\]" bol eol)
                    (fao-enclosed-p pos "^[ \t]*:properties:" "^[ \t]*:end:")
                    (fao-enclosed-p pos regexp-block-begin regexp-block-end)
                    (seq-contains-p (car org-todo-keywords) word))
                   (throw 'skip nil))

               ;; Per-file keywords
               (save-match-data
                 (when (fao-enclosed-p pos
                                       "^[ \t]*#\\+"
                                       "\\([[:alnum:]]+\\):\\s-*\\(.+\\)$"
                                       bol eol)
                   (if (not (member (match-string 1) '("title")))
                       (throw 'skip nil))))

               ;; Footnote prefix
               (when (string= word "fn")
                 (let* ((r (fao-enclosed-p pos "\\[" ":" bol eol))
                        (pos-beg (car r))
                        (pos-end (1- (cadr r))))
                   (when (and (char-equal (char-after pos-beg) 91)
                              (char-equal (char-after pos-end) ?:))
                     (throw 'skip nil)))))
             it))
          (when result
            (setq retval (append retval `(,result))))))
      retval)))

(advice-add #'flycheck-aspell--parse :around #'flycheck-aspell--parse-org)

(provide 'flycheck-aspell-org)

;; Local Variables:
;; read-symbol-shorthands: (("fao" . "flycheck-aspell-org"))
;; End:
;;; flycheck-aspell-org.el ends here
