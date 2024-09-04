;;; flycheck-aspell-org.el --- Aspell checker for flycheck in Org documents  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; Created: September 1, 2019
;; Keywords: wp flycheck spell aspell
;; URL: https://github.com/okomestudio/flycheck-aspell-org
;; Package-Version: 0.2.0
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
;; This module provides a plugin to make `flycheck-aspell' work better
;; in documents written in Org mode. See README for further information.
;;
;;; Code:

(require 'flycheck-aspell)

(flycheck-aspell-define-checker "org"
  "Org" ("--add-filter" "url")
  (org-mode))

(add-to-list 'flycheck-checkers 'org-aspell-dynamic)

(defcustom flycheck-aspell-org-skip-blocks '("src" "html" "latex" "example")
  "Org block structures for flycheck to skip.")

(defun flycheck-aspell--parse-org (orig-func &rest rest)
  "Filter Aspell results within Org files."
  (if (not (derived-mode-p 'org-mode))
      (apply orig-func rest)
    (let* ((pre "^[ \t]*")
           (regexp-per-file-keyword
            (concat pre "#\\+\\([[:alnum:]]+\\):\\s-*"))
           (regexp-block-begin
            (concat pre
                    "\\("
                    (string-join
                     `(,(concat "#\\+begin_\\("
                                (string-join flycheck-aspell-org-skip-blocks "\\|")
                                "\\)")
                       ":properties:")
                     "\\|")
                    "\\)"))
           (regexp-block-end
            (concat pre
                    "\\("
                    (string-join
                     `(,(concat "#\\+end_\\("
                                (string-join flycheck-aspell-org-skip-blocks "\\|")
                                "\\)")
                       ":end:")
                     "\\|")
                    "\\)"))
           (case-fold-search t)
           (retval '()))
      (dolist (it (apply orig-func rest))
        ;; Iterate over the error list items received from aspell and
        ;; skip the ones to not be considered as error based on their
        ;; position within the Org document.
        (let* ((inhibit-message t)
               (type (type-of it))
               (line (cl-struct-slot-value type 'line it))
               (column (cl-struct-slot-value type 'column it))
               bol ; beginning of the line in which the current error is found
               eol ; end of the line in which the current error is found
               mh  ; upper bound for the matched region
               pos ; position of error in the current document
               result)
          (setq result
                (catch 'skip
                  (save-mark-and-excursion
                    (goto-line line)
                    (setq bol (pos-bol))
                    (setq eol (pos-eol))

                    ;; NOTE: `column' from aspell is the number of
                    ;; characters, but Emacs's column counts two-byte
                    ;; character as two. Thus `move-to-column' cannot
                    ;; be used.
                    (goto-char bol)
                    (dotimes (i column)
                      (forward-char))

                    (setq pos (point))

                    ;; Skip tag
                    (if (string= "tag" (car (org-thing-at-point)))
                        (throw 'skip nil))

                    ;; Skip TODO
                    (if (seq-contains-p (car org-todo-keywords)
                                        (word-at-point t))
                        (throw 'skip nil))

                    ;; Skip text marked as code with the "~" markup
                    (goto-char pos)
                    (when (re-search-backward "~" bol t)
                      (forward-char 1)
                      (setq mh (re-search-forward "~" eol t))
                      (if (and mh (< pos mh))
                          (throw 'skip nil)))

                    ;; Skip link (but not description)
                    (goto-char pos)
                    (when (re-search-backward "\\[\\[" bol t)
                      (setq mh (re-search-forward "\\]\\(\\[.*\\]\\)?\\]" eol t))
                      (if (and mh (< pos mh))
                          (throw 'skip nil)))

                    ;; Skip per-file keywords
                    (goto-char pos)
                    (end-of-line)
                    (save-match-data
                      (when (re-search-backward regexp-per-file-keyword bol t)
                        (if (not (member (match-string 1) '("title")))
                            (throw 'skip nil))))

                    ;; Skip if within a code block or property drawer
                    (goto-char pos)
                    (when (re-search-backward regexp-block-begin nil t)
                      (setq mh (re-search-forward regexp-block-end nil t))
                      (if (and mh (< pos mh))
                          (throw 'skip nil))))
                  it))
          (when result
            (setq retval (append retval `(,result))))))
      retval)))

(advice-add #'flycheck-aspell--parse :around #'flycheck-aspell--parse-org)

(provide 'flycheck-aspell-org)
;;; flycheck-aspell-org.el ends here
