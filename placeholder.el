;;; placeholder.el --- Easy insertion and filling of placeholders in text  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Omar Antolín Camarena

;; Author: Omar Antolín Camarena <omar@matem.unam.mx>
;; Keywords: files
;; Version: 0.1
;; Homepage: https://github.com/oantolin/placeholder
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a simple way to treat text in a buffer as a
;; template with placeholders where text needs to be filled in.  Any
;; occurrence of <++> in the buffer is a placeholder.  You can navigate
;; among the placeholder with the `placeholder-forward' and
;; `placeholder-backward' commands.  They move the point to the next
;; placeholder in the specified direction and delete the placeholder so
;; you can immediately start typing the text that should replace it.
;; However, if you call them again immediately after, they restore that
;; occurrence of the placeholder and move to the next.

;; The placeholder in only <++> by default, it can be changed by
;; customizing the `placeholder-string' variable.

;; A `placeholder-insert' command is also provided, to insert the
;; placeholder string.
 

;;; Code:

(defgroup placeholder nil
  "Easy insertion and filling of placeholders."
  :group 'editing)

(defcustom placeholder-string "<++>"
  "String used by `placeholder-insert' as a placeholder.
Pick a string unlikely to appear in your buffers."
  :type 'string)

(defface placeholder
  '((((class color) (min-colors 88) (background dark)) :foreground "#60cfa2")
    (((class color) (min-colors 88) (background light)) :foreground "#005040")
    (t :foreground "green"))
  "Face for `placeholder-insert' strings.")

(defun placeholder-insert ()
  "Insert the `placeholder-string' in the current buffer."
  (interactive)
  (insert (propertize placeholder-string
                      'font-lock-face 'placeholder
                      'rear-nonsticky 'font-lock-face)))

(defun placeholder-forward (count)
  "Move forward over COUNT occurrences of `placeholder-string'.
This deletes the COUNTth placeholder.  If this command is called
again immediately after, restore that occurence of the
placeholder and move to the next."
  (interactive "p")
  (let ((n (length placeholder-string )))
    (when (eq last-command 'placeholder)
      (insert placeholder-string)
      (when (< count 0) (backward-char n)))
    (search-forward placeholder-string nil nil count)
    (delete-char (if (> count 0) (- n) n))
    (setq this-command 'placeholder)))

(defun placeholder-backward (count)
  "Move backward over COUNT occurrences of `placeholder-string'.
This deletes the COUNTth placeholder.  If this command is called
again immediately after, restore that occurence of the
placeholder and move to the next."
  (interactive "p")
  (placeholder-forward (- count)))

(provide 'placeholder)
;;; placeholder.el ends here
