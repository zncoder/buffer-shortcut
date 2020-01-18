;;; buffer-shortcut.el --- Jump to buffer quickly    -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Bin Zhang

;; Author: Bin Zhang <zncoder@gmail.com>
;; Keywords: lisp
;; Version: 1.0.0

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

;; BufferShortcut allows jumping to a buffer with a char, that is
;; assigned to the buffer with buffer-shortcut-set.

;;; Installation:

;; Add to ~/.emacs
;;
;;    (require 'buffer-shortcut)

;;; Code:

(defcustom buffer-shortcut-delimiter "ˆ"
	"Delimiter between buffer name and shortcut"
	:type 'string
	:group 'buffer-shortcut)

(defvar buffer-shortcut-after-goto-hook nil)
(defvar buffer-shortcut-not-found-hook nil)
(defvar-local buffer-shortcut-id nil)

(defun buffer-shortcut-id-prefix (&optional c)
	(setq c (or c buffer-shortcut-id))
	(if c
			(concat (char-to-string c) buffer-shortcut-delimiter)
		""))

(defun bs/list ()
	(let (sl)
		(dolist (b (buffer-list))
			(with-current-buffer b
				(when buffer-shortcut-id
					(setq sl (cons (cons buffer-shortcut-id b) sl)))))
		sl))

(defun buffer-shortcut-do (c sl)
	(interactive
	 (let* ((sl (bs/list))
					;; capital to set
					(c (read-char (concat "goto or set: " (bs/display sl)))))
		 (list c sl)))
	(cond
	 ((eq c 32)														;reset
		(setq buffer-shortcut-id nil))
	 ((and (<= ?a c) (<= c ?z))						;goto
		(let* ((x (assq c sl)))
			(if x
					(let ((b (current-buffer)))
						(switch-to-buffer (cdr x))
						(run-hook-with-args 'buffer-shortcut-after-goto-hook b))
				(bs/not-found c))))
	 ((and (<= ?A c) (<= c ?Z))						;set
		(let* ((c (+ ?a (- c ?A)))
					 (x (assq c sl)))
			(if x
					(with-current-buffer (cdr x)
						(setq buffer-shortcut-id nil))
				(setq buffer-shortcut-id c))))
	 (t
		(error "shortcut:%c out of range" c))))

(defun bs/not-found (c)
	(let ((b (run-hook-with-args-until-success 'buffer-shortcut-not-found-hook c)))
		(cond
		 ((bufferp b)
			(with-current-buffer b
				(setq buffer-shortcut-id c)))
		 ((not b)
			(error "shortcut:%s not found" c)))))

(defun bs/display (sl)
	(s-join
	 " "
	 (sort
		(mapcar
		 (lambda (x)
			 (let* ((c (car x))
						 (prefix (buffer-shortcut-id-prefix c))
						 (name (buffer-name (cdr x))))
				 (if (s-prefix? prefix name)
						 name
					 (concat prefix name))))
		 sl)
		'string<)))

;; save buffer-shortcut-id in desktop
(customize-push-and-save 'desktop-locals-to-save '(buffer-shortcut-id))

(provide 'buffer-shortcut)
;;; buffer-shortcut.el ends here
