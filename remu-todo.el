;;; remu-todo.el --- TODO  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2004-2023 Free Software Foundation, Inc.

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 最初是从org-agenda.el里抄来的

;;; Code:
(require 'org-agenda)
(require 'remu-link)

(defun remu-todo--collect ()
  (let ((table (make-hash-table :test 'equal))
        (table1 (make-hash-table :test 'equal))
        (regexp (concat "^" org-outline-regexp " *" org-todo-regexp))
        (all-matches nil)
        (all-regexps nil))
    (goto-char (point-min))
    (while (let (case-fold-search)
             (re-search-forward regexp nil t))
      (pcase-let
          ((`(,matches . ,regexps)
            (save-excursion
              (goto-char (match-beginning 0))
              (with-restriction
                  (point)
                  (save-excursion
                    (org-next-visible-heading 1)
                    (point))
                (remu-link--collect-links table t (point-min))))))
        (push matches all-matches)
        (push regexps all-regexps)))
    (maphash
     (lambda (key value)
       (puthash key (seq-uniq value) table1))
     table)
    (list (apply 'append all-matches) (apply 'append all-regexps) table1)))

(defun remu-todo--find-timestamp (bol key re)
  (save-excursion
    (when (search-backward re bol t)
      (goto-char (match-end 0))
      (skip-chars-forward " \t")
      (when (looking-at org-ts-regexp-both)
        key))))

(defun remu-todo--find-item-time ()
  (forward-line)
  (when (looking-at-p org-planning-line-re)
    (end-of-line)
    (let ((bol (line-beginning-position)))
      (or (remu-todo--find-timestamp bol 'closed org-closed-string)
          (remu-todo--find-timestamp bol 'deadline org-deadline-string)
          (remu-todo--find-timestamp bol 'scheduled org-scheduled-string)))))

(defun remu-todo--get-item-extra ()
  (pcase (remu-todo--find-item-time)
    ('closed
     (cons nil "Closed:    "))
    ('deadline
     (let* ((today (org-today))
            (today? t)
            (current today)
            (s (match-string 0))
            (pos (point))
	    (todo-state (save-match-data (org-get-todo-state)))
            (sexp? (string-prefix-p "%%" s))
	    ;; DEADLINE is the deadline date for the entry.  It is
	    ;; either the base date or the last repeat, according
	    ;; to `org-agenda-prefer-last-repeat'.
	    (deadline
	     (cond
	      (sexp? (org-agenda--timestamp-to-absolute s current))
	      ((or (eq org-agenda-prefer-last-repeat t)
		   (member todo-state org-agenda-prefer-last-repeat))
	       (org-agenda--timestamp-to-absolute
		s today 'past (current-buffer) pos))
	      (t (org-agenda--timestamp-to-absolute s))))
	    (diff (- deadline current))
	    (suppress-prewarning
	     (let ((scheduled
		    (and org-agenda-skip-deadline-prewarning-if-scheduled
			 (org-entry-get nil "SCHEDULED"))))
	       (cond
		((not scheduled) nil)
		;; The current item has a scheduled date, so
		;; evaluate its prewarning lead time.
		((integerp org-agenda-skip-deadline-prewarning-if-scheduled)
		 ;; Use global prewarning-restart lead time.
		 org-agenda-skip-deadline-prewarning-if-scheduled)
		((eq org-agenda-skip-deadline-prewarning-if-scheduled
		     'pre-scheduled)
		 ;; Set pre-warning to no earlier than SCHEDULED.
		 (min (- deadline
			 (org-agenda--timestamp-to-absolute scheduled))
		      org-deadline-warning-days))
		;; Set pre-warning to deadline.
		(t 0))))
            (wdays (or suppress-prewarning (org-get-wdays s)))
            (extra
	     ;; Insert appropriate suffixes before deadlines.
	     ;; Those only apply to today agenda.
	     (pcase-let ((`(,now ,future ,past)
			  org-agenda-deadline-leaders))
	       (cond
		((and today? (< deadline today)) (format past (- diff)))
		((and today? (> deadline today)) (format future diff))
		(t now))))
            (face (org-agenda-deadline-face
		   (- 1 (/ (float diff) (max wdays 1))))))
       (cons face extra)))
    ('scheduled
     (let* ((today (org-today))
            (todayp t)
            (current today)
            (s (match-string 0))
            (pos (point))
	    (todo-state (save-match-data (org-get-todo-state)))
	    (sexp? (string-prefix-p "%%" s))
	    ;; SCHEDULE is the scheduled date for the entry.  It is
	    ;; either the bare date or the last repeat, according
	    ;; to `org-agenda-prefer-last-repeat'.
	    (schedule
	     (cond
	      (sexp? (org-agenda--timestamp-to-absolute s current))
	      ((or (eq org-agenda-prefer-last-repeat t)
		   (member todo-state org-agenda-prefer-last-repeat))
	       (org-agenda--timestamp-to-absolute
		s today 'past (current-buffer) pos))
	      (t (org-agenda--timestamp-to-absolute s))))
	    (diff (- current schedule))
	    (pastschedp (< schedule today))
	    (futureschedp (> schedule today))
            (habitp (and (fboundp 'org-is-habit-p) (org-is-habit-p)))
            (extra
             (pcase-let ((`(,first ,past) org-agenda-scheduled-leaders))
	       ;; Show a reminder of a past scheduled today.
	       (if (and todayp pastschedp)
		   (format past diff)
		 first)))
            (face (cond ((and (not habitp) pastschedp)
			 'org-scheduled-previously)
			((and habitp futureschedp)
			 'org-agenda-done)
			(todayp 'org-scheduled-today)
			(t 'org-scheduled))))
     (cons face extra)))
    (_
     (cons nil "           "))))

(defun remu-todo--insert-item (buffer)
  (let* ((start (point))
         (todo-state (save-match-data (org-get-todo-state)))
         (donep (member todo-state org-done-keywords))
         (habitp (and (fboundp 'org-is-habit-p) (org-is-habit-p)))
         (category (org-get-category))
         (inherited-tags
	  (or (eq org-agenda-show-inherited-tags 'always)
	      (and (listp org-agenda-show-inherited-tags)
		   (memq 'agenda org-agenda-show-inherited-tags))
	      (and (eq org-agenda-show-inherited-tags t)
		   (or (eq org-agenda-use-tag-inheritance t)
		       (memq 'agenda
			     org-agenda-use-tag-inheritance)))))
         (tags (org-get-tags nil (not inherited-tags)))
         (level (make-string (org-reduced-level (org-outline-level)) ?\s))
         (head
          (progn
            (re-search-forward org-outline-regexp-bol)
            (buffer-substring (point) (line-end-position))))
         (face-extra (remu-todo--get-item-extra))
         (extra (cdr face-extra))
         (face (if donep 'org-agenda-done (car face-extra)))
         (item (org-agenda-format-item extra head level category tags nil nil habitp))
         (txt (if face (propertize item 'face face) item)))
    (with-current-buffer buffer
      (insert (propertize txt 'remu-section t 'remu-pos start))
      (insert "\n"))))

(defun remu-todo--display-links (buffer links headline)
  (org-compile-prefix-format 'agenda)
  (dolist (pos (seq-sort '< (remu-link--get-matches links headline t)))
    (goto-char pos)
    (remu-todo--insert-item buffer)))

;;;###autoload
(defun remu-todo-section (overlay)
  (remu-link-back-section overlay t 'remu-todo--collect 'remu-todo--display-links))

(provide 'remu-link)
;;; remu-link.el ends here
