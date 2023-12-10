;;; remu-toc.el --- 目录  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2020-2022 Jethro Kuan <jethrokuan95@gmail.com>

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

;; 最初是从org-roam-mode.el里抄来的

;;; Code:

(defun remu-toc--insert (level heading pos)
  (insert
   (concat (make-string level ? )
           (propertize heading 'remu-section t 'remu-pos pos)
           "\n")))

(defun remu-toc--insert-heading (buffer)
  (let ((heading (org-get-heading))
        (level (org-current-level))
        (pos (line-beginning-position)))
    (with-current-buffer buffer
      (remu-toc--insert level heading pos))))

;;;###autoload
(defun remu-toc-section (overlay)
  (let* ((sidebar-buffer (current-buffer))
         (buffer remu--current-buffer)
         (tick (buffer-chars-modified-tick buffer))
         (pos (with-current-buffer buffer (point))))
    (unless (eq tick (overlay-get overlay 'remu-last-tick))
      (delete-region (point-min) (point-max))
      (remu-toc--insert 0 (buffer-name buffer) 1)
      (with-current-buffer buffer
         (org-map-region
          (lambda () (remu-toc--insert-heading sidebar-buffer))
          (point-min)
          (point-max)))
      (overlay-put overlay 'remu-last-tick tick))
    (when-let ((ov (overlay-get overlay 'remu-highlight)))
      (delete-overlay ov))
    (goto-char (point-max))
    (while-let ((prop-match (text-property-search-backward 'remu-pos)))
      (when-let ((value (prop-match-value prop-match)))
        (when (<= value pos)
          (let ((ov (make-overlay (prop-match-beginning prop-match) (prop-match-end prop-match))))
            (overlay-put ov 'evaporate t)
            (overlay-put ov 'face 'secondary-selection)
            (overlay-put overlay 'remu-highlight ov))
          (goto-char (point-min)))))))

(provide 'remu-toc)
;;; remu-toc.el ends here
