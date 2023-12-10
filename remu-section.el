;;; remu-section.el --- 折叠时不刷新  -*- coding: utf-8; lexical-binding: t; -*-

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

(defvar remu-section--redisplay nil)
(put 'remu-section--redisplay 'permanent-local t)

(define-fringe-bitmap 'remu-section-collapsed [48 24 12 6 12 24 48 0])
(define-fringe-bitmap 'remu-section-expanded [0 0 65 99 54 28 8 0])

(defun remu-section--update-display (overlay open)
  (overlay-put
   overlay
   'before-string
   (propertize
    "*remu-section fringe*"
    'display
    (list
     'left-fringe
     (if open
         'remu-section-expanded
       'remu-section-collapsed))))
  (overlay-put overlay 'display (unless open "...\n")))

(defun remu-section--make (start body-start props)
  (let ((body-end (point)))
    (insert (propertize "\f" 'display ""))
    (let ((overlay (make-overlay body-start body-end nil nil t)))
      (overlay-put overlay 'evaporate t)
      (put-text-property start body-start 'remu-section overlay)
      (when-let ((redisplay (plist-get props :redisplay)))
        (put-text-property start body-start 'remu-section-redisplay redisplay)
        (overlay-put overlay 'remu-section-destroy t))
      (remu-section--update-display overlay (plist-get props :open))
      overlay)))

;;;###autoload
(defun remu-section-make (&rest props)
  (if-let ((name (plist-get props :name)))
      (let ((start (point)))
        (insert name)
        (let ((body-start (point)))
          (if-let ((face (plist-get props :face)))
              (insert (propertize "\n" 'face face))
            (insert "\n"))
          (remu-section--make start body-start props)))
    (let ((start (point))
          (body-start (line-end-position)))
      (if (< body-start (point-max))
          (goto-char (1+ body-start))
        (if-let ((face (get-text-property start 'face)))
            (insert (propertize "\n" 'face face))
          (insert "\n")))
      (remu-section--make start body-start props))))

(defun remu-section--get-overlay (match force)
  (let* ((start (prop-match-beginning match))
         (end (prop-match-end match))
         (ov (get-text-property start 'remu-section)))
    (if (and force
             (overlay-get ov 'remu-section-destroy))
        (let ((overlay (make-overlay (overlay-start ov) (overlay-end ov) nil nil t)))
          (overlay-put overlay 'evaporate t)
          (overlay-put overlay 'remu-section-destroy t)
          (overlay-put overlay 'display (overlay-get ov 'display))
          (overlay-put overlay 'before-string (overlay-get ov 'before-string))
          (delete-overlay ov)
          (put-text-property start end 'remu-section overlay)
          overlay)
      ov)))

;;;###autoload
(defmacro remu-section--with-restriction (overlay &rest body)
  (declare (indent 1) (debug (form body)))
  (let ((start-var (gensym))
        (end-var (gensym)))
    `(let ((,start-var (overlay-start ,overlay))
           (,end-var (overlay-end ,overlay)))
       (goto-char ,start-var)
       (save-excursion
         (with-restriction (1+ ,start-var) ,end-var
           (unwind-protect
               (progn ,@body)
             (goto-char (point-max))
             (when (and (> (point-max) (point-min))
                        (/= (char-after (1- (point-max))) ?\n))
               (insert "\n"))))))))

(defun remu-section--redisplay (redisplay overlay)
  (remu-section--with-restriction overlay
    (funcall redisplay overlay))
  (overlay-put overlay 'remu-section-ready t))

(defun remu-section-redisplay (redisplay match &optional force)
  (let ((inhibit-read-only t))
    (save-excursion
      (let* ((overlay (remu-section--get-overlay match force)))
        (remu-section--redisplay redisplay overlay)
        (with-restriction (point) (overlay-end overlay)
          (while-let ((m (text-property-search-forward 'remu-section-redisplay)))
            (let ((ov (remu-section--get-overlay m force)))
              (if (overlay-get ov 'display)
                  (progn
                    (overlay-put ov 'remu-section-ready nil)
                    (goto-char (overlay-end ov)))
                (remu-section--redisplay (prop-match-value m) ov)))))))))

(defun remu-section--section-at (&optional pos)
  (let ((pos (or pos (point))))
    (save-excursion
      (goto-char (1+ pos))
      (text-property-search-backward 'remu-section))))

(defun remu-section-redisplay-section (&optional force)
  (interactive "P")
  (let* ((m (remu-section--section-at))
         (pos (prop-match-beginning m))
         (overlay (prop-match-value m))
         (redisplay (get-text-property pos 'remu-section-redisplay)))
    (unless (and (overlayp overlay) redisplay)
      (user-error "Section has no redisplay"))
    (remu-section-redisplay redisplay m force)
    (let ((overlay (get-text-property pos 'remu-section)))
      (unless (overlay-get overlay 'display)
        (remu-section--update-display overlay t)))))

(defun remu-section-redisplay-buffer (&optional force)
  (interactive "P")
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (remu-section-redisplay
       remu-section--redisplay
       (text-property-search-forward 'remu-section)
       force))))

(defun remu-section-visit ()
  (interactive)
  (when-let ((pos (car (get-char-property-and-overlay (point) 'remu-pos))))
    (let ((buffer
           (if-let ((file (car (get-char-property-and-overlay (point) 'remu-file))))
               (find-file-noselect file)
             remu--current-buffer)))
      (select-window (display-buffer buffer 'display-buffer-reuse-window))
      (goto-char pos))))

(defun remu-section-toggle ()
  (interactive)
  (let* ((m (remu-section--section-at))
         (pos (prop-match-beginning m))
         (overlay (prop-match-value m)))
    (when (overlayp overlay)
      (let ((open (overlay-get overlay 'display))
            (redisplay (get-text-property pos 'remu-section-redisplay)))
        (when (and open redisplay
                   (not (overlay-get overlay 'remu-section-ready)))
          (remu-section-redisplay redisplay m))
        (remu-section--update-display overlay open)))))

(defun remu-section--search-backward (prop)
  (catch :ok
    (while-let
        ((match (text-property-search-backward prop nil nil t)))
      (if-let
          ((starts
            (mapcar
             'overlay-start
             (seq-filter
              (lambda (o) (overlay-get o 'display))
              (overlays-at (point))))))
          (goto-char (seq-min starts))
        (throw :ok (prop-match-beginning match))))
    nil))

(defun remu-section--search-forward (prop)
  (catch :ok
    (while-let
        ((match (text-property-search-forward prop nil nil t)))
      (goto-char (prop-match-beginning match))
      (if-let
          ((ends
            (mapcar
             'overlay-end
             (seq-filter
              (lambda (o) (overlay-get o 'display))
              (overlays-at (point))))))
          (goto-char (seq-max ends))
        (throw :ok (point))))
    nil))

(defun remu-section-prev ()
  (interactive)
  (if-let ((pos (save-excursion (remu-section--search-backward 'remu-section))))
      (goto-char pos)
    (user-error "Beginning of Buffer")))

(defun remu-section-next ()
  (interactive)
  (if-let ((pos (save-excursion (remu-section--search-forward 'remu-section))))
      (goto-char pos)
    (user-error "End of Buffer")))

(defun remu-section-backward ()
  (interactive)
  (if-let ((pos (save-excursion (remu-section--search-backward 'remu-item))))
      (goto-char pos)
    (user-error "Beginning of Buffer")))

(defun remu-section-forward ()
  (interactive)
  (if-let ((pos (save-excursion (remu-section--search-forward 'remu-item))))
      (goto-char pos)
    (user-error "End of Buffer")))

(defvar remu-section-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (keymap-set map "RET" 'remu-section-visit)
    (keymap-set map "<mouse-1>" 'remu-section-visit)
    (keymap-set map "TAB" 'remu-section-toggle)
    (keymap-set map "p" 'remu-section-prev)
    (keymap-set map "n" 'remu-section-next)
    (keymap-set map "f" 'remu-section-forward)
    (keymap-set map "b" 'remu-section-backward)
    (keymap-set map "r" 'remu-section-redisplay-section)
    map)
  "Parent keymap for all keymaps of modes derived from `remu-section-mode'.")

;;;###autoload
(define-derived-mode remu-section-mode special-mode "Remu-Section"
  ""
  :group 'remu-section
  (buffer-disable-undo)
  (let* ((inhibit-read-only t)
         (overlay (remu-section-make :name "root" :open t))
         (end (overlay-start overlay))
         (start (1+ end)))
    (put-text-property (point-min) start 'display "")
    (overlay-put overlay 'remu-section-destroy nil)
    (goto-char start)
    (narrow-to-region start (overlay-end overlay))))

;;;###autoload
(defun remu-section-make-sections (overlay &rest sections)
  (unless (overlay-get overlay 'remu-section-ready)
    (overlay-put overlay 'remu-section-destroy nil)
    (delete-region (point-min) (point-max))
    (dolist (section sections)
      (apply 'remu-section-make section))))


(provide 'remu-section)
;;; remu-section.el ends here
