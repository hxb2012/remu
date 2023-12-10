;;; remu.el --- Org Mode侧栏  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2020-2022 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>, Carsten Dominik <carsten.dominik@gmail.com>
;; Maintainer: 洪筱冰 <hxb@localhost.localdomain>
;; URL: https://github.com/hxb2012/remu
;; Keywords: Org
;; Package-Requires: ((emacs "29.0") (org "9.6"))
;; Version: 0.0.1

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

(require 'remu-section)

(defcustom remu-sections
  '(
    (:name "Contents" :redisplay remu-toc-section :open t)
    (:name "Link Target" :redisplay remu-link-target-section)
    (:name "Headline Backlink" :redisplay remu-link-back-headline-section)
    (:name "Backlink" :redisplay remu-link-back-section)
    (:name "TODO" :redisplay remu-todo-section)
    (:name "Debug" :redisplay remu-debug-section)
   )
  ""
  :group 'remu)

(defgroup remu-faces nil
  "Faces used by Remu"
  :group 'remu
  :group 'faces)

(defface remu-section-header
  '((((class color) (background light))
     :extend t
     :background "grey80"
     :foreground "grey30"
     :weight bold)
    (((class color) (background dark))
     :extend t
     :background "grey25"
     :foreground "grey70"
     :weight bold))
  "Face for section header."
  :group 'remu-faces)

(defvar remu-buffer "*remu*"
  "The persistent remu buffer name. Must be surround with
\"*\".  The content inside of this buffer will be automatically
updated to the nearest node at point that comes from the current
buffer.  To toggle its display use `remu-toggle' command.")

(defvar remu--current-buffer nil)
(put 'remu--current-buffer 'permanent-local t)

(define-derived-mode remu-mode remu-section-mode "Remu"
  "Major mode for displaying relevant information about remu
nodes.  This mode is used by special remu buffers, which render
the information in a section-like manner (see `remu-sections'),
with which the user can interact with."
  :group 'remu
 (setq-local remu-section--redisplay 'remu--root-redisplay))

(defun remu--root-redisplay (overlay)
  (apply
   'remu-section-make-sections
   overlay
   (mapcar
    (lambda (sec)
      (list
       :name (propertize (plist-get sec :name) 'face 'remu-section-header)
       :redisplay (plist-get sec :redisplay)
       :open (plist-get sec :open)
       :face 'remu-section-header))
    remu-sections)))

;;;###autoload
(defun remu ()
  "Toggle display of the persistent `remu-buffer'."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not an Org buffer"))
  (let ((window (get-buffer-window remu-buffer)))
    (if (and window
             (eq (current-buffer)
                 (with-current-buffer remu-buffer remu--current-buffer)))
        (quit-window nil window)
      (remu-buffer-redisplay)
      (display-buffer remu-buffer))))

(defun remu-buffer-redisplay ()
  (let ((buffer (current-buffer))
        (sidebar-buffer (get-buffer-create remu-buffer)))
    (with-current-buffer sidebar-buffer
      (let ((created remu--current-buffer)
            (changed (not (eq buffer remu--current-buffer))))
        (when changed
          (remu-buffer-remove-hook)
          (setq-local remu--current-buffer buffer)
          (with-current-buffer buffer
            (add-hook 'post-command-hook 'remu-buffer--redisplay-h nil t))
          (unless created
            (remu-mode)
            (add-hook 'kill-buffer-hook 'remu-buffer-remove-hook t)))
        (remu-section-redisplay-buffer (and created changed))))))

(defun remu-buffer-remove-hook ()
  (when (and remu--current-buffer
             (buffer-live-p remu--current-buffer))
    (with-current-buffer remu--current-buffer
      (remove-hook 'post-command-hook 'remu-buffer--redisplay-h t))))

(defun remu-buffer--redisplay-h ()
  (when (get-buffer-window remu-buffer)
    (remu-buffer-redisplay)))

(provide 'remu)
;;; remu.el ends here
