;;; remu-link.el --- 链接  -*- coding: utf-8; lexical-binding: t; -*-

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
(require 'org-macs)
(require 'remu-section)

(defcustom remu-link-property-refs "REMU_REFS"
  ""
  :group 'remu-link
  :type 'string)

(defun remu-link--trim-link (link)
  (let* ((type (org-element-property :type link))
         (props (list
                 :type type
                 :path (org-element-property :path link))))
    (when (equal type "file")
      (plist-put props :application (org-element-property :application link))
      (plist-put props :search-option (org-element-property :search-option link)))
    (list 'link props)))

(defun remu-link--parse-link ()
  (with-current-buffer remu--current-buffer
    (when-let ((face (get-text-property (point) 'face)))
      (when (or (eq 'org-link face)
                (and (listp face) (memq 'org-link face)))
        (if-let ((range (org-in-regexp org-link-any-re)))
            (save-excursion
              (goto-char (car range))
              (cons range (remu-link--trim-link (org-element-link-parser))))
          (when (eq (get-text-property (point) 'org-linked-text) t)
            (let ((start (previous-single-property-change (1+ (point)) 'org-linked-text))
                  (end (next-single-property-change (point) 'org-linked-text)))
              (cons
               (cons start end)
               (list
                'link
                (list
                 :type "radio"
                 :path (buffer-substring-no-properties start end)))))))))))

(defun remu-link--normalize-search (s)
  (replace-regexp-in-string "\n[ \t]*" " " s))

(defun remu-link--find-link-file (link)
  (let ((type (org-element-property :type link))
        (path (org-element-property :path link)))
    (or
     (pcase type
       ("radio"
        (list (current-buffer) type path))
       ("custom-id"
        (list (current-buffer) 'search (remu-link--normalize-search (concat "#" path))))
       ("coderef"
        (list (current-buffer) 'search (remu-link--normalize-search (format "(%s)" path))))
       ("fuzzy"
        (list (current-buffer) 'search (remu-link--normalize-search path)))
       ("id"
        (let ((file (org-id-find-id-file path)))
          (if-let ((buffer (org-find-base-buffer-visiting file)))
              (list buffer 'id path)
            (when (file-exists-p file)
              (list file 'id path)))))
       ((and "file"
             (guard (not (equal "sys" (org-element-property :application link))))
             (let `,option (org-element-property :search-option link))
             (guard option)
             (let `,search-option
               (if (string-match-p "\\`[0-9]+\\'" option)
                   (list 'line (string-to-number option))
                 (list 'search (remu-link--normalize-search option)))))
        (cond
         ((string-match-p "[*?{]" (file-name-nondirectory path)) nil)
         ((equal path "") (cons (current-buffer) search-option))
         (t
          (when-let*
              ((file
                (pcase (substitute-in-file-name (expand-file-name path))
                  ((pred file-remote-p) nil)
                  ((and (pred file-directory-p) file)
                   (when org-open-directory-means-index-dot-org
                     (concat (file-name-as-directory file) "index.org")))
                  (file
                   file)))
               (mode (assoc-default file auto-mode-alist 'string-match-p)))
            (when (and (symbolp mode)
                       (provided-mode-derived-p mode 'org-mode))
              (if-let ((buffer (org-find-base-buffer-visiting file)))
                  (cons buffer search-option)
                (when (file-exists-p file)
                  (cons file search-option)))))))))
     (list (current-buffer) 'refs (format "%s:%s" type path)))))

(defun remu-link--insert-file (output-buffer)
  (let* ((file-name (buffer-file-name))
         (title (or (org-get-title) (file-name-base file-name)))
         (s (concat
             (propertize title 'face 'org-document-title)
             " (" (file-name-nondirectory file-name) ")"))
         (overlay
          (with-current-buffer output-buffer
            (goto-char (point-max))
            (remu-section-make
             :name (propertize
                    s
                    'help-echo
                    (with-current-buffer remu--current-buffer
                      (file-relative-name file-name)))
             :open t))))
    (overlay-put overlay 'remu-file file-name)
    overlay))

(defun remu-link--get-heading-overlay (pos)
  (let ((overlay (get-text-property pos 'remu-section)))
    (if (overlayp overlay)
        overlay
      (goto-char pos)
      (remu-section-make :open t))))

(defun remu-link--insert-section (pos s)
  (goto-char (point-max))
  (let ((start (point)))
    (insert (propertize s 'remu-section t 'remu-pos pos))
    (insert "\n")
    start))

(defun remu-link--insert (file-overlay heading paragraph pos s)
  (with-current-buffer (overlay-buffer file-overlay)
    (remu-section--with-restriction
        (if heading (remu-link--get-heading-overlay heading) file-overlay)
      (when (and paragraph
                 (eq (point-min) (point-max)))
        (goto-char (point-max))
        (insert "\n"))
      (remu-link--insert-section pos s))))

(defun remu-link--copy-paragraph (file-overlay &optional heading)
  (let ((pos (point)))
    (remu-link--insert
     file-overlay heading
     (if heading
         (>= pos
             (save-excursion
               (org-end-of-meta-data t)
               (point)))
       (not (or (org-at-keyword-p) (org-at-property-drawer-p))))
     pos
     (concat
      (buffer-substring
       pos
       (save-excursion
         (org-forward-paragraph)
         (skip-chars-backward " \t\n")
         (point)))
      "\n"))))

(defun remu-link--copy-heading (file-overlay &optional parent)
  (remu-link--insert
   file-overlay parent nil (point)
   (buffer-substring
    (point)
    (save-excursion
      (org-end-of-line)
      (point)))))

(defun remu-link--get-heading (file-overlay &optional last-org-pos last-output-pos)
  (let ((pos (point)))
    (cond
     ((eq pos last-org-pos) last-output-pos)
     ((eq pos (point-min)) nil)
     ((and last-org-pos (< pos last-org-pos))
      (with-current-buffer (overlay-buffer file-overlay)
        (goto-char last-output-pos)
        (while (let ((match (text-property-search-backward 'remu-pos)))
                 (not (eq (prop-match-value match) pos))))
        (point)))
     (t
      (remu-link--copy-heading
       file-overlay
       (save-excursion
         (when (org-up-heading-safe)
           (remu-link--get-heading file-overlay last-org-pos last-output-pos))))))))

(defun remu-link--highlight-match (buffer start end &optional last-pos)
  (let* ((last-file-overlay (when last-pos (car last-pos)))
         (last-file-name
          (when last-file-overlay
            (overlay-get last-file-overlay 'remu-file)))
         (file-changed (not (equal last-file-name buffer-file-name)))
         (file-overlay
          (if file-changed
              (remu-link--insert-file buffer)
            last-file-overlay))
         (last-org-pos (unless file-changed (cadr last-pos)))
         (last-output-pos (unless file-changed (cddr last-pos)))
         (org-start
          (save-excursion
            (org-backward-paragraph)
            (skip-chars-forward " \t\n")
            (point)))
         (output-start
          (cond
           ((eq org-start last-org-pos)
            last-output-pos)
           ((org-before-first-heading-p)
            (goto-char org-start)
            (remu-link--copy-paragraph file-overlay))
           ((eq org-start (save-excursion (org-back-to-heading) (point)))
            (goto-char org-start)
            (remu-link--get-heading file-overlay last-org-pos last-output-pos))
           (t
            (goto-char org-start)
            (remu-link--copy-paragraph
             file-overlay
             (save-excursion
               (org-back-to-heading)
               (remu-link--get-heading file-overlay last-org-pos last-output-pos))))))
         (offset (- output-start org-start))
         (overlay
          (with-current-buffer buffer
            (let ((beg (+ start offset))
                  (end (+ end offset)))
              (put-text-property beg end 'remu-item start)
              (make-overlay beg end)))))
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'face 'match)
    (overlay-put overlay 'remu-pos start)
    (cons file-overlay (cons org-start output-start))))

(defun remu-link--search-radio-target (path)
  (ignore-errors
    (cl-letf (((symbol-function 'org-fold-show-context) #'identity))
      (org-link--search-radio-target path)
      t)))

(defun remu-link--search (s avoid-pos)
  (ignore-errors
    (cl-letf* ((org-link-search-must-match-exact-headline nil)
               (split-string (symbol-function 'split-string))
               ((symbol-function 'split-string)
                (lambda (string &optional separators omit-nulls trim)
                  (save-match-data
                    (funcall split-string string separators omit-nulls trim)))))
      (org-link-search s avoid-pos t)
      t)))

(defun remu-link--display-target (output-buffer range type path &optional same-buffer)
  (org-with-wide-buffer
   (cond
    ((equal type "radio")
     (when (remu-link--search-radio-target path)
       (remu-link--highlight-match output-buffer (match-beginning 0) (match-end 0))))
    ((eq type 'line)
     (goto-char (point-min))
     (forward-line (1- path))
     (remu-link--highlight-match output-buffer (point) (line-end-position)))
    ((eq type 'id)
     (org-find-property "ID" path)
     (goto-char (match-beginning 0))
     (remu-link--highlight-match output-buffer (point) (match-end 0)))
    ((eq type 'refs)
     (let ((regexp (org-re-property remu-link-property-refs nil nil path))
           (last-pos nil))
       (goto-char (point-min))
       (while (let ((case-fold-search t))
                (re-search-forward regexp nil t))
         (unless (and same-buffer (eq (match-end 0) (cdr range)))
           (setq last-pos
                 (save-excursion
                   (remu-link--highlight-match
                    output-buffer (match-beginning 0) (match-end 0) last-pos)))))))
    ((string-match "\\`/\\(.*\\)/\\'" path)
     (let* ((regexp (match-string 1 path))
            (remu-case-fold-search
             (if (eq org-occur-case-fold-search 'smart)
		 (isearch-no-upper-case-p regexp t)
	       org-occur-case-fold-search))
            (last-pos nil))
       (goto-char (point-min))
       (while
           (let ((case-fold-search remu-case-fold-search))
             (re-search-forward regexp nil t))
         (setq last-pos
               (save-excursion
                 (remu-link--highlight-match
                  output-buffer (match-beginning 0) (match-end 0) last-pos))))))
    ((string-match-p "\\`(\\(.*\\))\\'" path)
     (when (remu-link--search path (when same-buffer (+ 2 (car range))))
       (let ((start (match-beginning 2))
             (end (match-end 2)))
         (goto-char end)
         (remu-link--highlight-match output-buffer start end))))
    ((remu-link--search path (when same-buffer (+ 2 (car range))))
     (goto-char (match-beginning 0))
     (remu-link--highlight-match output-buffer (point) (match-end 0))))))

;;;###autoload
(defun remu-link-target-section (overlay)
  (pcase (remu-link--parse-link)
    (`(,range . ,link)
     (pcase-let*
         ((output-buffer (current-buffer))
          (`(,file ,type ,path)
           (with-current-buffer remu--current-buffer
             (remu-link--find-link-file link)))
          (tick
           (cons file
                 (cond
                  ((bufferp file)
                   (buffer-chars-modified-tick file))
                  ((stringp file)
                   (file-attribute-modification-time (file-attributes file)))))))
       (unless (and
                (equal link (overlay-get overlay 'remu-link))
                (equal tick (overlay-get overlay 'remu-tick)))
         (delete-region (point-min) (point-max))
         (overlay-put overlay 'remu-link link)
         (overlay-put overlay 'remu-tick tick)
         (cond
          ((bufferp file)
           (let ((same-buffer (eq file remu--current-buffer)))
             (with-current-buffer file
               (remu-link--display-target output-buffer range type path same-buffer))))
          ((stringp file)
           (with-temp-buffer
             (let ((buffer-file-name file)
                   (default-directory (file-name-directory file)))
               (insert-file-contents file)
               (org-mode)
               (font-lock-ensure)
               (remu-link--display-target output-buffer range type path))))
          (t
           (insert (format "%s" link)))))))
    ((guard (overlay-get overlay 'remu-link))
     (delete-region (point-min) (point-max))
     (overlay-put overlay 'remu-link nil)
     (overlay-put overlay 'remu-tick nil))))

(defun remu-link--resolve-local-link (range link)
  (if (equal "id" (org-element-property :type link))
      (cons 'id (org-element-property :path link))
    (pcase-let ((`(,file ,type ,path) (remu-link--find-link-file link)))
      (when (equal file (current-buffer))
        (cond
         ((eq type 'refs)
          (cons 'refs path))
         ((eq type 'line)
          (goto-char (point-min))
          (forward-line (1- path))
          (cons 'match (cons (point) (line-end-position))))
         ((eq (string-to-char path) ?#)
          (cons 'custom-id (substring path 1)))
         ((string-match "\\`/\\(.*\\)/\\'"
                        (replace-regexp-in-string "\n[ \t]*" " " path))
          (cons 'regexp (match-string 1 path)))
         ((remu-link--search path (+ 2 (car range)))
          (cons 'match (cons (match-beginning 0) (match-end 0)))))))))

(defun remu-link--collect-links (table &optional skip-radio data)
  (let ((matches nil)
        (regexps nil))
    (goto-char (point-min))
    (while (re-search-forward org-link-any-re nil t)
      (save-excursion
        (goto-char (match-beginning 0))
        (let* ((range (cons (point) (match-end 0)))
               (value (or data range)))
          (pcase (org-with-wide-buffer
                  (remu-link--resolve-local-link
                   range (remu-link--trim-link (org-element-link-parser))))
            (`(match . ,match)
             (push (cons match value) matches))
            (`(regexp . ,regexp)
             (push (cons value regexp) regexps))
            ((and `(,_ . ,_) `,key)
             (puthash key (cons value (gethash key table)) table))))))
    (unless skip-radio
      (let ((targets (make-hash-table :test 'equal)))
        (goto-char (point-min))
        (while-let ((match (text-property-search-forward 'org-linked-text)))
          (let* ((start (prop-match-beginning match))
                 (end (prop-match-end match))
                 (path (buffer-substring-no-properties start end))
                 (range (cons start end))
                 (data (or data range))
                 (face (get-text-property start 'face)))
            (when (and
                   (or (eq 'org-link face)
                       (and (listp face) (memq 'org-link face))))
              (pcase (gethash path targets)
                ((and `(,_ . ,_) `,match)
                 (push (cons match data) matches))
                (`nil
                 (puthash
                  path
                  (if (save-excursion
                        (org-with-wide-buffer (remu-link--search-radio-target path)))
                      (let ((match (cons (match-beginning 0) (match-end 0))))
                        (push (cons match data) matches)
                        match)
                    :error)
                  targets))))))))
    (cons matches regexps)))

(defun remu-link--collect (&optional skip-radio)
  (let ((table (make-hash-table :test 'equal)))
    (org-with-wide-buffer
     (pcase-let ((`(,matches . ,regexps) (remu-link--collect-links table skip-radio)))
       (list matches regexps table)))))

(defun remu-link--get-matches (links headline &optional dedup)
  (pcase-let*
      ((`(,matches ,regexps ,table) links)
       (pos (point))
       (nonre-matches
        (append
         (mapcar
          'cdr
          (cond
           ((numberp headline)
            (seq-filter
             (lambda (x) (and (>= (caar x) pos) (>= headline (cdar x))))
             matches))
           ((not headline)
            (seq-filter
             (lambda (x) (and (<= (caar x) pos) (< pos (cdar x))))
             matches))))
         (when (or headline
                   (or (eq pos (point-min)) (org-at-heading-p)))
           (append
            (when-let ((id (org-entry-get pos "ID")))
              (gethash (cons 'id id) table))
            (when-let ((id (org-entry-get pos "CUSTOM_ID")))
              (gethash (cons 'custom-id id) table))
            (when-let ((id (org-entry-get pos remu-link-property-refs)))
              (gethash (cons 'refs id) table))))))
       (all-matches (if dedup (seq-uniq nonre-matches) nonre-matches)))
    (pcase-dolist (`(,value . ,re) regexps)
      (let ((case-fold-search
             (if (eq org-occur-case-fold-search 'smart)
	         (isearch-no-upper-case-p re t)
	       org-occur-case-fold-search)))
        (when (cond
               ((and dedup (member value all-matches))
                nil)
               ((numberp headline)
                (with-restriction pos headline
                  (goto-char pos)
                  (re-search-forward re nil t)))
               ((not headline) (org-in-regexp re)))
          (push value all-matches))))
    all-matches))

(defun remu-link--display-links (buffer links &optional headline)
  (let ((last-pos nil))
    (pcase-dolist
        (`(,start . ,end)
         (seq-sort-by 'car '< (remu-link--get-matches links headline)))
      (goto-char start)
      (setq last-pos (remu-link--highlight-match buffer start end last-pos)))))

(defun remu-link--insert-outline-path (output-buffer)
  (unless (org-before-first-heading-p)
    (let ((outline-path (org-format-outline-path (org-get-outline-path t))))
      (with-current-buffer output-buffer
        (insert outline-path)
        (insert "\n")))))

;;;###autoload
(defun remu-link-back-headline-section (overlay)
  (let* ((output-buffer (current-buffer))
         (tick (buffer-chars-modified-tick remu--current-buffer))
         (modified (not (eq tick (overlay-get overlay 'remu-tick))))
         (links
          (if modified
              (with-current-buffer remu--current-buffer
                (remu-link--collect t))
            (overlay-get overlay 'remu-links)))
         (pos
          (with-current-buffer remu--current-buffer
            (org-with-wide-buffer
             (org-back-to-heading-or-point-min)
             (point))))
         (old-pos (overlay-get overlay 'remu-pos))
         (moved (not (eq pos old-pos))))
    (when modified
      (overlay-put overlay 'remu-tick tick)
      (overlay-put overlay 'remu-links links))
    (when moved
      (overlay-put overlay 'remu-pos pos))
    (when (or modified moved)
      (delete-region (point-min) (point-max))
      (with-current-buffer remu--current-buffer
        (org-with-wide-buffer
         (goto-char pos)
         (remu-link--insert-outline-path output-buffer)
         (remu-link--display-links
          output-buffer links
          (if (org-at-heading-p) (line-end-position) t)))))))

;;;###autoload
(defun remu-link-back-section (overlay)
  (let* ((output-buffer (current-buffer))
         (tick (buffer-chars-modified-tick remu--current-buffer))
         (modified (not (eq tick (overlay-get overlay 'remu-tick))))
         (links
          (if modified
              (with-current-buffer remu--current-buffer
                (remu-link--collect))
            (overlay-get overlay 'remu-links)))
         (pos
          (with-current-buffer remu--current-buffer
            (point)))
         (old-pos (overlay-get overlay 'remu-pos))
         (moved (not (eq pos old-pos))))
    (when modified
      (overlay-put overlay 'remu-tick tick)
      (overlay-put overlay 'remu-links links))
    (when moved
      (overlay-put overlay 'remu-pos pos))
    (when (or modified moved)
      (delete-region (point-min) (point-max))
      (with-current-buffer remu--current-buffer
        (org-with-wide-buffer
         (remu-link--display-links output-buffer links))))))

(provide 'remu-link)
;;; remu-link.el ends here
