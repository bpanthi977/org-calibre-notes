;;; org-calibre-notes.el --- Extract highlights and notes from Calibre EPUB reader -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Bibek Panthi

;; Author: Bibek Panthi <bpanthi977@gmail.com>
;; Maintainer: Bibek Panthi <bpanthi977@gmail.com>
;; URL: https://github.com/bpanthi977/org-calibre-notes
;; Version: 0.0.1
;; Package-Requires: ((counsel "0.13.4") (emacs "27.1"))
;; Kewords: epub, calibre, org

;; This file in not part of GNU Emacs

;;; License: MIT

;;; Commentary:

;; org-calibre-notes is a utility package for importing notes and
;; highlights made through Calibre Ebook Reader while reading Ebooks
;; (epub).  It imports the notes to a org file, with heading structure
;; similar to the chapters/section structure of the ebook and each
;; note/highlight kept in the corresponding heading.  It also inserts
;; a Nov.el link to the ebook section so you can jump to the highlight
;; quickly.
;;
;; To import: Open ebook in calibre reader then:
;; Export->Format = calibre highlights -> Copy to Clipboard
;; Then in Emacs do M-x org-calibre-notes-save


;;; Code:
(require 'cl-lib)
(require 'counsel)

(cl-defun org-calibre-notes-assort (seq &key (key #'identity) (test #'eql) (start 0) end)
  "Return SEQ assorted by KEY.

     (assort (iota 10)
             :key (lambda (n) (mod n 3)))
     => '((0 3 6 9) (1 4 7) (2 5 8))

Groups are ordered as encountered.  This property means you could, in
principle, use `assort' to implement `remove-duplicates' by taking the
first element of each group:

     (mapcar #'first (assort list))
     ≡ (remove-duplicates list :from-end t)

However, if TEST is ambiguous (a partial order), and an element could
qualify as a member of more than one group, then it is not guaranteed
that it will end up in the leftmost group that it could be a member
of.

START and END specify which portion of SEQ should be assorted.
From Serapeum Library (Common Lisp)"
  (let ((groups nil)
        last-group)
    (cl-map 'nil (lambda (item)
                (let* ((kitem (funcall key item))
                       (group (if (and last-group
                                       (funcall test kitem (car last-group)))
                                  last-group
                                (cl-find-if
                                 (lambda (group)
                                   (funcall test kitem (car group)))
                                 groups))))
                  (cond (group
                         (setf last-group group)
                         (push item (cdr group)))
                        (t
                         (push (cons kitem (cons item nil)) groups)))))
         (cl-subseq seq start end))
        (mapcar (lambda (group)
                  (nreverse (cdr group)))
                (nreverse groups))))


(defun org-calibre-notes-parse-and-insert (json-string file)
  "Parse json `JSON-STRING' exported from calibre and insert into org FILE."
  (cl-flet ((note-headings (highlight)
                        (gethash "toc_family_titles" highlight)))
    (let ((json (ignore-errors (json-parse-string json-string))))
      (if (null json)
          (message "invalid json string")
        (let ((notes-group (org-calibre-notes-assort (gethash "highlights" json) :key #'note-headings
                                   :test #'cl-equalp)))
          (cl-flet ((print-heading (note heading extra-level)
                                   (when (> (length heading) 0)
                                     (cl-loop for title across heading
                                              for i from (1+ extra-level) do
                                              (insert (format "%s %s\n" (make-string i ?*) title))
                                              (when file
                                                (insert (format "[[nov:%s::%d:0][link]]\n"
                                                                file
                                                                (1+ (gethash "spine_index" note))))))))
                    (print-note (note)
                                (insert (format "%s\n\n" (gethash "highlighted_text" note)))))
            (let ((prev-heading nil))
              (cl-loop for notes in notes-group
                       for heading = (note-headings (cl-first notes)) do
                       (unless (cl-equalp heading prev-heading)
                         (let ((max-match-length (cl-loop for a across prev-heading
                                                          for b across heading
                                                          for i from 0
                                                          unless (cl-equalp a b)
                                                          return i
                                                          finally (return i))))
                           (if (= max-match-length 0)
                               ;; completely new heading
                               (print-heading (elt notes 0) heading 0)
                             ;; subset of previous heading
                             (print-heading (elt notes 0) (cl-subseq heading max-match-length) max-match-length))
                           (setf prev-heading heading)))
                       (cl-loop for note in notes
                                do (print-note note))))))))))

(defun org-calibre-notes-select-ebook-file ()
  "Present user with an interface to select the ebook file."
  (let ((default-directory
          (if (eq major-mode 'dired-mode)
              (dired-current-directory)
            "~/Documents/")))
    (let ((ivy-sort-functions-alist '((read-file-name-internal . file-newer-than-file-p))))
      (ivy-read "EPub File: " #'read-file-name-internal
                :matcher #'counsel--find-file-matcher
                :preselect (counsel--preselect-file)
                :require-match 'confirm-after-completion
                :sort t
                :history 'file-name-history
                :keymap counsel-find-file-map
                :caller 'org-calibre-notes))))

(defun org-calibre-notes-select-save-file (file)
  "Present user with an interface to select the org file in which to save notes.

FILE is the source ebook file."
  (let ((default-directory
          (cond (file (file-name-directory file))
                ((eq major-mode 'dired-mode)
                 (dired-current-directory))
                (t "~/org/"))))
    (ivy-read "Save to: " #'read-file-name-internal
              :initial-input (concat (file-name-base file) ".org")
              :matcher #'counsel--find-file-matcher
              :preselect (counsel--preselect-file)
              :require-match 'confirm-after-completion
              :history 'file-name-history
              :keymap counsel-find-file-map
              :caller 'org-calibre-notes)))

(defun org-calibre-notes-save ()
  "Save calibre notes (exported as json to clipboard) into a org file."
  (interactive)
  (let ((parsed-json (ignore-errors (json-parse-string (current-kill 0)))))
    (unless parsed-json
      (message "invalid json. Open ebook in calibre reader then: Export-> Format = calibre highlights -> Copy to Clipboard"))
    (when parsed-json
      (let* ((source (org-calibre-notes-select-ebook-file))
             (savefile (org-calibre-notes-select-save-file source)))
        (when savefile
          (find-file savefile)
          (org-calibre-notes-parse-and-insert parsed-json source))))))

(provide 'org-calibre-notes)
;;; org-calibre-notes.el ends here
