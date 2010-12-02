;;; etags-mode.el --- etags under your control

;; Copyright (C) 2009 Free Software Foundation, Inc.
;;
;; Author: Seungcheol Jung <scjung.hyu@gmail.com>
;; Maintainer: Seungcheol Jung <scjung.hyu@gmail.com>
;; Created: 02 May 2009
;; Version: 0.1
;; Keywords

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;   TODO

;;; Required utility:
;;   etags
;;   find   - to build TAGS file of all target files recursivly

;;; TODO:
;;   rebuild TAGS periodically
;;   error handling (error message of etags)
;;   prohibit two etags at the same time
;;   documentation
;;   customization

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'etags-mode)

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'etags)
(require 'basedir)

(defgroup etags-mode nil
  "manage Emacs tags in a convenient way"
  :group 'tools)

(defcustom etags-major-modes
  '(emacs-lisp-mode
    lisp-interaction-mode
    c-mode cc-mode c++-mode clojure-mode java-mode
    perl-mode cperl-mode python-mode ruby-mode
    ecmascript-mode javascript-mode js2-mode php-mode css-mode
    makefile-mode sh-mode fortran-mode f90-mode ada-mode
    xml-mode sgml-mode
    tuareg-mode)
  "Major modes `etags-mode' may run on. See also `global-etags-mode'."
  :type '(repeat symbol)
  :group 'etags-mode)

(defcustom etags-bin-alist
 '((default-etags . "etags")
   (tuareg-mode   . "otags")
   (caml-mode     . "otags"))
 "Etags binaries for specific modes. If a mode is not specified here,
`etags-default-bin' is used by default."
 :group 'etags-mode
 :type '(alist :key-type '(symbol) :value-type '(file)))

(defcustom etags-default-bin "etags"
  "default etags binary. It is used when there is no proper association in
`etags-bin-alist'."
  :group 'etags-mode
  :type '(string))

(defcustom etags-mode-auto-generate-tags-if-absent t
  "If t, it generates TAGS file if it does not exists when the mode starts."
  :type 'boolean
  :group 'etags-mode)

(defcustom etags-mode-use-tags-existing-directory t
  "Non-nil means `etags-mode' use the directory containing a TAGS file
without asking."
  :type 'boolean
  :group 'etags-mode)

(defvar etags-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-e" 'etags-build-tags-of-all)
    map)
  "Keymap used in 'etags-mode'")

(define-minor-mode etags-minor-mode
  "Etags mode"
  :lighter " Etags"
  :group 'etags-mode
  :keymap etags-mode-map
  (when etags-minor-mode
    (etags-mode-start)))

(defvar etags-file-name "TAGS"
  "TAGS file name")

(make-local-variable 'etags-file-name)

(defun etags-mode-enable-if-tags-exists ()
  "Enable etags-mode if TAGS exists"
  (if (and (not (minibufferp (current-buffer)))
           (memq major-mode etags-major-modes))
      (let ((basedir (basedir-of-current-buffer)))
        (when (and basedir
                   (file-exists-p (concat (file-name-as-directory basedir)
                                          etags-file-name)))
          (etags-minor-mode 1)))
    nil))

(define-global-minor-mode global-etags-mode
  etags-minor-mode etags-mode-enable-if-tags-exists
  :group 'etags-mode)

(defvar etags-mode-target-regex-alist
  '((cc-mode         . ".*\.[cChH]\(pp)?")
    (emacs-lisp-mode . ".*\.el")
    (tuareg-mode     . ".*\\.ml")
    (caml-mode       . ".*\\.ml")
    (php-mode        . ".*\.php")))

(defvar etags-mode-target-regex nil
  "Regular expression of target files of etags")

(make-local-variable 'etags-mode-target-regex)

(defvar etags-dir nil
  "A directory contains TAGS file.")

(make-local-variable 'etags-dir)

(defvar etags-bin "etags"
  "Etags command. It might be possible to change this value to
third-party TAGS builder, such as otags for Objective-Caml.")

(make-local-variable 'etags-bin)

(defvar etags-buffer "*etags*"
  "a buffer for output of etags processes")

(defun etags-mode-start ()
  (let ((regex (cdr-safe (assoc major-mode etags-mode-target-regex-alist))))
    (if regex
        (progn
          (setq etags-mode-target-regex regex)
          (etags-mode-set-dir)
          (etags-set-bin)
          (when etags-mode-auto-generate-tags-if-absent
            (unless (file-exists-p (etags-file))
              (etags-build-tags-of-all)))
          (when (and (featurep 'auto-complete) (featurep 'auto-complete-etags))
            (add-to-list 'ac-sources 'ac-source-etags)))
      (message "No TAGS generator for the current file")
      (etags-minor-mode nil))))

(defun etags-file ()
  (concat (file-name-as-directory etags-dir) etags-file-name))

(defun etags-builder-of-file (tags-file file)
  (concat
   (shell-quote-argument etags-bin) " -a "
   (shell-quote-argument file) " -o "
   (shell-quote-argument tags-file)))

(defun etags-run (command)
  (message command)
  (shell-command (concat command "&") "*etags*")
  (kill-buffer "*etags*"))

(defun etags-mode-set-dir ()
  (let ((dir (basedir-of-current-buffer)))
    (setq etags-dir
          (if (and dir
                   (file-exists-p (concat (file-name-as-directory dir)
                                          etags-file-name)))
              dir
            (expand-file-name
             (file-name-as-directory
              (if dir
                  dir
                (let ((insert-default-directory t))
                  (read-directory-name "Directory: " nil nil t)))))))
    (message (concat "TAGS directory: " etags-dir))
    (add-to-list 'tags-table-list (etags-file))))

(defun etags-set-bin ()
  (let ((bin (cdr (assoc major-mode etags-bin-alist))))
    (setq etags-bin
          (if bin bin etags-default-bin))))

(defun etags-build-result (process event)
  (let ((msg
         (if (string= event "finished\n")
             (format "%s has been built successfully." etags-file-name)
           (format "Failed to build %s" etags-file-name))))
    (message msg)))

(defun etags-build-tags-of-all ()
  (interactive)
  (message (format "Building %s for %s in %s..."
                   etags-file-name etags-mode-target-regex etags-dir))
  (when (buffer-live-p (get-buffer etags-buffer)) (kill-buffer etags-buffer))
  (let ((proc (start-process "etags-build-tags-of-all"
                             etags-buffer
                             "find" (shell-quote-argument etags-dir) "-regex"
                             etags-mode-target-regex
                             "-print"
                             "-exec" (shell-quote-argument etags-bin)
                             "-o" (shell-quote-argument (etags-file))
                             "{}" "+"
                             )))
    (set-process-sentinel proc 'etags-build-result)))

(defun etags-rebuild-tags-of-all ()
  (interactive)
  (ignore-errors (delete-file (etags-file)))
  (etags-build-tags-of-all))

(defun etags-build-tags-of-file (file)
  (interactive)
  (message (format "Building %s for %s..." etags-file-name file))
  (let ((proc (start-process (concat "etags-build-tags-of-file: " file)
                             etags-buffer
                             (shell-quote-argument etags-bin)
                             "-a" "-o" (shell-quote-argument (etags-file))
                             (shell-quote-argument file))))
    (set-process-sentinel proc 'etags-build-result)))

(defun etags-build-tags-of-current-file ()
  (interactive)
  (etags-build-tags-of-file buffer-file-name))

(provide 'etags-mode)
;;; etags-mode.el ends here
