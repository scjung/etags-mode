;;; basedir.el --- Find base directory of current directory automagically

;; Copyright (C) 2009 Free Software Foundation, Inc.
;;
;; Author: Seungcheol Jung <scjung.hyu@gmail.com>
;; Maintainer: Seungcheol Jung <scjung.hyu@gmail.com>
;; Created: 30 Dec 2009
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


;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'basedir)

;;; Code:

(eval-when-compile
  (require 'cl))

(defun basedir-vcs-directory-p (dir)
  (let ((vcs nil))
    (dolist (file (directory-files dir) vcs)
      (when (file-directory-p file)
        (cond ((string= file "CVS") (setq vcs "cvs"))
              ((string= file ".svn") (setq vcs "svn"))
              ((string= file ".git") (setq vcs "git"))
              ((string= file ".hg") (setq vcs "hg"))
              ((string= file "_darcs") (setq vcs "darcs"))
              (t ())
              )))))

(defun basedir-root-vcs-directory-p (dir)
  (and (basedir-vcs-directory-p dir)
       (not (basedir-vcs-directory-p (expand-file-name ".." dir)))))

(defun basedir-of (dir)
  (if (basedir-root-vcs-directory-p dir)
      dir
    (let ((parent (expand-file-name ".." dir)))
      (if (or (string= parent "/") ; FIXME: not portable code
              (string= dir parent))
          nil
        (basedir-of parent)))))

(defun basedir-of-current-buffer ()
  (if default-directory
      (basedir-of default-directory)
    nil))

(provide 'basedir)
;;; basedir.el ends here
