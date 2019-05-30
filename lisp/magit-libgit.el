;;; magit-libgit.el --- Libgit functionality       -*- lexical-binding: t -*-

;; Copyright (C) 2010-2019  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; Package-Requires: ((emacs "26.1") (magit "0") (libgit "0"))
;; Keywords: git tools vc
;; Homepage: https://github.com/magit/magit

;; Magit is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; This package teaches Magit to use functions provided by the
;; `libegit2' module to perform certain tasks.  That module used the
;; Libgit2 implementation of the Git core methods and is implemented
;; in the `libgit' package.

;; The hope is that using a C module instead of calling out to `git'
;; all the time increases performance; especially on Windows where
;; starting a process is unreasonably slow.

;; This package is still experimental and not many functions have been
;; reimplemented to use `libgit' yet.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'magit-git)
(require 'libgit)

;;; Utilities

(defun magit-libgit-repo (&optional directory)
  "Return an object for the repository in DIRECTORY.
If optional DIRECTORY is nil, then use `default-directory'."
  (when-let ((default-directory
               (let ((magit-inhibit-libgit t))
                 (magit-gitdir directory))))
    (magit--with-refresh-cache
        (cons default-directory 'magit-libgit-repo)
      (libgit-repository-open default-directory))))

;;; Methods

(cl-defmethod magit-bare-repo-p
  (&context ((magit-gitimpl) (eql libgit)) &optional noerror)
  (and (magit--assert-default-directory noerror)
       (if-let ((repo (magit-libgit-repo)))
           (libgit-repository-bare-p repo)
         (unless noerror
           (signal 'magit-outside-git-repo default-directory)))))

(cl-defmethod magit-file-tracked-p
  (file &context ((magit-gitimpl) (eql libgit)))
  (and (magit--assert-default-directory)
       (if-let ((repo (magit-libgit-repo)))
           ;; FIXME: not sure how to check if a file is untracked.
           ;; libgit errors out with Git error: invalid: "attempt to
           ;; get status of nonexistent file"
           ;;
           ;; Possible solution:
           ;; Extend libgit to check for the error code and return an
           ;; additional status'untracked?
           (condition-case nil
               (eq 'ignored (libgit-status-file repo file))
             (error t))
         (unless noerror
           (signal 'magit-outside-git-repo default-directory)))))

(cl-defmethod magit-anything-staged-p (&context ((magit-gitimpl) (eql libgit))
                                       &optional ignore-submodules
                                       &rest files)
  (magit--libgit-anything-p 'index-only ignore-submodules files))

(cl-defmethod magit-anything-unstaged-p (&context ((magit-gitimpl) (eql libgit))
                                       &optional ignore-submodules
                                       &rest files)
  ;; FIXME: current libgit uses a default value of flags whenever
  ;; flags are not passed. That default value unfortunately includes
  ;; 'include-ignored and 'include-untracked, so we end up with a
  ;; bunch unrelated paths.
  ;;
  ;; Solution: Remove "default" flags from libgit: it is always
  ;; possible to explicitly add flags that we consider default, but
  ;; there is no way to remove flags if we don't have any.
  (magit--libgit-anything-p 'workdir-only ignore-submodules files))

(cl-defmethod magit-anything-modified-p (&context ((magit-gitimpl) (eql libgit))
                                       &optional ignore-submodules
                                       &rest files)
  (magit--libgit-anything-p 'index-and-workdir ignore-submodules files))


(defun magit--libgit-anything-p (show ignore-submodules files)
  (and (magit--assert-default-directory)
       (if-let ((repo (magit-libgit-repo)))
           (let (found flags)
             (when ignore-submodules (push 'exclude-submodules flags))
             (when files (push 'disable-pathspec-match flags))
             (libgit-status-foreach repo
                                    (lambda (file status) (setq found t))
                                    show flags files)
             found)
         (signal 'magit-outside-git-repo default-directory))))

;;; _
(provide 'magit-libgit)
;;; magit-libgit.el ends here
