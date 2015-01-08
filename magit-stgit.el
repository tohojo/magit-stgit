;;; magit-stgit.el --- StGit extension for Magit

;; Copyright (C) 2011-2015  The Magit Project Developers

;; Author: Lluís Vilanova <vilanova@ac.upc.edu>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>
;; Keywords: vc tools
;; Package: magit-stgit
;; Package-Requires: ((cl-lib "0.5") (magit "2.1.0"))

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

;; This package provides very basic support for StGit.
;;
;;   StGit (Stacked Git) is an application that aims to provide a
;;   convenient way to maintain a patch stack on top of a Git branch.
;;
;; For information about StGit see http://www.procode.org/stgit.
;;
;; If you are looking for full fledged StGit support in Emacs, then
;; have a look at `stgit.el' which is distributed with StGit.

;; When `magit-stgit-mode' is turned on then the current patch series
;; is displayed in the status buffer.  While point is on a patch the
;; changes it introduces can be shown using `RET', it can be selected
;; as the current patch using `a', and it can be discarded using `k'.
;; Other StGit commands are available from the StGit popup on `Y'.

;; To enable the mode in a particular repository use:
;;
;;   cd /path/to/repository
;;   git config --add magit.extension stgit
;;
;; To enable the mode for all repositories use:
;;
;;   git config --global --add magit.extension stgit
;;
;; To enable the mode globally without dropping to a shell:
;;
;;   (add-hook 'magit-mode-hook 'magit-stgit-mode)

;;; Code:

(require 'cl-lib)

(require 'magit)

;;; Options
;;;; Variables

(defgroup magit-stgit nil
  "StGit support for Magit."
  :group 'magit-extensions)

(defcustom magit-stgit-executable "stg"
  "The name of the StGit executable."
  :group 'magit-stgit
  :type 'string)

(defcustom magit-stgit-show-patch-name t
  "Whether to prefix patch messages with the patch name, in patch series."
  :group 'magit-stgit
  :type 'boolean)

(defcustom magit-stgit-mode-lighter " Stg"
  "Mode-line lighter for Magit-Stgit mode."
  :group 'magit-stgit
  :type 'string)

(defcustom magit-stgit-refresh-stage-only t
  "Whether a patch is refreshed only with staged changes (or the worktree otherwise)."
  :group 'magit-stgit
  :type 'boolean)

(defcustom magit-stgit-refresh-ask-to-stage magit-commit-ask-to-stage
  "Whether to ask to stage everything when refreshing a patch and nothing is staged."
  :group 'magit-stgit
  :type 'boolean)

;;;; Faces

(defgroup magit-stgit-faces nil
  "Faces used by Magit-StGit."
  :group 'magit-stgit
  :group 'magit-faces)

(defface magit-stgit-patch
  '((t :inherit magit-hash))
  "Face for name of a stgit patch."
  :group 'magit-stgit-faces)

(add-to-list 'magit-ref-namespaces
             '("^refs/patches/\\(.+\\)" magit-stgit-patch nil))

(defface magit-stgit-current
  '((t :inherit magit-hash))
  "Face for the current stgit patch."
  :group 'magit-stgit-faces)

(defface magit-stgit-applied
  '((t :inherit magit-cherry-equivalent))
  "Face for an applied stgit patch."
  :group 'magit-stgit-faces)

(defface magit-stgit-unapplied
  '((t :inherit magit-cherry-unmatched))
  "Face for an unapplied stgit patch."
  :group 'magit-stgit-faces)

(defface magit-stgit-empty
  '((t :inherit magit-diff-removed))
  "Face for an empty stgit patch."
  :group 'magit-stgit-faces)

(defface magit-stgit-hidden
  '((t :inherit magit-diff-context))
  "Face for an hidden stgit patch."
  :group 'magit-stgit-faces)

;;; Utilities

(defun magit-run-stgit (&rest args)
  (apply #'magit-call-process magit-stgit-executable args)
  (magit-refresh))

(defun magit-stgit-lines (&rest args)
  (with-temp-buffer
    (apply 'process-file magit-stgit-executable nil (list t nil) nil args)
    (split-string (buffer-string) "\n" 'omit-nulls)))

(defvar magit-stgit-read-patch-history nil)

(defun magit-stgit-read-patch (prompt &optional require-match)
  (magit-completing-read prompt (magit-stgit-lines "series" "--noprefix")
                         nil require-match
                         nil 'magit-stgit-read-patch-history))

(defun magit-stgit-read-args (prompt)
  (list (or (magit-section-when stgit-patch)
            (magit-stgit-read-patch prompt t))))

;;; Commands

(magit-define-popup magit-stgit-popup
  "Popup console for StGit commands."
  'magit-popups
  :actions '((?i  "Init"    magit-stgit-init)
             (?f  "Float"   magit-stgit-float)
             (?s  "Sink"    magit-stgit-sink)
             (?\r "Show"    magit-stgit-show)
             (?a  "Goto"    magit-stgit-goto)
             (?k  "Discard" magit-stgit-discard)
             (?g  "Refresh" magit-stgit-refresh)
             (?r  "Repair"  magit-stgit-repair)
             (?R  "Rebase"  magit-stgit-rebase)
             (?z  "Undo"    magit-stgit-undo)
             (?Z  "Redo"    magit-stgit-redo)))

;;;###autoload
(defun magit-stgit-init ()
  "Initialize StGit support for the current branch."
  (interactive)
  (magit-run-stgit "init"))

;;;###autoload
(defun magit-stgit-float (patch)
  "Float StGit PATCH to the top."
  (interactive
   (list (or (magit-stgit-read-patch "Float patch")
             (user-error "No patch selected"))))
  (magit-run-stgit "float" patch))

;;;###autoload
(defun magit-stgit-sink (patch &optional target)
  "Sink StGit PATCH deeper down the stack.
If TARGET is not specified, sink PATCH to the bottom of the stack."
  (interactive
   (list (or (magit-stgit-read-patch "Sink patch")
             (user-error "No patch selected"))
         (magit-stgit-read-patch "Target patch")))
  (let ((args (list patch)))
    (when target
      (add-to-list 'args "-t" t)
      (add-to-list 'args target t))
    (apply #'magit-run-stgit "sink" args)))

;;;###autoload
(defun magit-stgit-refresh (&optional patch)
  "Refresh a StGit patch."
  (interactive
   (list (magit-stgit-read-patch "Refresh patch (default top)")))
  (let ((args '("refresh")))
    (when magit-stgit-refresh-stage-only
      (add-to-list 'args "-i" t)
      (unless (magit-anything-staged-p)
        (if magit-stgit-refresh-ask-to-stage
            (if (y-or-n-p "Nothing staged.  Stage and refresh everything? ")
                (magit-run-git "add" "-u" ".")
              (user-error "Nothing staged"))
          (user-error "Nothing staged"))))
    (when patch
      (add-to-list 'args "-p" t)
      (add-to-list 'args patch t))
    (apply #'magit-run-stgit args)))

;;;###autoload
(defun magit-stgit-repair ()
  "Repair StGit metadata if branch was modified with git commands.
In the case of Git commits these will be imported as new patches
into the series."
  (interactive)
  (message "Repairing series...")
  (magit-run-stgit "repair")
  (message "Repairing series...done"))

;;;###autoload
(defun magit-stgit-rebase ()
  "Rebase a StGit patch series."
  (interactive)
  (let ((remote (magit-get-current-remote))
        (branch (magit-get-current-branch)))
    (if (not (and remote branch))
        (user-error "Branch has no upstream")
      (when (y-or-n-p "Update remote first? ")
        (message "Updating remote...")
        (magit-run-git-async "remote" "update" remote)
        (message "Updating remote...done"))
      (magit-run-stgit "rebase" (format "remotes/%s/%s" remote branch)))))

;;;###autoload
(defun magit-stgit-discard (patch)
  "Discard a StGit patch."
  (interactive (magit-stgit-read-args "Discard patch"))
  (when (yes-or-no-p (format "Discard patch `%s'? " patch))
    (magit-run-stgit "delete" patch)))

;;;###autoload
(defun magit-stgit-goto (patch)
  "Set PATCH as target of StGit push and pop operations."
  (interactive (magit-stgit-read-args "Goto patch"))
  (magit-run-stgit "goto" patch))

;;;###autoload
(defun magit-stgit-show (patch)
  "Show diff of a StGit patch."
  (interactive (magit-stgit-read-args "Show patch"))
  (magit-show-commit (magit-stgit-lines "id" patch)))

;;;###autoload
(defun magit-stgit-undo ()
  "Undo the last operation."
  (interactive)
  (magit-run-stgit "undo"))

;;;###autoload
(defun magit-stgit-redo ()
  "Undo the last undo operation."
  (interactive)
  (magit-run-stgit "redo"))

;;; Mode

(defvar magit-stgit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "Y" 'magit-stgit-popup)
    map))

;;;###autoload
(define-minor-mode magit-stgit-mode
  "StGit support for Magit."
  :lighter magit-stgit-mode-lighter
  :keymap  magit-stgit-mode-map
  (unless (derived-mode-p 'magit-mode)
    (user-error "This mode only makes sense with Magit"))
  (if magit-stgit-mode
      (magit-add-section-hook 'magit-status-sections-hook
                              'magit-insert-stgit-series
                              'magit-insert-stashes t t)
    (remove-hook 'magit-status-sections-hook
                 'magit-insert-stgit-series t))
  (when (called-interactively-p 'any)
    (magit-refresh)))

;;;###autoload
(custom-add-option 'magit-mode-hook #'magit-stgit-mode)

(easy-menu-define magit-stgit-mode-menu nil "Magit-Stgit mode menu"
  '("StGit" :visible magit-stgit-mode
    ["Initialize" magit-stgit-init
     :help "Initialize StGit support for the current branch"]
    "---"
    ["Float patch" magit-stgit-float
     :help "Float StGit patch to the top"]
    ["Sink patch" magit-stgit-sink
     :help "Sink StGit patch deeper down the stack"]
    "---"
    ["Refresh patch" magit-stgit-refresh
     :help "Refresh the contents of a patch in an StGit series"]
    ["Repair" magit-stgit-repair
     :help "Repair StGit metadata if branch was modified with git commands"]
    ["Rebase series" magit-stgit-rebase
     :help "Rebase an StGit patch series"]
    "---"
    ["Undo the last operation" magit-stgit-undo
     :help "Undo the last operation"]
    ["Undo the last undo operation" magit-stgit-redo
     :help "Undo the last undo operation"]))

(easy-menu-add-item 'magit-mode-menu '("Extensions") magit-stgit-mode-menu)

;;; Sections

(defconst magit-stgit-patch-re
  "^\\(.\\)\\([-+>!]\\) \\([^ ]+\\) +# \\(.*\\)$")

(defvar magit-stgit-patch-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "k"  'magit-stgit-discard)
    (define-key map "a"  'magit-stgit-goto)
    (define-key map "\r" 'magit-stgit-show)
    map))

(defun magit-insert-stgit-series ()
  (when magit-stgit-mode
    (magit-insert-section (stgit-series)
      (magit-insert-heading "Patch series:")
      (let ((beg (point)))
        (process-file magit-stgit-executable nil (list t nil) nil
                      "series" "--all" "--empty" "--description")
        (if (= (point) beg)
            (magit-cancel-section)
          (save-restriction
            (narrow-to-region beg (point))
            (goto-char beg)
            (magit-wash-sequence #'magit-stgit-wash-patch)))
        (insert ?\n)))))

(defun magit-stgit-wash-patch ()
  (when (looking-at magit-stgit-patch-re)
    (magit-bind-match-strings (empty state patch msg) nil
      (delete-region (point) (point-at-eol))
      (magit-insert-section (stgit-patch patch)
        (magit-insert state (cond ((equal state ">") 'magit-stgit-current)
                                  ((equal state "+") 'magit-stgit-applied)
                                  ((equal state "-") 'magit-stgit-unapplied)
                                  ((equal state "!") 'magit-stgit-hidden)
                                  (t (user-error "Unknown stgit patch state: %s"
                                                 state))))
        (magit-insert empty 'magit-stgit-empty ?\s)
        (when magit-stgit-show-patch-name
          (magit-insert patch 'magit-stgit-patch ?\s))
        (insert msg)
        (put-text-property (line-beginning-position) (1+ (line-end-position))
                           'keymap 'magit-stgit-patch-map)
        (forward-line)))))

;;; magit-stgit.el ends soon

(define-obsolete-function-alias 'turn-on-magit-stgit 'magit-stgit-mode)

(provide 'magit-stgit)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-stgit.el ends here
