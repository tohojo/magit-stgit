;;; magit-stgit.el --- StGit extension for Magit

;; Copyright (C) 2011-2019  The Magit Project Contributors

;; Author: Lluís Vilanova <vilanova@ac.upc.edu>
;; Maintainer: Lluís Vilanova <vilanova@ac.upc.edu>
;; Keywords: vc tools
;; Package: magit-stgit
;; Package-Requires: ((emacs "24.4") (magit "3.0.0") (transient "0.3.6"))

;; Magit-StGit is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Magit-StGit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit-StGit.  If not, see http://www.gnu.org/licenses.

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
;; Other StGit commands are available from the StGit popup on `/'.

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
(require 'dash)

(require 'magit)
(require 'transient)

;;; Options
;;;; Variables

(defgroup magit-stgit nil
  "StGit support for Magit."
  :group 'magit-extensions)

(defgroup magit-stgit-commands nil
  "Options controlling behavior of certain commands."
  :group 'magit-stgit)


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
             (cons "^refs/patches/\\(.+\\)" 'magit-stgit-patch))

(defface magit-stgit-current
  '((((background dark)) (:weight bold :foreground "yellow"))
    (((background light)) (:weight bold :foreground "purple"))
    (t (:weight bold)))
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
  "Run StGit command with given arguments.
Any list in ARGS is flattened."
  (magit-run-stgit-callback (lambda ()) args))

(defun magit-run-stgit-async (&rest args)
  "Asynchronously run StGit command with given arguments.
Any list in ARGS is flattened."
  (with-editor "GIT_EDITOR"
    (let ((magit-process-popup-time -1))
      (message "Running %s %s" magit-stgit-executable
               (mapconcat 'identity (-flatten args) " "))
      (apply #'magit-start-process magit-stgit-executable nil (-flatten args)))))

(defun magit-run-stgit-and-mark-remove (patches &rest args)
  "Run `magit-run-stgit' and `magit-stgit-mark-remove'.
Argument PATCHES sets the marks to remove, and ARGS the arguments to StGit."
  (magit-run-stgit-callback (lambda () (magit-stgit-mark-remove patches)) args))

(defun magit-run-stgit-callback (callback &rest args)
  "Run StGit command with given arguments.
Function CALLBACK will be executed before refreshing the buffer.
Any list in ARGS is flattened."
  (apply #'magit-call-process magit-stgit-executable (-flatten args))
  (funcall callback)
  (magit-refresh))

(defun magit-stgit-lines (&rest args)
  (with-temp-buffer
    (apply 'process-file magit-stgit-executable nil (list t nil) nil args)
    (split-string (buffer-string) "\n" 'omit-nulls)))

(defvar magit-stgit-read-patch-history nil)

(defun magit-stgit-read-patch (prompt &optional initial-input history require-match)
  (magit-completing-read prompt (magit-stgit-lines "series" "--noprefix")
                         nil require-match
                         initial-input (or history 'magit-stgit-read-patch-history)))

(defun magit-stgit-patches-sorted (patches)
  "Return elements in PATCHES with the same partial order as the series."
  (let ((original (magit-stgit-lines "series" "--noprefix"))
        sorted)
    (mapc (lambda (patch)
            (when (member patch patches)
              (add-to-list 'sorted patch t)))
          original)
    sorted))

(defun magit-stgit-read-patches (use-region use-marks use-point require-match prompt)
  "Return list of selected patches.
If USE-REGION and there is an active region, return marked
patches in it (if USE-MARKS), or all patches in the region if
USE-MARKS is not set or none is marked.
Else, if USE-MARKS and some patches are marked, return these.
Else, if USE-POINT, return the patch at point.
Else, if PROMPT, ask the user for the name of a patch using
PROMPT."
  (let* ((region (and use-region (magit-region-values 'stgit-patch)))
         (intersection (cl-intersection region magit-stgit-marked-patches
                                        :test #'equal)))
    (or (and use-marks
             intersection)
        region
        (and use-marks
             (magit-stgit-patches-sorted magit-stgit-marked-patches))
        (list (or (and use-point (magit-section-when stgit-patch))
                  (and prompt (magit-stgit-read-patch prompt nil nil require-match)))))))

;;; Marking

(defvar-local magit-stgit-marked-patches nil
  "Internal list of marked patches.")

(defun magit-stgit-mark-contains (patch)
  "Whether the given PATCH is marked."
  (member patch magit-stgit-marked-patches))

(defun magit-stgit-mark-add (patches)
  "Set mark of patches.
See `magit-stgit-mark-toggle' for the meaning of PATCHES."
  (interactive (list (magit-stgit-read-patches t nil t t "Patch name")))
  (mapc (lambda (patch)
          (add-to-list 'magit-stgit-marked-patches patch))
        patches)
  (when (called-interactively-p 'any)
    (forward-line)
    (magit-refresh)))

(defun magit-stgit-mark-remove (patches)
  "Unset mark of patches.
See `magit-stgit-mark-toggle' for the meaning of PATCHES."
  (interactive (list (magit-stgit-read-patches t nil t t "Patch name")))
  (mapc (lambda (patch)
          (setq magit-stgit-marked-patches (delete patch magit-stgit-marked-patches)))
        patches)
  (when (called-interactively-p 'any)
    (forward-line)
    (magit-refresh)))

(defun magit-stgit-mark-toggle (patches)
  "Toggle mark of patches.
If given, PATCHES specifies the patch names.
Else, if there is an active region, toggles these.
Else, if point is in an StGit section, toggles the patch at point.
Else, asks the user for a patch name."
  (interactive (list (magit-stgit-read-patches t nil t t "Patch name")))
  (mapc (lambda (patch)
          (if (magit-stgit-mark-contains patch)
              (magit-stgit-mark-remove (list patch))
            (magit-stgit-mark-add (list patch))))
        patches)
  (when (called-interactively-p 'any)
    (forward-line)
    (magit-refresh)))

;;; Commands

(transient-define-prefix magit-stgit-dispatch ()
  "Dispatch a magit-stgit command"

  [["Edit"
    ("N" "New" magit-stgit-new-popup)
    ("n" "Rename" magit-stgit-rename)
    ("e" "Edit" magit-stgit-edit-popup)
    ("g" "Refresh" magit-stgit-refresh-popup)]

   ["Move"
    ("f" "Float"    magit-stgit-float-popup)
    ("s" "Sink"     magit-stgit-sink-popup)
    ("a" "Goto"     magit-stgit-goto-popup)
    ("k" "Delete" magit-stgit-delete-popup)]

   ["Repository"
    ("i" "Init" magit-stgit-init)
    ("r" "Repair" magit-stgit-repair)
    ("R" "Rebase" magit-stgit-rebase-popup)
    ("c" "Commit"   magit-stgit-commit-popup)
    ("C" "Uncommit" magit-stgit-uncommit-popup)]

   ["Other"
    ("m" "Mail patches" magit-stgit-mail-popup)
    ("z" "Undo"     magit-stgit-undo-popup)
    ("Z" "Redo"     magit-stgit-redo-popup)]])

;;;###autoload
(defun magit-stgit-init ()
  "Initialize StGit support for the current branch."
  (interactive)
  (magit-run-stgit "init"))

(defvar magit-stgit-new-filename-regexp ".stgit-\\(new\\|edit\\).txt")

(defun magit-stgit-new-check-buffer ()
  "Check if buffer is an StGit commit message."
  ;; TODO: must remove the stray file on cancel
  (and buffer-file-name
       (string-match-p magit-stgit-new-filename-regexp buffer-file-name)
       (git-commit-setup)))

(defun magit-stgit-read-patch-name (prompt initial-input history)
  (magit-completing-read
   prompt
   nil nil nil initial-input history))

(transient-define-prefix magit-stgit-new-popup ()
  "Popup console for StGit new."
  :man-page "stg-new"
  ["Arguments"
   ("-a" "Add \"Acked-by:\" line" "--ack")
   ("-s" "Add \"Signed-off-by:\" line" "--signoff")
   ("-n" "Set patch name" ("-n" "--name=")
    magit-stgit-read-patch-name)]
  ["Create"
   ("N"  "New"  magit-stgit-new)])

;;;###autoload
(defun magit-stgit-new (&rest args)
  "Create a new StGit patch.
Use ARGS to pass additional arguments."
  (interactive (transient-args 'magit-stgit-new-popup))
  (magit-run-stgit-async "new" args))

(transient-define-prefix magit-stgit-edit-popup ()
  "Popup console for StGit new."
  :man-page "stg-edit"
  ["Arguments"
   ("-a" "Add \"Acked-by:\" line" "--ack")
   ("-s" "Add \"Signed-off-by:\" line" "--signoff")]
  ["Edit"
   ("e"  "Edit"  magit-stgit-edit)])

;;;###autoload
(defun magit-stgit-edit (patch &rest args)
  "Edit the description of an existing StGit PATCH.
Use ARGS to pass additional arguments."
  (interactive (list (magit-stgit-read-patches nil nil t nil "Edit patch (default is top)")
                     (transient-args 'magit-stgit-edit-popup)))
  (magit-run-stgit-async "edit" "--edit" args "--" patch))

(transient-define-prefix magit-stgit-float-popup ()
  "Popup console for StGit float."
  :man-page "stg-float"
  ["Arguments"
   ("-k" "Keep the local changes" "--keep")]
  ["Float"
   ("f"  "Float"  magit-stgit-float)])

;;;###autoload
(defun magit-stgit-float (patches &rest args)
  "Float StGit PATCHES to the top.
Use ARGS to pass additional arguments."
  (interactive (list (magit-stgit-read-patches t t t t "Float patch")
                     (transient-args 'magit-stgit-float-popup)))
  (magit-run-stgit-and-mark-remove patches "float" args "--" patches))

;;;###autoload
(defun magit-stgit-rename (oldname newname)
  "Rename StGit patch OLDNAME to NEWNAME."
  (interactive
   (list (magit-stgit-read-patch "Patch to rename" nil nil t)
         (read-from-minibuffer "New name: ")))
  (magit-run-stgit "rename" oldname newname))

(transient-define-prefix magit-stgit-sink-popup ()
  "Popup console for StGit sink."
  :man-page "stg-sink"
  ["Arguments"
   ("-k" "Keep the local changes" "--keep")
   ("-t" "Sink patches below the target patch (else to bottom)"
    "--to=" magit-stgit-read-patch)]
  ["Sink"
   ("s"  "Sink"  magit-stgit-sink)])

;;;###autoload
(defun magit-stgit-sink (patches &rest args)
  "Sink StGit PATCHES deeper down the stack.
Use ARGS to pass additional arguments."
  (interactive (list (magit-stgit-read-patches t t t t "Sink patch")
                     (transient-args 'magit-stgit-sink-popup)))
  (when (and (called-interactively-p 'any)
             (not magit-current-popup))
    (let ((target (magit-stgit-read-patch "Target patch (default is bottom)")))
      (when target
        (add-to-list 'args "-t" t)
        (add-to-list 'args target t))))
  (magit-run-stgit-and-mark-remove patches "sink" args "--" patches))

(transient-define-prefix magit-stgit-commit-popup ()
  "Popup console for StGit commit."
  :man-page "stg-commit"
  ["Arguments"
   ("-s" "Commit all applied patches" "--all")
   ("-n" "Commit the specified number of patches" "--number=" transient-read-number-N+)]
  ["Commit"
   ("c"  "Commit"  magit-stgit-commit)])

;;;###autoload
(defun magit-stgit-commit (patches &rest args)
  "Permanently store patches into the stack base."
  (interactive (list (magit-stgit-read-patches t t t t nil)
                     (transient-args 'magit-stgit-commit-popup)))
  (when (and (member "--all" (car args))
             (= 1 (length patches)))
    (setq patches (list nil)))
  (magit-run-stgit-and-mark-remove patches "commit" args "--" patches))

(transient-define-prefix magit-stgit-uncommit-popup ()
  "Popup console for StGit commit."
  :man-page "stg-uncommit"
  ["Arguments"
   ("-n" "Commit the specified number of patches" "--number=" transient-read-number-N+)]
  ["Uncommit"
   ("C"  "Uncommit"  magit-stgit-uncommit)])

;;;###autoload
(defun magit-stgit-uncommit (&rest args)
  "Turn regular commits into StGit patches."
  (interactive (-flatten (list (transient-args 'magit-stgit-uncommit-popup))))
  (magit-run-stgit "uncommit" args))

(transient-define-prefix magit-stgit-refresh-popup ()
  "Popup console for StGit refresh."
  :man-page "stg-refresh"
  ["Arguments"
   ("-u" "Only update the current patch files"    "--update")
   ("-i" "Refresh from index instead of worktree" "--index")
   ("-F" "Force refresh even if index is dirty"   "--force")
   ("-e" "Edit the patch description"             "--edit")
   ("-s" "Add \"Signed-off-by:\" line"            "--sign")
   ("-a" "Add \"Acked-by:\" line"                 "--ack")]
  ["Refresh"
   ("g"  "Refresh"  magit-stgit-refresh)])

;;;###autoload
(defun magit-stgit-refresh (&optional patch &rest args)
  "Refresh StGit patch PATCH.
Use ARGS to pass additional arguments."
  (interactive (list (magit-stgit-read-patches nil nil t nil "Refresh patch (default top)")
                     (transient-args 'magit-stgit-refresh-popup)))
  (setq patch (nth 0 patch))
  (when patch
    (add-to-list 'args (format "--patch=%s" patch) t))
  (magit-run-stgit-async "refresh" args))

;;;###autoload
(defun magit-stgit-repair ()
  "Repair StGit metadata if branch was modified with git commands.
In the case of Git commits these will be imported as new patches
into the series."
  (interactive)
  (message "Repairing series...")
  (magit-run-stgit "repair")
  (message "Repairing series...done"))

(transient-define-prefix magit-stgit-rebase-popup ()
  "Popup console for StGit rebase."
  :man-page "stg-rebase"
  ["Arguments"
   ("-n" "Do not push the patches back after rebasing" "--nopush")
   ("-m" "Check for patches merged upstream"           "--merged")]
  ["Rebase"
   ("R"  "On current remote"  magit-stgit-rebase-remote)
   ("o"  "On other commit"  magit-stgit-rebase-other)])


;;;###autoload
(defun magit-stgit-rebase-other (target &rest args)
  "Rebase a StGit patch series.
Use ARGS to pass additional arguments"
  (interactive (list
                (magit-transient-read-revision "Rebase target" nil nil)
                (transient-args 'magit-stgit-rebase-popup)))
  (magit-run-stgit "rebase" args "--" target))

(defun magit-stgit-rebase-remote (&rest args)
  "Rebase a StGit patch series.
Use ARGS to pass additional arguments"
  (interactive (transient-args 'magit-stgit-rebase-popup))
  (let* ((branch (magit-get-current-branch))
         (remote (magit-get-remote branch)))
    (if (not (and remote branch))
        (user-error "Branch has no upstream")
      (when (y-or-n-p "Update remote first? ")
        (message "Updating remote...")
        (magit-run-git-async "remote" "update" remote)
        (message "Updating remote...done"))
      (magit-stgit-rebase-other (format "remotes/%s/%s" remote branch) args))))

(transient-define-prefix magit-stgit-delete-popup ()
  "Popup console for StGit delete."
  :man-page "stg-delete"
  ["Arguments"
   ("-s" "Spill patch contents to worktree and index" "--spill")]
  ["Delete"
   ("k"  "Delete"  magit-stgit-delete)])

;;;###autoload
(defun magit-stgit-delete (patches &rest args)
  "Delete StGit patches.
Argument PATCHES is a list of patchnames.
Use ARGS to pass additional arguments."
  (interactive (list (magit-stgit-read-patches t t t t "Delete patch")
                     (transient-args 'magit-stgit-delete-popup)))
  (let ((affected-files
         (-mapcat (lambda (patch)
                    (magit-stgit-lines "files" "--bare" patch))
                  patches)))
    (when (and (called-interactively-p 'any)
               (not magit-current-popup)
               (and affected-files (y-or-n-p "Spill contents? ")))
      (add-to-list 'args "--spill")))
  (let ((spill (member "--spill" args)))
    (when spill
      (setq spill (list "--spill")))
    (when (or (not (called-interactively-p 'any))
              (yes-or-no-p (format "Delete%s patch%s %s? "
                                   (if spill " and spill" "")
                                   (if (> (length patches) 1) "es" "")
                                   (mapconcat (lambda (patch) (format "`%s'" patch)) patches ", "))))
      (magit-run-stgit-and-mark-remove patches "delete" args "--" patches))))

(transient-define-prefix magit-stgit-goto-popup ()
  "Popup console for StGit goto."
  :man-page "stg-goto"
  ["Arguments"
   ("-k" "Keep the local changes"            "--keep")
   ("-m" "Check for patches merged upstream" "--merged")]
  ["Goto"
   ("a"  "Goto"  magit-stgit-goto)])

;;;###autoload
(defun magit-stgit-goto (patch &rest args)
  "Set PATCH as target of StGit push and pop operations.
Use ARGS to pass additional arguments."
  (interactive (list (magit-stgit-read-patches nil nil t t "Goto patch")
                     (transient-args 'magit-stgit-goto-popup)))
  (magit-run-stgit "goto" patch args))

;;;###autoload
(defun magit-stgit-show (patch)
  "Show diff of a StGit patch."
  (interactive (magit-stgit-read-patches nil nil t t "Show patch"))
  (magit-show-commit (car (magit-stgit-lines "id" patch))))

(transient-define-prefix magit-stgit-undo-popup ()
  "Popup console for StGit undo."
  :man-page "stg-undo"
  ["Arguments"
   ("-n" "Undo the last N commands" "--number=" read-number)
   ("-h" "Discard changes in index/worktree" "--hard")]
  ["Undo"
   ("z"  "Undo"  magit-stgit-undo)])

;;;###autoload
(defun magit-stgit-undo (&rest args)
  "Undo the last operation.
Use ARGS to pass additional arguments."
  (interactive (transient-args magit-stgit-undo-popup))
  (magit-run-stgit "undo" args))

(transient-define-prefix magit-stgit-redo-popup ()
  "Popup console for StGit redo."
  :man-page "stg-redo"
  ["Arguments"
   ("-n" "Undo the last N commands" "--number=" read-number)
   ("-h" "Discard changes in index/worktree" "--hard")]
  ["Redo"
   ("Z"  "Redo"  magit-stgit-redo)])

;;;###autoload
(defun magit-stgit-redo (&rest args)
  "Undo the last undo operation.
Use ARGS to pass additional arguments."
  (interactive (transient-args 'magit-stgit-redo-popup))
  (magit-run-stgit "redo" args))

;;;; magit-stgit-mail

(transient-define-prefix magit-stgit-mail-popup ()
  "Popup console for StGit mail."
  :man-page "stg-mail"
  ["Arguments"
   ("-m" "Generate an mbox file instead of sending" "--mbox")
   ("-g" "Use git send-email" "--git")
   ("-e" "Edit cover letter before send" "--edit-cover")
   ("-a" "Auto-detect recipients for each patch" "--auto")
   ("-A" "Auto-detect To, Cc and Bcc for all patches from cover" "--auto-recipients")
   ("-o" "Set file as cover message" "--cover=" transient-read-file)
   ("-v" "Add version to [PATCH ...]" "--version=")
   ("-p" "Add prefix to [... PATCH ...]" "--prefix=")
   ("-t" "Mail To" "--to=")
   ("-c" "Mail Cc" "--cc=")
   ("-b" "Mail Bcc:" "--bcc=")]
  ["Send"
   ("m" "Send" magit-stgit-mail)])

;;;###autoload
(defun magit-stgit-mail (patches &rest args)
  "Send PATCHES with \"stg mail\".

If a cover is specified, it will be searched to automatically set
the To, Cc, and Bcc fields for all patches."
  (interactive (list (magit-stgit-read-patches t t t t "Send patch")
                     (transient-args 'magit-stgit-mail-popup)))
  (setq args (-flatten args))           ; nested list when called from popup
  (let* ((auto "--auto-recipients")
         (have-auto (member auto args))
         (cover (car (delq nil (mapcar (lambda (arg)
                                         (if (string-prefix-p "--cover=" arg)
                                             arg nil))
                                       args))))
         (cover-pos -1))
    (when have-auto
      (setq args (delete auto args)))
    (when (and have-auto cover)
      (setq cover (substring cover 8))
      (setq cover (with-temp-buffer (insert-file-contents cover)
                                    (buffer-string)))
      (while (setq cover-pos
                   (string-match
                        "^\\(To\\|Cc\\|Bcc\\):[[:space:]]+\\(.*\\)[[:space:]]*$"
                        cover (1+ cover-pos)))
        (let ((field (match-string 1 cover))
              (recipient (match-string 2 cover)))
          (setq field (match-string 1 cover))
          (when (string-match "<" recipient)
            (setq recipient (format "\"%s\"" recipient)))
          (cond ((equal field "To")
                 (setq args (cons (format "--to=%s" recipient)
                                  args)))
                ((equal field "Cc")
                 (setq args (cons (format "--cc=%s" recipient)
                                  args)))
                ((equal field "Bcc")
                 (setq args (cons (format "--bcc=%s" recipient)
                                  args)))))))
    (magit-run-stgit-async "mail" args patches)))

;;; Mode

(defvar magit-stgit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "S" 'magit-stgit-dispatch)
    (define-key map "/" 'magit-stgit-dispatch)
    map))

;;;###autoload
(define-minor-mode magit-stgit-mode
  "StGit support for Magit."
  :lighter magit-stgit-mode-lighter
  :keymap  magit-stgit-mode-map
  (unless (derived-mode-p 'magit-mode)
    (user-error "This mode only makes sense with Magit"))
  (if magit-stgit-mode
      (progn
        (magit-add-section-hook 'magit-status-sections-hook
                                'magit-insert-stgit-series
                                'magit-insert-stashes t t)
        (add-hook 'find-file-hook #'magit-stgit-new-check-buffer)
        (transient-insert-suffix 'magit-dispatch "t"
            '("S" "Stgit" magit-stgit-dispatch)))
    (remove-hook 'magit-status-sections-hook
                 'magit-insert-stgit-series t)
    (remove-hook 'find-file-hook #'magit-stgit-new-check-buffer)
    (transient-remove-suffix 'magit-dispatch "S"))
  (when (called-interactively-p 'any)
    (magit-refresh)))

;;;###autoload
(custom-add-option 'magit-mode-hook #'magit-stgit-mode)

(easy-menu-define magit-stgit-mode-menu nil "Magit-Stgit mode menu"
  '("StGit" :visible magit-stgit-mode
    ["Initialize" magit-stgit-init
     :help "Initialize StGit support for the current branch"]
    "---"
    ["Create new patch" magit-stgit-new-popup
     :help "Create a new StGit patch"]
    ["Rename patch" magit-stgit-rename
     :help "Rename a patch"]
    ["Edit patch" magit-stgit-edit-popup
     :help "Edit a patch"]
    ["Commit patch" magit-stgit-commit-popup
     :help "Permanently store the base patch into the stack base"]
    ["Uncommit patch" magit-stgit-uncommit-popup
     :help "Turn a regular commit into an StGit patch"]
    ["Delete patch" magit-stgit-delete-popup
     :help "Delete an StGit patch"]
    "---"
    ["Float patch" magit-stgit-float-popup
     :help "Float StGit patch to the top"]
    ["Sink patch" magit-stgit-sink-popup
     :help "Sink StGit patch deeper down the stack"]
    "---"
    ["Refresh patch" magit-stgit-refresh-popup
     :help "Refresh the contents of a patch in an StGit series"]
    ["Repair" magit-stgit-repair
     :help "Repair StGit metadata if branch was modified with git commands"]
    ["Rebase series" magit-stgit-rebase-popup
     :help "Rebase an StGit patch series"]
    "---"
    ["Undo the last operation" magit-stgit-undo-popup
     :help "Undo the last operation"]
    ["Undo the last undo operation" magit-stgit-redo-popup
     :help "Undo the last undo operation"]))

(easy-menu-add-item 'magit-mode-menu '("Extensions") magit-stgit-mode-menu)

;;; Sections

(defconst magit-stgit-patch-re
  "^\\(.\\)\\([-+>!]\\) \\([^ ]+\\) +# \\(.*\\)$")

(defvar magit-stgit-patch-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "k"  'magit-stgit-delete)
    (define-key map "a"  'magit-stgit-goto)
    (define-key map "\r" 'magit-stgit-show)
    (define-key map "#"  #'magit-stgit-mark-toggle)
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
        (insert (if (magit-stgit-mark-contains patch) "#" " "))
        (insert (propertize state 'face
                            (cond ((equal state ">") 'magit-stgit-current)
                                  ((equal state "+") 'magit-stgit-applied)
                                  ((equal state "-") 'magit-stgit-unapplied)
                                  ((equal state "!") 'magit-stgit-hidden)
                                  (t (user-error "Unknown stgit patch state: %s"
                                                 state)))))
        (insert (propertize empty 'face 'magit-stgit-empty) ?\s)
        (when magit-stgit-show-patch-name
          (insert (propertize patch 'face 'magit-stgit-patch) ?\s))
        (insert msg)
        (put-text-property (line-beginning-position) (1+ (line-end-position))
                           'keymap 'magit-stgit-patch-map)
        (forward-line)))))

;;; magit-stgit.el ends soon

(provide 'magit-stgit)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-stgit.el ends here
