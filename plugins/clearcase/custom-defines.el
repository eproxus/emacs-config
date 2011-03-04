;;; DO NOT MODIFY THIS FILE
(if (featurep 'clearcase-custom-defines) (error "Feature clearcase-custom-defines already loaded"))

;;;### (autoloads (clearcase-annotate-fmt-string clearcase-rebase-id-regexp clearcase-hide-rebase-activities clearcase-suppress-vc-within-mvfs clearcase-viewroot-drive clearcase-viewroot clearcase-vxpath-glue clearcase-normal-diff-arguments clearcase-normal-diff-program clearcase-use-normal-diff clearcase-directory-exclusion-list clearcase-checkout-arguments clearcase-suppress-checkout-comments clearcase-checkin-on-mkelem clearcase-checkin-arguments clearcase-command-messages clearcase-initial-mkelem-comment clearcase-suppress-confirm clearcase-diff-on-checkin clearcase-verify-pre-mkelem-dir-checkout clearcase-dired-show-view clearcase-dired-highlight clearcase-auto-dired-mode clearcase-minimise-menus clearcase-complete-viewtags clearcase-make-backup-files clearcase-prompt-for-activity-names clearcase-set-to-new-activity clearcase-remove-branch-after-unheckout-when-only-0-version clearcase-keep-unhijacks clearcase-keep-uncheckouts clearcase) "clearcase" "clearcase/clearcase.el")
(defconst custom-define-current-source-file "clearcase.el")

(defgroup clearcase nil "ClearCase Options" :group 'tools :prefix "clearcase")

(defcustom clearcase-keep-uncheckouts t "\
When true, the contents of an undone checkout will be kept in a file
with a \".keep\" suffix. Otherwise it will be removed." :group 'clearcase :type 'boolean)

(defcustom clearcase-keep-unhijacks t "\
When true, the contents of an undone hijack will be kept in a file
with a \".keep\" suffix. Otherwise it will be removed." :group 'clearcase :type 'boolean)

(defcustom clearcase-remove-branch-after-unheckout-when-only-0-version t "\
When true, after a file has been unchecked out, if the version is .../0, remove the branch." :group 'clearcase :type 'boolean)

(defcustom clearcase-set-to-new-activity t "\
*If this variable is non-nil when a new activity is created, that activity
will be set as the current activity for the view, otherwise no change is made
to the view's current activity setting." :group 'clearcase :type 'boolean)

(defcustom clearcase-prompt-for-activity-names t "\
*If this variable is non-nil the user will be prompted for activity names.
Otherwise, activity names will be generated automatically and will typically
have the form \"activity011112.155233\". If the name entered is empty sucn an
internal name will also be generated." :group 'clearcase :type 'boolean)

(defcustom clearcase-make-backup-files nil "\
*If non-nil, backups of ClearCase files are made as with other files.
If nil (the default), files under ClearCase control don't get backups." :group 'clearcase :type 'boolean)

(defcustom clearcase-complete-viewtags t "\
*If non-nil, completion on viewtags is enabled. For sites with thousands of view
this should be set to nil." :group 'clearcase :type 'boolean)

(defcustom clearcase-minimise-menus nil "\
*If non-nil, menus will hide rather than grey-out inapplicable choices." :group 'clearcase :type 'boolean)

(defcustom clearcase-auto-dired-mode t "\
*If non-nil, automatically enter `clearcase-dired-mode' in dired-mode
for directories in ClearCase." :group 'clearcase :type 'boolean)

(defcustom clearcase-dired-highlight t "\
If non-nil, highlight reserved files in clearcase-dired buffers." :group 'clearcase :type 'boolean)

(defcustom clearcase-dired-show-view t "\
If non-nil, show the view tag in dired buffers." :group 'clearcase :type 'boolean)

(defcustom clearcase-verify-pre-mkelem-dir-checkout nil "\
*If non-nil, prompt before checking out the containing directory
before creating a new ClearCase element." :group 'clearcase :type 'boolean)

(defcustom clearcase-diff-on-checkin nil "\
Display diff on checkin to help you compose the checkin comment." :group 'clearcase :type 'boolean)

(defcustom clearcase-suppress-confirm nil "\
If non-nil, treat user as expert; suppress yes-no prompts on some things." :group 'clearcase :type 'boolean)

(defcustom clearcase-initial-mkelem-comment nil "\
Prompt for initial comment when an element is created." :group 'clearcase :type 'boolean)

(defcustom clearcase-command-messages nil "\
Display run messages from back-end commands." :group 'clearcase :type 'boolean)

(defcustom clearcase-checkin-arguments (if (and (boundp 'clearcase-checkin-switches) (not (null clearcase-checkin-switches))) (list clearcase-checkin-switches) nil) "\
A list of extra arguments passed to the checkin command." :group 'clearcase :type '(repeat (string :tag "Argument")))

(defcustom clearcase-checkin-on-mkelem nil "\
If t, file will be checked-in when first created as an element." :group 'clearcase :type 'boolean)

(defcustom clearcase-suppress-checkout-comments nil "\
Suppress prompts for checkout comments for those version control
systems which use them." :group 'clearcase :type 'boolean)

(defcustom clearcase-checkout-arguments (if (and (boundp 'clearcase-checkout-arguments) (not (null clearcase-checkout-arguments))) (list clearcase-checkout-arguments) nil) "\
A list of extra arguments passed to the checkout command." :group 'clearcase :type '(repeat (string :tag "Argument")))

(defcustom clearcase-directory-exclusion-list '("lost+found") "\
Directory names ignored by functions that recursively walk file trees." :group 'clearcase :type '(repeat (string :tag "Subdirectory")))

(defcustom clearcase-use-normal-diff nil "\
If non-nil, use normal diff instead of cleardiff." :group 'clearcase :type 'boolean)

(defcustom clearcase-normal-diff-program "diff" "\
*Program to use for generating the differential of the two files
when `clearcase-use-normal-diff' is t." :group 'clearcase :type 'string)

(defcustom clearcase-normal-diff-arguments (if (and (boundp 'clearcase-normal-diff-switches) (not (null clearcase-normal-diff-switches))) (list clearcase-normal-diff-switches) (list "-u")) "\
A list of extra arguments passed to `clearcase-normal-diff-program'
when `clearcase-use-normal-diff' is t.  Usage of the -u switch is
recommended to produce unified diffs, when your
`clearcase-normal-diff-program' supports it." :group 'clearcase :type '(repeat (string :tag "Argument")))

(defcustom clearcase-vxpath-glue "@@" "\
The string used to construct version-extended pathnames." :group 'clearcase :type 'string)

(defcustom clearcase-viewroot (if clearcase-on-mswindows "//view" "/view") "\
The ClearCase viewroot directory." :group 'clearcase :type 'file)

(defcustom clearcase-viewroot-drive "m:" "\
The ClearCase viewroot drive letter for Windows." :group 'clearcase :type 'string)

(defcustom clearcase-suppress-vc-within-mvfs t "\
Suppresses VC activity within the MVFS." :group 'clearcase :type 'boolean)

(defcustom clearcase-hide-rebase-activities t "\
Hide rebase activities from activity selection list." :group 'clearcase :type 'boolean)

(defcustom clearcase-rebase-id-regexp "^rebase\\." "\
The regexp used to detect rebase actvities." :group 'clearcase :type 'string)

(defcustom clearcase-annotate-fmt-string "/** %Sd  %-8.8u **/" "\
The -fmt argument passed top cleartool+annotate when it is called." :group 'clearcase :type 'string)

;;;***

(provide 'clearcase-custom-defines)
