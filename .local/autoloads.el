;;; autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "../core/autoload/buffers" "../core/autoload/buffers.el"
;;;;;;  (23288 273 444576 544000))
;;; Generated autoloads from ../core/autoload/buffers.el

(defvar doom-real-buffer-functions 'nil "\
A list of predicate functions run to determine if a buffer is real. These
functions are iterated over with one argument, the buffer in question. If any
function returns non-nil, the procession stops and the buffer is qualified as
real.")

(defvar-local doom-real-buffer-p nil "\
If non-nil, this buffer should be considered real no matter what.")

(defvar doom-fallback-buffer "*scratch*" "\
The name of the buffer to fall back to if no other buffers exist (will create
it if it doesn't exist).")

(autoload 'doom-fallback-buffer "../core/autoload/buffers" "\
Returns the fallback buffer, creating it if necessary. By default this is the
scratch buffer.

\(fn)" nil nil)

(defalias 'doom-buffer-list #'buffer-list)

(autoload 'doom-project-buffer-list "../core/autoload/buffers" "\
Return a list of buffers belonging to the current project.

If no project is active, return all buffers.

\(fn)" nil nil)

(autoload 'doom-real-buffer-list "../core/autoload/buffers" "\
Return a list of buffers that satify `doom-real-buffer-p'.

\(fn &optional BUFFER-LIST)" nil nil)

(autoload 'doom-real-buffer-p "../core/autoload/buffers" "\
Returns t if BUFFER-OR-NAME is a 'real' buffer. The complete criteria for a
real buffer is:

  1. The buffer-local value of `doom-real-buffer-p' (variable) is non-nil OR
  2. Any function in `doom-real-buffer-functions' must return non-nil when
     passed this buffer OR
  3. The current buffer:
     a) has a `buffer-file-name' defined AND
     b) is not in a popup window (see `doom-popup-p') AND
     c) is not a special buffer (its name isn't something like *Help*)

If BUFFER-OR-NAME is omitted or nil, the current buffer is tested.

\(fn &optional BUFFER-OR-NAME)" nil nil)

(autoload 'doom-buffers-in-mode "../core/autoload/buffers" "\
Return a list of buffers whose `major-mode' is `eq' to MODE(S).

If DERIVED-P, test with `derived-mode-p', otherwise use `eq'.

\(fn MODES &optional BUFFER-LIST DERIVED-P)" nil nil)

(autoload 'doom-visible-windows "../core/autoload/buffers" "\
Return a list of the visible, non-popup windows.

\(fn &optional WINDOW-LIST)" nil nil)

(autoload 'doom-visible-buffers "../core/autoload/buffers" "\
Return a list of visible buffers (i.e. not buried).

\(fn &optional BUFFER-LIST)" nil nil)

(autoload 'doom-buried-buffers "../core/autoload/buffers" "\
Get a list of buffers that are buried.

\(fn &optional BUFFER-LIST)" nil nil)

(autoload 'doom-matching-buffers "../core/autoload/buffers" "\
Get a list of all buffers that match the regex PATTERN.

\(fn PATTERN &optional BUFFER-LIST)" nil nil)

(autoload 'doom-set-buffer-real "../core/autoload/buffers" "\
Forcibly mark BUFFER as FLAG (non-nil = real).

\(fn BUFFER FLAG)" nil nil)

(autoload 'doom-kill-buffer "../core/autoload/buffers" "\
Kill BUFFER (defaults to current buffer), but make sure we land on a real
buffer. Bury the buffer if the buffer is present in another window.

Will prompt to save unsaved buffers when attempting to kill them, unless
DONT-SAVE is non-nil.

See `doom-real-buffer-p' for what 'real' means.

\(fn &optional BUFFER DONT-SAVE)" nil nil)

(autoload 'doom-kill-buffer-and-windows "../core/autoload/buffers" "\
Kill the buffer and delete all the windows it's displayed in.

\(fn BUFFER)" nil nil)

(autoload 'doom-kill-matching-buffers "../core/autoload/buffers" "\
Kill all buffers (in current workspace OR in BUFFER-LIST) that match the
regex PATTERN. Returns the number of killed buffers.

\(fn PATTERN &optional BUFFER-LIST)" nil nil)

(autoload 'doom/kill-this-buffer "../core/autoload/buffers" "\
Use `doom-kill-buffer' on the current buffer.

\(fn &optional INTERACTIVE-P)" t nil)

(autoload 'doom/kill-this-buffer-in-all-windows "../core/autoload/buffers" "\
Kill BUFFER globally and ensure all windows previously showing this buffer
have switched to a real buffer.

If DONT-SAVE, don't prompt to save modified buffers (discarding their changes).

\(fn BUFFER &optional DONT-SAVE)" t nil)

(autoload 'doom/kill-all-buffers "../core/autoload/buffers" "\
Kill all buffers and closes their windows.

If PROJECT-P (universal argument), kill only buffers that belong to the current
project.

\(fn &optional PROJECT-P)" t nil)

(autoload 'doom/kill-other-buffers "../core/autoload/buffers" "\
Kill all other buffers (besides the current one).

If PROJECT-P (universal argument), kill only buffers that belong to the current
project.

\(fn &optional PROJECT-P)" t nil)

(autoload 'doom/kill-matching-buffers "../core/autoload/buffers" "\
Kill buffers that match PATTERN in BUFFER-LIST.

If PROJECT-P (universal argument), only kill matching buffers in the current
project.

\(fn PATTERN &optional PROJECT-P)" t nil)

(autoload 'doom/cleanup-session "../core/autoload/buffers" "\
Clean up buried buries and orphaned processes in the current workspace. If
ALL-P (universal argument), clean them up globally.

\(fn &optional ALL-P)" t nil)

(autoload 'doom/cleanup-processes "../core/autoload/buffers" "\
Kill all processes that have no visible associated buffers. Return number of
processes killed.

\(fn)" t nil)

(autoload 'doom/next-buffer "../core/autoload/buffers" "\
Switch to the next real buffer, skipping non-real buffers. See
`doom-real-buffer-p' for what 'real' means.

\(fn)" t nil)

(autoload 'doom/previous-buffer "../core/autoload/buffers" "\
Switch to the previous real buffer, skipping non-real buffers. See
`doom-real-buffer-p' for what 'real' means.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../core/autoload/buffers" '("doom-")))

;;;***

;;;### (autoloads nil "../core/autoload/debug" "../core/autoload/debug.el"
;;;;;;  (23288 273 444918 121000))
;;; Generated autoloads from ../core/autoload/debug.el

(autoload 'doom/what-face "../core/autoload/debug" "\
Shows all faces and overlay faces at point.

Interactively prints the list to the echo area. Noninteractively, returns a list
whose car is the list of faces and cadr is the list of overlay faces.

\(fn &optional POS)" t nil)

(autoload 'doom-active-minor-modes "../core/autoload/debug" "\
Get a list of active minor-mode symbols.

\(fn)" nil nil)

(autoload 'doom/what-minor-mode "../core/autoload/debug" "\
Get information on an active minor mode. Use `describe-minor-mode' for a
selection of all minor-modes, active or not.

\(fn MODE)" t nil)

(autoload 'doom/am-i-secure "../core/autoload/debug" "\
Test to see if your root certificates are securely configured in emacs.

\(fn)" t nil)

(function-put 'doom/am-i-secure 'interactive-only 't)

(autoload 'doom/toggle-profiler "../core/autoload/debug" "\


\(fn)" t nil)

(autoload 'doom/info "../core/autoload/debug" "\
Collects information about this session of Doom Emacs and copies it to the
clipboard. Helpful when filing bug reports!

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../core/autoload/debug" '("doom--profiler")))

;;;***

;;;### (autoloads nil "../core/autoload/editor" "../core/autoload/editor.el"
;;;;;;  (23288 273 445239 826000))
;;; Generated autoloads from ../core/autoload/editor.el

(autoload 'doom/sudo-find-file "../core/autoload/editor" "\
Open FILE as root.

\(fn FILE)" t nil)

(autoload 'doom/sudo-this-file "../core/autoload/editor" "\
Open the current file as root.

\(fn)" t nil)

(autoload 'doom/backward-to-bol-or-indent "../core/autoload/editor" "\
Move back to the current line's indentation. If already there, move to the
beginning of the line instead. If at bol, do nothing.

\(fn)" t nil)

(autoload 'doom/forward-to-last-non-comment-or-eol "../core/autoload/editor" "\
Move forward to the last non-blank character in the line, ignoring comments
and trailing whitespace. If already there, move to the real end of the line.
If already there, do nothing.

\(fn)" t nil)

(autoload 'doom/dumb-indent "../core/autoload/editor" "\
Inserts a tab character (or spaces x tab-width).

\(fn)" t nil)

(autoload 'doom/dumb-dedent "../core/autoload/editor" "\
Dedents the current line.

\(fn)" t nil)

(autoload 'doom/backward-kill-to-bol-and-indent "../core/autoload/editor" "\
Kill line to the first non-blank character. If invoked again
afterwards, kill line to column 1.

\(fn)" t nil)

(autoload 'doom/backward-delete-whitespace-to-column "../core/autoload/editor" "\
Delete back to the previous column of whitespace, or as much whitespace as
possible, or just one char if that's not possible.

\(fn)" t nil)

(autoload 'doom/inflate-space-maybe "../core/autoload/editor" "\
Checks if point is surrounded by {} [] () delimiters and adds a
space on either side of the point if so.

\(fn)" t nil)

(autoload 'doom/deflate-space-maybe "../core/autoload/editor" "\
Checks if point is surrounded by {} [] () delimiters, and deletes
spaces on either side of the point if so. Resorts to
`doom/backward-delete-whitespace-to-column' otherwise.

\(fn)" t nil)

(autoload 'doom/newline-and-indent "../core/autoload/editor" "\
Inserts a newline and possibly indents it. Also continues comments if
executed from a commented line; handling special cases for certain languages
with weak native support.

\(fn)" t nil)

(autoload 'doom/retab "../core/autoload/editor" "\
Changes all tabs to spaces or spaces to tabs, so that indentation is
consistent throughout a selected region, depending on `indent-tab-mode'.

\(fn &optional BEG END)" t nil)

(autoload 'doom/narrow-buffer "../core/autoload/editor" "\
Restrict editing in this buffer to the current region, indirectly. With CLONE-P,
clone the buffer and hard-narrow the selection. If mark isn't active, then widen
the buffer (if narrowed).

Inspired from http://demonastery.org/2013/04/emacs-evil-narrow-region/

\(fn BEG END &optional CLONE-P)" t nil)

(autoload 'doom|enable-delete-trailing-whitespace "../core/autoload/editor" "\
Attaches `delete-trailing-whitespace' to a buffer-local `before-save-hook'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../core/autoload/editor" '("doom--surrounded-p")))

;;;***

;;;### (autoloads nil "../core/autoload/help" "../core/autoload/help.el"
;;;;;;  (23288 273 445514 772000))
;;; Generated autoloads from ../core/autoload/help.el

(autoload 'doom/describe-setting "../core/autoload/help" "\
Open the documentation of SETTING (a keyword defined with `def-setting!').

\(fn SETTING)" t nil)

(autoload 'doom/describe-module "../core/autoload/help" "\
Open the documentation of MODULE (a string that represents the category and
submodule in the format, e.g. ':feature evil').

\(fn MODULE)" t nil)

;;;***

;;;### (autoloads nil "../core/autoload/memoize" "../core/autoload/memoize.el"
;;;;;;  (23288 273 445739 488000))
;;; Generated autoloads from ../core/autoload/memoize.el

(defvar doom-memoized-table (make-hash-table :test 'equal :size 10) "\
A lookup table containing memoized functions. The keys are argument lists,
and the value is the function's return value.")

(autoload 'doom-memoize "../core/autoload/memoize" "\
Memoizes an existing function. NAME is a symbol.

\(fn NAME)" nil nil)

(autoload 'def-memoized! "../core/autoload/memoize" "\
Create a memoize'd function. NAME, ARGLIST, DOCSTRING and BODY
have the same meaning as in `defun'.

\(fn NAME ARGLIST &rest BODY)" nil t)

(function-put 'def-memoized! 'lisp-indent-function 'defun)

(function-put 'def-memoized! 'doc-string-elt '3)

;;;***

;;;### (autoloads nil "../core/autoload/menu" "../core/autoload/menu.el"
;;;;;;  (23288 273 445926 223000))
;;; Generated autoloads from ../core/autoload/menu.el

(autoload 'def-menu! "../core/autoload/menu" "\
Defines a menu and returns a function symbol for invoking it.

A dispatcher is an interactive command named NAME (a symbol). When called, this
dispatcher prompts you to select a command to run. This list is filtered
depending on its properties. Each command is takes the form of:

  (DESCRIPTION :exec COMMAND &rest PROPERTIES)

PROPERTIES accepts the following properties:

  :when FORM
  :unless FORM
  :region BOOL
  :cwd t|PATH
  :project BOOL|DIRECTORY

COMMAND can be a string (a shell command), a symbol (an elisp function) or a
lisp form.

`def-menu!'s PLIST supports the following properties:

  :prompt STRING

\(fn NAME DESC COMMANDS &rest PLIST)" nil t)

(function-put 'def-menu! 'lisp-indent-function 'defun)

(function-put 'def-menu! 'doc-string-elt '2)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../core/autoload/menu" '("doom-")))

;;;***

;;;### (autoloads nil "../core/autoload/message" "../core/autoload/message.el"
;;;;;;  (23288 273 446148 141000))
;;; Generated autoloads from ../core/autoload/message.el

(autoload 'format! "../core/autoload/message" "\
An alternative to `format' that strips out ANSI codes if used in an
interactive session.

\(fn MESSAGE &rest ARGS)" nil t)

(autoload 'message! "../core/autoload/message" "\
An alternative to `message' that strips out ANSI codes if used in an
interactive session.

\(fn MESSAGE &rest ARGS)" nil t)

(autoload 'debug! "../core/autoload/message" "\
Out a debug message if `doom-debug-mode' is non-nil. Otherwise, ignore this.

\(fn MESSAGE &rest ARGS)" nil t)

(autoload 'doom-ansi-apply "../core/autoload/message" "\


\(fn CODE FORMAT &rest ARGS)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../core/autoload/message" '("doom-message-")))

;;;***

;;;### (autoloads nil "../core/autoload/minibuffer" "../core/autoload/minibuffer.el"
;;;;;;  (23288 273 446290 467000))
;;; Generated autoloads from ../core/autoload/minibuffer.el

(autoload 'doom/minibuffer-kill-word "../core/autoload/minibuffer" "\
Kill a word, backwards, but only if the cursor is after
`minibuffer-prompt-end', to prevent the 'Text is read-only' warning from
monopolizing the minibuffer.

\(fn)" t nil)

(autoload 'doom/minibuffer-kill-line "../core/autoload/minibuffer" "\
Kill the entire line, but only if the cursor is after
`minibuffer-prompt-end', to prevent the 'Text is read-only' warning from
monopolizing the minibuffer.

\(fn)" t nil)

(autoload 'doom/minibuffer-undo "../core/autoload/minibuffer" "\
Undo an edit in the minibuffer without throwing errors.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../core/autoload/packages" "../core/autoload/packages.el"
;;;;;;  (23288 273 446697 923000))
;;; Generated autoloads from ../core/autoload/packages.el

(autoload 'doom-refresh-packages "../core/autoload/packages" "\
Refresh ELPA packages.

\(fn &optional FORCE-P)" nil nil)

(autoload 'doom-refresh-clear-cache "../core/autoload/packages" "\
Clear the cache for `doom-refresh-packages'.

\(fn)" nil nil)

(autoload 'doom-package-backend "../core/autoload/packages" "\
Get which backend the package NAME was installed with. Can either be elpa or
quelpa. Throws an error if NOERROR is nil and the package isn't installed.

\(fn NAME &optional NOERROR)" nil nil)

(autoload 'doom-package-outdated-p "../core/autoload/packages" "\
Determine whether NAME (a symbol) is outdated or not. If outdated, returns a
list, whose car is NAME, and cdr the current version list and latest version
list of the package.

\(fn NAME)" nil nil)

(autoload 'doom-package-prop "../core/autoload/packages" "\
Return PROPerty in NAME's plist.

\(fn NAME PROP)" nil nil)

(autoload 'doom-package-different-backend-p "../core/autoload/packages" "\
Return t if NAME (a package's symbol) has a new backend than what it was
installed with. Returns nil otherwise, or if package isn't installed.

\(fn NAME)" nil nil)

(autoload 'doom-get-packages "../core/autoload/packages" "\
Retrieves a list of explicitly installed packages (i.e. non-dependencies).
Each element is a cons cell, whose car is the package symbol and whose cdr is
the quelpa recipe (if any).

BACKEND can be 'quelpa or 'elpa, and will instruct this function to return only
the packages relevant to that backend.

Warning: this function is expensive; it re-evaluates all of doom's config files.
Be careful not to use it in a loop.

If INSTALLED-ONLY-P, only return packages that are installed.

\(fn &optional INSTALLED-ONLY-P)" nil nil)

(autoload 'doom-get-depending-on "../core/autoload/packages" "\
Return a list of packages that depend on the package named NAME.

\(fn NAME)" nil nil)

(autoload 'doom-get-dependencies-for "../core/autoload/packages" "\
Return a list of dependencies for a package.

\(fn NAME &optional ONLY)" nil nil)

(autoload 'doom-get-outdated-packages "../core/autoload/packages" "\
Return a list of packages that are out of date. Each element is a list,
containing (PACKAGE-SYMBOL OLD-VERSION-LIST NEW-VERSION-LIST).

If INCLUDE-FROZEN-P is non-nil, check frozen packages as well.

Used by `doom//packages-update'.

\(fn &optional INCLUDE-FROZEN-P)" nil nil)

(autoload 'doom-get-orphaned-packages "../core/autoload/packages" "\
Return a list of symbols representing packages that are no longer needed or
depended on.

Used by `doom//packages-autoremove'.

\(fn)" nil nil)

(autoload 'doom-get-missing-packages "../core/autoload/packages" "\
Return a list of requested packages that aren't installed or built-in, but
are enabled (with a `package!' directive). Each element is a list whose CAR is
the package symbol, and whose CDR is a plist taken from that package's
`package!' declaration.

If INCLUDE-IGNORED-P is non-nil, includes missing packages that are ignored,
i.e. they have an :ignore property.

Used by `doom//packages-install'.

\(fn &optional INCLUDE-IGNORED-P)" nil nil)

(autoload 'doom*package-delete "../core/autoload/packages" "\
Update `quelpa-cache' upon a successful `package-delete'.

\(fn DESC &rest _)" nil nil)

(autoload 'doom//packages-install "../core/autoload/packages" "\
Interactive command for installing missing packages.

\(fn)" t nil)

(autoload 'doom//packages-update "../core/autoload/packages" "\
Interactive command for updating packages.

\(fn)" t nil)

(autoload 'doom//packages-autoremove "../core/autoload/packages" "\
Interactive command for auto-removing orphaned packages.

\(fn)" t nil)

(defalias 'doom/install-package #'package-install)

(autoload 'doom/reinstall-package "../core/autoload/packages" "\
Reinstalls package package with optional quelpa RECIPE (see `quelpa-recipe' for
an example; the package package can be omitted).

\(fn DESC)" t nil)

(function-put 'doom/reinstall-package 'interactive-only 't)

(autoload 'doom/delete-package "../core/autoload/packages" "\
Prompts the user with a list of packages and deletes the selected package.
Use this interactively. Use `doom-delete-package' for direct calls.

\(fn DESC)" t nil)

(function-put 'doom/delete-package 'interactive-only 't)

(autoload 'doom/update-package "../core/autoload/packages" "\
Prompts the user with a list of outdated packages and updates the selected
package. Use this interactively. Use `doom-update-package' for direct
calls.

\(fn PKG)" t nil)

(function-put 'doom/update-package 'interactive-only 't)

(autoload 'doom/refresh-packages "../core/autoload/packages" "\
Synchronize package metadata with the sources in `package-archives'. If
FORCE-P (the universal argument) is set, ignore the cache.

\(fn &optional FORCE-P)" t nil)

(function-put 'doom/refresh-packages 'interactive-only 't)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../core/autoload/packages" '("doom-")))

;;;***


(function-put 'doom-popup-buffer 'lisp-indent-function 'defun)

(autoload 'doom-popup-switch-to-buffer "../core/autoload/popups" "\
Switch the current (or closest) pop-up window to BUFFER.

\(fn BUFFER)" nil nil)

(autoload 'doom-popup-fit-to-buffer "../core/autoload/popups" "\
Fit WINDOW to the size of its content.

\(fn &optional WINDOW MAX-SIZE)" nil nil)

(autoload 'doom-popup-move "../core/autoload/popups" "\
Move a popup window to another side of the frame, in DIRECTION, which can be
one of the following: 'left 'right 'above 'below

\(fn DIRECTION)" nil nil)

(autoload 'doom-popup-file "../core/autoload/popups" "\
Display FILE in a shackle popup, with PLIST rules. See `shackle-rules' for
possible rules.

\(fn FILE &optional PLIST EXTEND-P)" nil nil)

(autoload 'doom-popup-windows "../core/autoload/popups" "\
Get a list of open pop up windows.

\(fn &optional FILTER-STATIC-P)" nil nil)

(autoload 'doom-popup-properties "../core/autoload/popups" "\
Returns a window's popup property list, if possible. The buffer-local
`doom-popup-rules' always takes priority, but this will fall back to the popup
window parameter.

\(fn WINDOW-OR-BUFFER)" nil nil)

(autoload 'doom-popup-property "../core/autoload/popups" "\
Returns a `doom-popup-rules' PROPerty from WINDOW.

\(fn PROP &optional WINDOW)" nil nil)

(autoload 'doom-popup-side "../core/autoload/popups" "\
Return what side a popup WINDOW came from ('left 'right 'above or 'below).

\(fn &optional WINDOW)" nil nil)

(autoload 'doom-popup-size "../core/autoload/popups" "\
Return the size of a popup WINDOW.

\(fn &optional WINDOW)" nil nil)

(autoload 'with-popup-rules! "../core/autoload/popups" "\
TODO

\(fn RULES &rest BODY)" nil t)

(function-put 'with-popup-rules! 'lisp-indent-function 'defun)

(autoload 'save-popups! "../core/autoload/popups" "\
Sets aside all popups before executing the original function, usually to
prevent the popup(s) from messing up the UI (or vice versa).

\(fn &rest BODY)" nil t)

(autoload 'doom/popup-restore "../core/autoload/popups" "\
Restore the last open popups. If the buffers have been killed, and
represented real files, they will be restored. Dead special buffers or buffers
with non-nil :autokill properties will not be.

Returns t if popups were restored, nil otherwise.

\(fn)" t nil)

(autoload 'doom/popup-toggle "../core/autoload/popups" "\
Toggle popups on and off. If used outside of popups (and popups are
available), it will select the nearest popup window.

\(fn)" t nil)

(autoload 'doom/popup-close "../core/autoload/popups" "\
Find and close WINDOW if it's a popup. If WINDOW is omitted, defaults to
`selected-window'. The contained buffer is buried, unless it has an :autokill
property.

\(fn &optional WINDOW)" t nil)

(autoload 'doom/popup-close-all "../core/autoload/popups" "\
Closes most open popups.

Does not close popups that are :static or don't have an :autoclose property (see
`shackle-rules').

If FORCE-P is non-nil (or this function is called interactively), ignore popups'
:autoclose property. This command will never close :static popups.

\(fn &optional FORCE-P)" t nil)

(autoload 'doom/popup-kill-all "../core/autoload/popups" "\
Like `doom/popup-close-all', but kill *all* popups, including :static ones,
without leaving any trace behind (muahaha).

\(fn)" t nil)

(autoload 'doom/popup-close-maybe "../core/autoload/popups" "\
Close the current popup *if* its window doesn't have a noesc parameter.

\(fn)" t nil)

(autoload 'doom/popup-this-buffer "../core/autoload/popups" "\
Display currently selected buffer in a popup window.

\(fn)" t nil)

(autoload 'doom/popup-toggle-messages "../core/autoload/popups" "\
Toggle *Messages* buffer.

\(fn)" t nil)

(autoload 'doom/other-popup "../core/autoload/popups" "\
Cycle through popup windows. Like `other-window', but for popups.

\(fn COUNT)" t nil)

(defalias 'other-popup #'doom/other-popup)

(autoload 'doom/popup-raise "../core/autoload/popups" "\
Turn a popup window into a normal window.

\(fn &optional WINDOW)" t nil)

(autoload 'doom/popup-move-top "../core/autoload/popups" "\
See `doom-popup-move'.

\(fn)" t nil)

(autoload 'doom/popup-move-bottom "../core/autoload/popups" "\
See `doom-popup-move'.

\(fn)" t nil)

(autoload 'doom/popup-move-left "../core/autoload/popups" "\
See `doom-popup-move'.

\(fn)" t nil)

(autoload 'doom/popup-move-right "../core/autoload/popups" "\
See `doom-popup-move'.

\(fn)" t nil)

(autoload 'doom-popup-mode "../core/autoload/popups" "\
Minor mode for popup windows.

\(fn &optional ARG)" t nil)

(autoload 'doom|hide-modeline-in-popup "../core/autoload/popups" "\
Don't show modeline in popup windows without a :modeline rule. If one exists
and it's a symbol, use `doom-modeline' to grab the format. If non-nil, show the
mode-line as normal. If nil (or omitted, by default), then hide the modeline
entirely.

\(fn)" nil nil)

(autoload 'doom*shackle-always-align "../core/autoload/popups" "\
Ensure popups are always aligned and selected by default. Eliminates the need
for :align t on every rule.

\(fn PLIST)" nil nil)

(autoload 'doom*popup-init "../core/autoload/popups" "\
Initializes a window as a popup window by enabling `doom-popup-mode' in it
and setting `doom-popup-rules' within it. Returns the window.

\(fn ORIG-FN &rest ARGS)" nil nil)

(autoload 'doom*popups-save "../core/autoload/popups" "\
Sets aside all popups before executing the original function, usually to
prevent the popup(s) from messing up the UI (or vice versa).

\(fn ORIG-FN &rest ARGS)" nil nil)

(autoload 'doom*delete-popup-window "../core/autoload/popups" "\
Ensure that popups are deleted properly, and killed if they have :autokill
properties.

\(fn &optional WINDOW)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../core/autoload/popups" '("doom--popup-data")))

;;;***

;;;### (autoloads nil "../core/autoload/scratch" "../core/autoload/scratch.el"
;;;;;;  (23288 273 447385 916000))
;;; Generated autoloads from ../core/autoload/scratch.el

(autoload 'doom/open-scratch-buffer "../core/autoload/scratch" "\
Opens a temporary scratch buffer in a popup window. It is discarded once it
is closed. If a region is active, copy it to the scratch buffer.

\(fn)" t nil)

(autoload 'doom/open-project-scratch-buffer "../core/autoload/scratch" "\
Opens a (persistent) scratch buffer associated with the current project in a
popup window. Scratch buffers are stored in `doom-scratch-files-dir'. If a
region is active, copy it to the scratch buffer.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../core/autoload/scratch" '("doom-")))

;;;***

;;;### (autoloads nil "../core/autoload/system" "../core/autoload/system.el"
;;;;;;  (23288 273 447656 783000))
;;; Generated autoloads from ../core/autoload/system.el

(autoload 'doom-system-os "../core/autoload/system" "\
Returns the OS: arch, debian, macos, general linux, cygwin or windows. If OS
is given, returns t if it matches the current system, and nil otherwise.

\(fn &optional OS)" nil nil)

(autoload 'doom-sh "../core/autoload/system" "\
Runs a shell command and prints any output to the DOOM buffer.

\(fn COMMAND &rest ARGS)" nil nil)

(autoload 'doom-sudo "../core/autoload/system" "\
Like `doom-sh', but runs as root (prompts for password).

\(fn COMMAND &rest ARGS)" nil nil)

(autoload 'doom-fetch "../core/autoload/system" "\
Clone a remote version-controlled repo at REPO-URL to PATH, if it exists.
Requires the corresponding client, e.g. git for git repos, hg for mercurial,
etc.

\(fn FETCHER LOCATION DEST)" nil nil)

;;;***

;;;### (autoloads nil "../core/autoload/test" "../core/autoload/test.el"
;;;;;;  (23288 273 448049 420000))
;;; Generated autoloads from ../core/autoload/test.el

(autoload 'doom//run-tests "../core/autoload/test" "\
Run all loaded tests, specified by MODULES (a list of module cons cells) or
command line args following a double dash (each arg should be in the
'module/submodule' format).

If neither is available, run all tests in all enabled modules.

\(fn &optional MODULES)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../core/autoload/test" '("with-minor-mode!!" "point!!" "goto-char!!" "should-buffer!!" "def-test!")))

;;;***

;;;### (autoloads nil "../core/autoload/ui" "../core/autoload/ui.el"
;;;;;;  (23288 273 448549 664000))
;;; Generated autoloads from ../core/autoload/ui.el

(autoload 'doom/toggle-fullscreen "../core/autoload/ui" "\
Toggle fullscreen Emacs (non-native on MacOS).

\(fn)" t nil)

(autoload 'doom/toggle-line-numbers "../core/autoload/ui" "\
Toggle `display-line-numbers-mode'.

\(fn &optional ARG)" t nil)

(autoload 'doom-resize-window "../core/autoload/ui" "\
Resize a window to NEW-SIZE. If HORIZONTAL, do it width-wise.
If FORCE-P is omitted when `window-size-fixed' is non-nil, resizing will fail.

\(fn WINDOW NEW-SIZE &optional HORIZONTAL FORCE-P)" nil nil)

(autoload 'doom/window-zoom "../core/autoload/ui" "\
Close other windows to focus on this one. Activate again to undo this. If the
window changes before then, the undo expires.

Alternatively, use `doom/window-enlargen'.

\(fn)" t nil)

(autoload 'doom/window-enlargen "../core/autoload/ui" "\
Enlargen the current window to focus on this one. Does not close other
windows (unlike `doom/window-zoom') Activate again to undo.

\(fn)" t nil)

(autoload 'doom/delete-frame "../core/autoload/ui" "\
Delete the current frame, but ask for confirmation if it isn't empty.

\(fn)" t nil)

(defvar doom-big-font-mode nil "\
Non-nil if Doom-Big-Font mode is enabled.
See the `doom-big-font-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `doom-big-font-mode'.")

(custom-autoload 'doom-big-font-mode "../core/autoload/ui" nil)

(autoload 'doom-big-font-mode "../core/autoload/ui" "\
A global mode that resizes the font, for streams, screen-sharing and
presentations.

\(fn &optional ARG)" t nil)

(autoload 'doom//reload-theme "../core/autoload/ui" "\
Reset the color theme currently in use.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../core/autoload/ui" '("doom--window-enlargened")))

;;;***

;;;### (autoloads nil "../modules/app/irc/autoload/irc" "../modules/app/irc/autoload/irc.el"
;;;;;;  (23288 273 457366 932000))
;;; Generated autoloads from ../modules/app/irc/autoload/irc.el

(autoload '=irc "../modules/app/irc/autoload/irc" "\
Connect to IRC and auto-connect to all registered networks.

If INHIBIT-WORKSPACE (the universal argument) is non-nil, don't spawn a new
workspace for it.

\(fn &optional INHIBIT-WORKSPACE)" t nil)

(autoload '+irc/connect "../modules/app/irc/autoload/irc" "\
Connect to a specific registered server.

If INHIBIT-WORKSPACE (the universal argument) is non-nil, don't spawn a new
workspace for it.

\(fn &optional INHIBIT-WORKSPACE)" t nil)

(autoload '+irc/quit "../modules/app/irc/autoload/irc" "\
Kill current circe session and workgroup.

\(fn)" t nil)

(autoload '+irc/ivy-jump-to-channel "../modules/app/irc/autoload/irc" "\
Jump to an open channel or server buffer with ivy. If THIS-SERVER (universal
argument) is non-nil only show channels in current server.

\(fn &optional THIS-SERVER)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../modules/app/irc/autoload/irc" '("+irc-")))

;;;***

;;;### (autoloads nil "../modules/completion/company/autoload" "../modules/completion/company/autoload.el"
;;;;;;  (23288 273 460891 862000))
;;; Generated autoloads from ../modules/completion/company/autoload.el

(autoload '+company/complete "../modules/completion/company/autoload" "\
Bring up the completion popup. If only one result, complete it.

\(fn)" t nil)

(autoload '+company/whole-lines "../modules/completion/company/autoload" "\
`company-mode' completion backend that completes whole-lines, akin to vim's
C-x C-l.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(autoload '+company/dict-or-keywords "../modules/completion/company/autoload" "\
`company-mode' completion combining `company-dict' and `company-keywords'.

\(fn)" t nil)

(autoload '+company/dabbrev-code-previous "../modules/completion/company/autoload" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "../modules/feature/eval/autoload/eval" "../modules/feature/eval/autoload/eval.el"
;;;;;;  (23288 273 466835 142000))
;;; Generated autoloads from ../modules/feature/eval/autoload/eval.el

(autoload '+eval/buffer "../modules/feature/eval/autoload/eval" "\
Evaluate the whole buffer.

\(fn)" t nil)

(autoload '+eval/region "../modules/feature/eval/autoload/eval" "\
Evaluate a region between BEG and END and display the output.

\(fn BEG END)" t nil)

(autoload '+eval/region-and-replace "../modules/feature/eval/autoload/eval" "\
Evaluation a region between BEG and END, and replace it with the result.

\(fn BEG END)" t nil)

;;;***

;;;### (autoloads nil "../modules/feature/eval/autoload/repl" "../modules/feature/eval/autoload/repl.el"
;;;;;;  (23288 273 467303 997000))
;;; Generated autoloads from ../modules/feature/eval/autoload/repl.el

(autoload '+eval/open-repl "../modules/feature/eval/autoload/repl" "\
Opens (or reopens) the REPL associated with the current major-mode and place
the cursor at the prompt.

\(fn)" t nil)

(autoload '+eval/send-region-to-repl "../modules/feature/eval/autoload/repl" "\
REPL must be open! Sends a selected region to it. If AUTO-EXECUTE-P, then
execute it immediately after.

\(fn BEG END &optional AUTO-EXECUTE-P)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../modules/feature/eval/autoload/repl" '("+eval-")))

;;;***

;;;### (autoloads nil "../modules/feature/jump/autoload/jump" "../modules/feature/jump/autoload/jump.el"
;;;;;;  (23288 273 488458 539000))
;;; Generated autoloads from ../modules/feature/jump/autoload/jump.el

(autoload '+jump/definition "../modules/feature/jump/autoload/jump" "\
Jump to the definition of the symbol at point.

Tries xref and falls back to `dumb-jump', then rg/ag, then
`evil-goto-definition' (if evil is active).

\(fn IDENTIFIER &optional OTHER-WINDOW)" t nil)

(autoload '+jump/references "../modules/feature/jump/autoload/jump" "\
Show a list of references to the symbol at point.

Tries `xref-find-references' and falls back to rg/ag.

\(fn IDENTIFIER)" t nil)

(autoload '+jump/documentation "../modules/feature/jump/autoload/jump" "\
Show documentation for the symbol at point, if available.

\(fn IDENTIFIER)" t nil)

(autoload '+jump/online "../modules/feature/jump/autoload/jump" "\
Looks up SEARCH (a string) in you browser using PROVIDER.

PROVIDER should be a key of `+jump-search-provider-alist'.

When used interactively, it will prompt for a query and, for the first time, the
provider from `+jump-search-provider-alist'. On consecutive uses, the last
provider will be reused. If the universal argument is supplied, always prompt
for the provider.

\(fn SEARCH &optional PROVIDER)" t nil)

(autoload '+jump/online-select "../modules/feature/jump/autoload/jump" "\
Runs `+jump/online', but always prompts for the provider to use.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../modules/feature/jump/autoload/jump" '("+jump-")))

;;;***

;;;### (autoloads nil "../modules/feature/services/autoload" "../modules/feature/services/autoload.el"
;;;;;;  (23288 273 489355 206000))
;;; Generated autoloads from ../modules/feature/services/autoload.el

(autoload '+services/create "../modules/feature/services/autoload" "\
Interactively create a new prodigy service.

\(fn)" t nil)

(autoload '+services/prodigy-delete "../modules/feature/services/autoload" "\
Delete service at point. Asks for confirmation.

\(fn ARG)" t nil)

(autoload '+services/cleanup "../modules/feature/services/autoload" "\
Delete all services associated with projects that don't exist.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../modules/feature/snippets/autoload/snippets"
;;;;;;  "../modules/feature/snippets/autoload/snippets.el" (23288
;;;;;;  273 490485 358000))
;;; Generated autoloads from ../modules/feature/snippets/autoload/snippets.el

(autoload '+snippets/goto-start-of-field "../modules/feature/snippets/autoload/snippets" "\
Go to the beginning of the current field.

\(fn)" t nil)

(autoload '+snippets/goto-end-of-field "../modules/feature/snippets/autoload/snippets" "\
Go to the end of the current field.

\(fn)" t nil)

(autoload '+snippets/delete-backward-char "../modules/feature/snippets/autoload/snippets" "\
Prevents Yas from interfering with backspace deletion.

\(fn &optional FIELD)" t nil)

(autoload '+snippets/delete-forward-char-or-field "../modules/feature/snippets/autoload/snippets" "\
Delete forward, or skip the current field if it's empty. This is to prevent
buggy behavior when <delete> is pressed in an empty field.

\(fn &optional FIELD)" t nil)

(autoload '+snippets/delete-to-start-of-field "../modules/feature/snippets/autoload/snippets" "\
Delete to start-of-field.

\(fn &optional FIELD)" t nil)

;;;***

;;;### (autoloads nil "../modules/feature/version-control/autoload"
;;;;;;  "../modules/feature/version-control/autoload.el" (23288 273
;;;;;;  492503 148000))
;;; Generated autoloads from ../modules/feature/version-control/autoload.el

(autoload '+vcs-root "../modules/feature/version-control/autoload" "\
Get git url root.

\(fn)" nil nil)

(autoload '+vcs/git-browse "../modules/feature/version-control/autoload" "\
Open the website for the current version controlled file. Fallback to
repository root.

\(fn)" t nil)

(autoload '+vcs/git-browse-issues "../modules/feature/version-control/autoload" "\
Open the github issues page for current repo.

\(fn)" t nil)

(autoload '+vcs|init-header-line "../modules/feature/version-control/autoload" "\
Toggle the git-timemachine header-line on activate. Use this on
`git-timemachine-mode-hook'.

\(fn)" nil nil)

(autoload '+vcs|enable-smerge-mode-maybe "../modules/feature/version-control/autoload" "\
Auto-enable `smerge-mode' when merge conflict is detected.

\(fn)" nil nil)

(autoload '+vcs*update-header-line "../modules/feature/version-control/autoload" "\
Show revision details in the header-line, instead of the minibuffer.

Sometimes I forget `git-timemachine' is enabled in a buffer. Putting info into,
putting them in `header-line-format' has better visibility.

\(fn &rest _)" nil nil)

;;;***

;;;### (autoloads nil "../modules/feature/workspaces/autoload/workspaces"
;;;;;;  "../modules/feature/workspaces/autoload/workspaces.el" (23288
;;;;;;  273 493875 473000))
;;; Generated autoloads from ../modules/feature/workspaces/autoload/workspaces.el

(defalias #'+workspace-p #'persp-p "\
Return t if OBJ is a perspective hash table.")

(autoload '+workspace-exists-p "../modules/feature/workspaces/autoload/workspaces" "\
Returns t if NAME is the name of an existing workspace.

\(fn NAME)" nil nil)

(autoload '+workspace-contains-buffer-p "../modules/feature/workspaces/autoload/workspaces" "\
Return non-nil if buffer is in workspace (defaults to current workspace).

\(fn BUFFER &optional WORKSPACE)" nil nil)

(autoload '+workspace-get "../modules/feature/workspaces/autoload/workspaces" "\
Returns a workspace (perspective hash table) named NAME.

\(fn NAME &optional NOERROR)" nil nil)

(defalias '+workspace-current #'get-current-persp)

(autoload '+workspace-current-name "../modules/feature/workspaces/autoload/workspaces" "\
Get the name of the current workspace.

\(fn)" nil nil)

(autoload '+workspace-list "../modules/feature/workspaces/autoload/workspaces" "\
Return a list of workspace structs.

\(fn)" nil nil)

(autoload '+workspace-list-names "../modules/feature/workspaces/autoload/workspaces" "\
Return a list of workspace names (strings).

\(fn)" nil nil)

(autoload '+workspace-buffer-list "../modules/feature/workspaces/autoload/workspaces" "\
Return a list of buffers in PERSP (defaults to the current perspective).

The buffer list is ordered by recency (same as `buffer-list').

PERSP can be a string (name of a workspace) or a perspective hash (satisfies
`+workspace-p').

If PERSP is t, then return a list of orphaned buffers associated with no
perspectives.

\(fn &optional PERSP)" nil nil)

(autoload '+workspace-load "../modules/feature/workspaces/autoload/workspaces" "\
Loads a single workspace (named NAME) into the current session. Can only
retrieve perspectives that were explicitly saved with `+workspace-save'.

Returns t if successful, nil otherwise.

\(fn NAME)" nil nil)

(autoload '+workspace-load-session "../modules/feature/workspaces/autoload/workspaces" "\
Replace current session with the entire session named NAME. If NAME is nil,
use `persp-auto-save-fname'.

\(fn &optional NAME)" nil nil)

(autoload '+workspace-save "../modules/feature/workspaces/autoload/workspaces" "\
Saves a single workspace (NAME) from the current session. Can be loaded again
with `+workspace-load'. NAME can be the string name of a workspace or its
perspective hash table.

Returns t on success, nil otherwise.

\(fn NAME)" nil nil)

(autoload '+workspace-save-session "../modules/feature/workspaces/autoload/workspaces" "\
Save a whole session as NAME. If NAME is nil, use `persp-auto-save-fname'.
Return t on success, nil otherwise.

\(fn &optional NAME)" nil nil)

(autoload '+workspace-new "../modules/feature/workspaces/autoload/workspaces" "\
Create a new workspace named NAME. If one already exists, return nil.
Otherwise return t on success, nil otherwise.

\(fn NAME)" nil nil)

(autoload '+workspace-rename "../modules/feature/workspaces/autoload/workspaces" "\
Rename the current workspace named NAME to NEW-NAME. Returns old name on
success, nil otherwise.

\(fn NAME NEW-NAME)" nil nil)

(autoload '+workspace-delete "../modules/feature/workspaces/autoload/workspaces" "\
Delete the workspace denoted by NAME, which can be the name of a perspective
or its hash table. If INHIBIT-KILL-P is non-nil, don't kill this workspace's
buffers.

\(fn NAME &optional INHIBIT-KILL-P)" nil nil)

(autoload '+workspace-switch "../modules/feature/workspaces/autoload/workspaces" "\
Switch to another workspace.

\(fn NAME &optional AUTO-CREATE-P)" nil nil)

(autoload '+workspace/load "../modules/feature/workspaces/autoload/workspaces" "\
Load a workspace and switch to it. If called with C-u, try to reload the
current workspace (by name) from session files.

\(fn NAME)" t nil)

(autoload '+workspace/load-session "../modules/feature/workspaces/autoload/workspaces" "\
Load a session and switch to it. If called with C-u, try to load the last
session.

\(fn &optional NAME)" t nil)

(autoload '+workspace/save "../modules/feature/workspaces/autoload/workspaces" "\
Save the current workspace. If called with C-u, autosave the current
workspace.

\(fn NAME)" t nil)

(autoload '+workspace/save-session "../modules/feature/workspaces/autoload/workspaces" "\
Save the current session. If called with C-u, prompt you for the name to save
the session as.

\(fn &optional NAME)" t nil)

(autoload '+workspace/rename "../modules/feature/workspaces/autoload/workspaces" "\
Rename the current workspace.

\(fn NEW-NAME)" t nil)

(autoload '+workspace/delete "../modules/feature/workspaces/autoload/workspaces" "\
Delete this workspace. If called with C-u, prompts you for the name of the
workspace to delete.

\(fn NAME)" t nil)

(autoload '+workspace/kill-session "../modules/feature/workspaces/autoload/workspaces" "\
Delete the current session, clears all workspaces, windows and buffers.

\(fn)" t nil)

(autoload '+workspace/kill-session-and-quit "../modules/feature/workspaces/autoload/workspaces" "\
Forgets current session and quits.

\(fn)" t nil)

(autoload '+workspace/new "../modules/feature/workspaces/autoload/workspaces" "\
Create a new workspace named NAME. If OVERWRITE-P is non-nil, clear any
pre-existing workspace.

\(fn &optional NAME CLONE-P)" t nil)

(autoload '+workspace/switch-to "../modules/feature/workspaces/autoload/workspaces" "\
Switch to a workspace at a given INDEX. A negative number will start from the
end of the workspace list.

\(fn INDEX)" t nil)

(autoload '+workspace/switch-to-last "../modules/feature/workspaces/autoload/workspaces" "\
Switch to the last workspace.

\(fn)" t nil)

(autoload '+workspace/cycle "../modules/feature/workspaces/autoload/workspaces" "\
Cycle n workspaces to the right (default) or left.

\(fn N)" t nil)

(autoload '+workspace/switch-left "../modules/feature/workspaces/autoload/workspaces" "\


\(fn)" t nil)

(autoload '+workspace/switch-right "../modules/feature/workspaces/autoload/workspaces" "\


\(fn)" t nil)

(autoload '+workspace/close-window-or-workspace "../modules/feature/workspaces/autoload/workspaces" "\
Close the selected window. If it's the last window in the workspace, close
the workspace and move to the next.

\(fn)" t nil)

(autoload '+workspace/close-workspace-or-frame "../modules/feature/workspaces/autoload/workspaces" "\
Close the current workspace. If it's the last, delete the frame instead.

\(fn)" t nil)

(autoload '+workspace-message "../modules/feature/workspaces/autoload/workspaces" "\
Show an 'elegant' message in the echo area next to a listing of workspaces.

\(fn MESSAGE &optional TYPE)" nil nil)

(autoload '+workspace-error "../modules/feature/workspaces/autoload/workspaces" "\
Show an 'elegant' error in the echo area next to a listing of workspaces.

\(fn MESSAGE &optional NOERROR)" nil nil)

(autoload '+workspace/display "../modules/feature/workspaces/autoload/workspaces" "\
Display a list of workspaces (like tabs) in the echo area.

\(fn)" t nil)

(autoload '+workspace-on-new-frame "../modules/feature/workspaces/autoload/workspaces" "\
Spawn a perspective for each new frame.

\(fn FRAME &optional NEW-FRAME-P)" nil nil)

(autoload '+workspaces|delete-associated-workspace-maybe "../modules/feature/workspaces/autoload/workspaces" "\
Delete workspace associated with current frame IF it has no real buffers.

\(fn FRAME)" nil nil)

(autoload '+workspaces*autosave-real-buffers "../modules/feature/workspaces/autoload/workspaces" "\
Don't autosave if no real buffers are open.

\(fn ORIG-FN &rest ARGS)" nil nil)

(autoload '+workspaces*switch-project-by-name "../modules/feature/workspaces/autoload/workspaces" "\
Switch to a project and prompt for a file to open.

Ensures the scratch (or dashboard) buffers are CDed into the project's root.

\(fn ORIG-FN &rest ARGS)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../modules/feature/workspaces/autoload/workspaces" '("+workspace-")))

;;;***

;;;### (autoloads nil "../modules/lang/emacs-lisp/autoload" "../modules/lang/emacs-lisp/autoload.el"
;;;;;;  (23288 273 499463 840000))
;;; Generated autoloads from ../modules/lang/emacs-lisp/autoload.el

(autoload '+emacs-lisp/repl "../modules/lang/emacs-lisp/autoload" "\
Open the Emacs Lisp REPL (`ielm').

\(fn)" t nil)

(autoload '+emacs-lisp-eval "../modules/lang/emacs-lisp/autoload" "\
Evaluate a region and print it to the echo area (if one line long), otherwise
to a pop up buffer.

\(fn BEG END)" nil nil)

;;;***

;;;### (autoloads nil "../modules/lang/java/autoload" "../modules/lang/java/autoload.el"
;;;;;;  (23288 273 503163 452000))
;;; Generated autoloads from ../modules/lang/java/autoload.el

(autoload '+java-android-mode-is-layout-file "../modules/lang/java/autoload" "\


\(fn)" nil nil)

(autoload '+java-android-mode-in-tags "../modules/lang/java/autoload" "\


\(fn &rest TAGS)" nil nil)

(autoload '+java-android-mode-tag-name "../modules/lang/java/autoload" "\


\(fn)" nil nil)

(autoload '+java|android-mode-maybe "../modules/lang/java/autoload" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil "../modules/lang/javascript/autoload" "../modules/lang/javascript/autoload.el"
;;;;;;  (23288 273 504390 100000))
;;; Generated autoloads from ../modules/lang/javascript/autoload.el

(autoload '+javascript-npm-conf "../modules/lang/javascript/autoload" "\
Retrieves an alist of this project's 'package.json'. If REFRESH-P is non-nil
ignore the cache.

\(fn &optional PROJECT-ROOT REFRESH-P)" nil nil)

(autoload '+javascript-npm-dep-p "../modules/lang/javascript/autoload" "\


\(fn PACKAGES &optional PROJECT-ROOT REFRESH-P)" nil nil)

(autoload '+javascript/repl "../modules/lang/javascript/autoload" "\
Open a Javascript REPL. Meaning either `skewer-repl', if any of the
skewer-*-mode's are enabled, or `nodejs-repl' otherwise.

\(fn)" t nil)

(autoload '+javascript/skewer-this-buffer "../modules/lang/javascript/autoload" "\
Toggle a globalized skewer-mode, attaching an external browser (once),
initiating an internal httpd server (once) and enabling the appropriate
skewer-mode for the current buffer.

Run this for any buffer you want to skewer.

\(fn)" t nil)

(autoload '+javascript/skewer-cleanup "../modules/lang/javascript/autoload" "\
Disable skewer-mode globally and disable the httpd server.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../modules/lang/javascript/autoload" '("+javascript-npm-conf")))

;;;***

;;;### (autoloads nil "../modules/lang/markdown/autoload" "../modules/lang/markdown/autoload.el"
;;;;;;  (23288 273 507735 450000))
;;; Generated autoloads from ../modules/lang/markdown/autoload.el

(autoload '+markdown/insert-del "../modules/lang/markdown/autoload" "\
Surround region in github strike-through delimiters.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../modules/lang/markdown/autoload" '("+markdown--regex-del")))

;;;***

;;;### (autoloads nil "../modules/lang/org/autoload/org" "../modules/lang/org/autoload/org.el"
;;;;;;  (23288 273 511831 52000))
;;; Generated autoloads from ../modules/lang/org/autoload/org.el

(autoload '+org-pretty-mode "../modules/lang/org/autoload/org" "\
TODO

\(fn &optional ARG)" t nil)

(autoload '+org|realign-table-maybe "../modules/lang/org/autoload/org" "\
Auto-align table under cursor and re-calculate formulas.

\(fn)" nil nil)

(autoload '+org|update-cookies "../modules/lang/org/autoload/org" "\
Update counts in headlines (aka \"cookies\").

\(fn)" nil nil)

(autoload '+org/dwim-at-point "../modules/lang/org/autoload/org" "\
Do-what-I-mean at point.

If on a:
- checkbox list item or todo heading: toggle it.
- clock: update its time.
- headline: toggle latex fragments and inline images underneath.
- footnote definition: jump to the footnote
- table-row or a TBLFM: recalculate the table's formulas
- table-cell: clear it and go into insert mode. If this is a formula cell,
  recaluclate it instead.
- babel-call: execute the source block
- statistics-cookie: update it.
- latex fragment: toggle it.
- link: follow it
- otherwise, refresh all inline images in current tree.

\(fn)" t nil)

(autoload '+org/indent "../modules/lang/org/autoload/org" "\
Indent the current item (header or item). Otherwise, forward to
`self-insert-command'.

\(fn)" t nil)

(autoload '+org/indent-or-next-field-or-yas-expand "../modules/lang/org/autoload/org" "\
Depending on the context either a) indent the current line, b) go the next
table field or c) run `yas-expand'.

\(fn)" t nil)

(autoload '+org/dedent "../modules/lang/org/autoload/org" "\
Dedent the current item (header or item). Otherwise, forward to
`self-insert-command'.

\(fn)" t nil)

(autoload '+org/dedent-or-prev-field "../modules/lang/org/autoload/org" "\
Depending on the context either dedent the current item or go the previous
table field.

\(fn)" t nil)

(autoload '+org/insert-item "../modules/lang/org/autoload/org" "\
Inserts a new heading, table cell or item, depending on the context.
DIRECTION can be 'above or 'below.

I use this instead of `org-insert-item' or `org-insert-heading' which are too
opinionated and perform this simple task incorrectly (e.g. whitespace in the
wrong places).

\(fn DIRECTION)" t nil)

(autoload '+org-get-property "../modules/lang/org/autoload/org" "\
Get a propery from an org file.

\(fn NAME &optional FILE)" nil nil)

(autoload '+org/refresh-inline-images "../modules/lang/org/autoload/org" "\
Refresh image previews in the current heading/tree.

\(fn)" t nil)

(autoload '+org/toggle-checkbox "../modules/lang/org/autoload/org" "\
Toggle the presence of a checkbox in the current item.

\(fn)" t nil)

(autoload '+org/toggle-fold "../modules/lang/org/autoload/org" "\
Toggle the local fold at the point (as opposed to cycling through all levels
with `org-cycle'). Also:

  + If in a babel block, removes result blocks.
  + If in a table, realign it, if necessary.

\(fn)" t nil)

(autoload '+org/remove-link "../modules/lang/org/autoload/org" "\
Unlink the text at point.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../modules/lang/org/autoload/org-attach" "../modules/lang/org/autoload/org-attach.el"
;;;;;;  (23288 273 510825 776000))
;;; Generated autoloads from ../modules/lang/org/autoload/org-attach.el

(autoload '+org-attach/file "../modules/lang/org/autoload/org-attach" "\
Copies the file at PATH to `+org-attach-dir' and places an org link to it at
the cursor.

\(fn PATH)" t nil)

(autoload '+org-attach/uri "../modules/lang/org/autoload/org-attach" "\
Downloads the file at URL and place an org link to it at the cursor.

\(fn URI)" t nil)

(autoload '+org-attach-download-dnd "../modules/lang/org/autoload/org-attach" "\
TODO

\(fn URI ACTION)" nil nil)

(autoload '+org-attach*link-format "../modules/lang/org/autoload/org-attach" "\


\(fn FILENAME &optional EXT)" nil nil)

(autoload '+org-attach*insert-link "../modules/lang/org/autoload/org-attach" "\
TODO

\(fn LINK FILENAME)" nil nil)

(autoload '+org-attach*relative-to-attach-dir "../modules/lang/org/autoload/org-attach" "\
TODO

\(fn ORIG-FN &rest ARGS)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../modules/lang/org/autoload/org-attach" '("+org-attach--icon")))

;;;***

;;;### (autoloads nil "../modules/lang/org/autoload/org-babel" "../modules/lang/org/autoload/org-babel.el"
;;;;;;  (23288 273 511045 207000))
;;; Generated autoloads from ../modules/lang/org/autoload/org-babel.el

(autoload '+org-babel/edit "../modules/lang/org/autoload/org-babel" "\
Edit the source block at point in a popup.

If ARG is non-nil (universal argument), use the current window.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads nil "../modules/lang/org/autoload/org-capture"
;;;;;;  "../modules/lang/org/autoload/org-capture.el" (23288 273
;;;;;;  511225 507000))
;;; Generated autoloads from ../modules/lang/org/autoload/org-capture.el

(autoload '+org-capture/open "../modules/lang/org/autoload/org-capture" "\
Sends STRING, the current selection or prompted input to `org-capture'.

Uses the capture template specified by KEY. Otherwise, prompts you for one.

\(fn &optional STRING KEY)" t nil)

(autoload '+org-capture|cleanup-frame "../modules/lang/org/autoload/org-capture" "\
Closes the org-capture frame once done adding an entry.

\(fn)" nil nil)

(autoload '+org-capture-frame-p "../modules/lang/org/autoload/org-capture" "\
Return t if the current frame is an org-capture frame opened by
`+org-capture/open-frame'.

\(fn &rest _)" nil nil)

(autoload '+org-capture/open-frame "../modules/lang/org/autoload/org-capture" "\
Opens the org-capture window in a floating frame that cleans itself up once
you're done. This can be called from an external shell script.

\(fn &optional STRING KEY)" t nil)

(autoload '+org-capture-available-keys "../modules/lang/org/autoload/org-capture" "\
TODO

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../modules/lang/org/autoload/org-capture" '("+org-capture-window-params")))

;;;***

;;;### (autoloads nil "../modules/lang/org/autoload/org-link" "../modules/lang/org/autoload/org-link.el"
;;;;;;  (23288 273 511370 754000))
;;; Generated autoloads from ../modules/lang/org/autoload/org-link.el

(autoload '+org-link-read-file "../modules/lang/org/autoload/org-link" "\


\(fn KEY DIR)" nil nil)

(autoload '+org-link-read-directory "../modules/lang/org/autoload/org-link" "\


\(fn KEY DIR)" nil nil)

;;;***

;;;### (autoloads nil "../modules/lang/org/autoload/org-present"
;;;;;;  "../modules/lang/org/autoload/org-present.el" (23288 273
;;;;;;  511537 93000))
;;; Generated autoloads from ../modules/lang/org/autoload/org-present.el

(autoload '+org-present/start "../modules/lang/org/autoload/org-present" "\
TODO

\(fn)" t nil)

(autoload '+org-present|add-overlays "../modules/lang/org/autoload/org-present" "\


\(fn)" nil nil)

(autoload '+org-present|remove-overlays "../modules/lang/org/autoload/org-present" "\


\(fn)" nil nil)

(autoload '+org-present|detect-slide "../modules/lang/org/autoload/org-present" "\


\(fn)" nil nil)

(autoload '+org-present|init-org-tree-window "../modules/lang/org/autoload/org-present" "\
Set up the org window for presentation.

\(fn)" nil nil)

(autoload '+org-present*narrow-to-subtree "../modules/lang/org/autoload/org-present" "\
Narrow to the target subtree when you start the presentation.

\(fn ORIG-FN &rest ARGS)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../modules/lang/org/autoload/org-present" '("+org-present--")))

;;;***

;;;### (autoloads nil "../modules/lang/org/autoload/tables" "../modules/lang/org/autoload/tables.el"
;;;;;;  (23288 273 512113 881000))
;;; Generated autoloads from ../modules/lang/org/autoload/tables.el

(autoload '+org/table-next-row "../modules/lang/org/autoload/tables" "\
Go to the next row (same column) in the current table.

\(fn)" t nil)

(autoload '+org/table-previous-row "../modules/lang/org/autoload/tables" "\
Go to the previous row (same column) in the current table. Before doing so,
re-align the table if necessary. (Necessary because org-mode has a
`org-table-next-row', but not `org-table-previous-row')

\(fn)" t nil)

(autoload '+org/table-next-field "../modules/lang/org/autoload/tables" "\


\(fn)" t nil)

(autoload '+org/table-previous-field "../modules/lang/org/autoload/tables" "\


\(fn)" t nil)

(autoload '+org/table-append-field-or-shift-right "../modules/lang/org/autoload/tables" "\


\(fn)" t nil)

(autoload '+org/table-prepend-field-or-shift-left "../modules/lang/org/autoload/tables" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "../modules/lang/sh/autoload" "../modules/lang/sh/autoload.el"
;;;;;;  (23288 273 519234 861000))
;;; Generated autoloads from ../modules/lang/sh/autoload.el

(autoload '+sh--match-variables-in-quotes "../modules/lang/sh/autoload" "\
Search for variables in double-quoted strings bounded by LIMIT.

\(fn LIMIT)" nil nil)

(autoload '+sh--match-command-subst-in-quotes "../modules/lang/sh/autoload" "\
Search for variables in double-quoted strings bounded by LIMIT.

\(fn LIMIT)" nil nil)

(autoload '+sh/repl "../modules/lang/sh/autoload" "\
Open a shell REPL.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "../modules/lang/web/autoload/css" "../modules/lang/web/autoload/css.el"
;;;;;;  (23288 273 521658 82000))
;;; Generated autoloads from ../modules/lang/web/autoload/css.el

(autoload '+css/toggle-inline-or-block "../modules/lang/web/autoload/css" "\
Toggles between a bracketed block and inline block.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../modules/lang/web/autoload/html" "../modules/lang/web/autoload/html.el"
;;;;;;  (23288 273 522156 648000))
;;; Generated autoloads from ../modules/lang/web/autoload/html.el

(autoload '+web-encode-entities "../modules/lang/web/autoload/html" "\
TODO

\(fn TEXT)" nil nil)

(autoload '+web-decode-entities "../modules/lang/web/autoload/html" "\
TODO

\(fn TEXT)" nil nil)

(autoload '+web/encode-entities-region "../modules/lang/web/autoload/html" "\
Encode HTML entities in region.

\(fn BEG END)" t nil)

(autoload '+web/decode-entities-region "../modules/lang/web/autoload/html" "\
Decode HTML entities in region.

\(fn BEG END)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../modules/lang/web/autoload/html" '("+web-")))

;;;***

;;;### (autoloads nil "../modules/private/default/autoload/default"
;;;;;;  "../modules/private/default/autoload/default.el" (23288 273
;;;;;;  524104 566000))
;;; Generated autoloads from ../modules/private/default/autoload/default.el

(autoload '+default/yank-buffer-filename "../modules/private/default/autoload/default" "\
Copy the current buffer's path to the kill ring.

\(fn)" t nil)

(autoload '+default--def-browse-in! "../modules/private/default/autoload/default" "\


\(fn NAME DIR)" nil t)

(autoload '+default--def-find-in! "../modules/private/default/autoload/default" "\


\(fn NAME DIR)" nil t)
 (autoload '+default/browse-project "private/default/autoload/default" nil t)
 (autoload '+default/find-in-templates "private/default/autoload/default" nil t)
 (autoload '+default/browse-templates "private/default/autoload/default" nil t)
 (autoload '+default/find-in-emacsd "private/default/autoload/default" nil t)
 (autoload '+default/browse-emacsd "private/default/autoload/default" nil t)
 (autoload '+default/find-in-notes "private/default/autoload/default" nil t)
 (autoload '+default/browse-notes "private/default/autoload/default" nil t)
 (autoload '+default/find-in-snippets "private/default/autoload/default" nil t)

;;;***

;;;### (autoloads nil "../modules/tools/eshell/autoload/eshell" "../modules/tools/eshell/autoload/eshell.el"
;;;;;;  (23288 273 525976 537000))
;;; Generated autoloads from ../modules/tools/eshell/autoload/eshell.el

(autoload '+eshell/open "../modules/tools/eshell/autoload/eshell" "\
Open eshell in the current buffer.

\(fn &optional COMMAND)" t nil)

(autoload '+eshell/open-popup "../modules/tools/eshell/autoload/eshell" "\
Open eshell in a popup window.

\(fn &optional COMMAND)" t nil)

(autoload '+eshell/open-workspace "../modules/tools/eshell/autoload/eshell" "\
Open eshell in a separate workspace. Requires the (:feature workspaces)
module to be loaded.

\(fn &optional COMMAND)" t nil)

(autoload '+eshell|init "../modules/tools/eshell/autoload/eshell" "\
Keep track of eshell buffers.

\(fn)" nil nil)

(autoload '+eshell|cleanup "../modules/tools/eshell/autoload/eshell" "\
Close window (or workspace) on quit.

\(fn)" nil nil)

(autoload '+eshell/quit-or-delete-char "../modules/tools/eshell/autoload/eshell" "\


\(fn ARG)" t nil)

(autoload '+eshell/split "../modules/tools/eshell/autoload/eshell" "\


\(fn)" t nil)

(autoload '+eshell/vsplit "../modules/tools/eshell/autoload/eshell" "\


\(fn)" t nil)

(autoload '+eshell-prompt "../modules/tools/eshell/autoload/eshell" "\


\(fn)" nil nil)

(autoload '+eshell/evil-append "../modules/tools/eshell/autoload/eshell" "\


\(fn)" t nil)

(autoload '+eshell/evil-append-maybe "../modules/tools/eshell/autoload/eshell" "\


\(fn)" t nil)

(autoload '+eshell/evil-prepend "../modules/tools/eshell/autoload/eshell" "\


\(fn)" t nil)

(autoload '+eshell/evil-prepend-maybe "../modules/tools/eshell/autoload/eshell" "\


\(fn)" t nil)

(autoload '+eshell/evil-replace-maybe "../modules/tools/eshell/autoload/eshell" "\


\(fn)" t nil)

(autoload '+eshell/evil-replace-state-maybe "../modules/tools/eshell/autoload/eshell" "\


\(fn)" t nil)

(autoload '+eshell/evil-change "../modules/tools/eshell/autoload/eshell" "\


\(fn)" t nil)

(autoload '+eshell/evil-change-line "../modules/tools/eshell/autoload/eshell" "\


\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../modules/tools/eshell/autoload/eshell" '("+eshell-")))

;;;***

;;;### (autoloads nil "../modules/tools/gist/autoload/gist" "../modules/tools/gist/autoload/gist.el"
;;;;;;  (23288 273 527101 508000))
;;; Generated autoloads from ../modules/tools/gist/autoload/gist.el

(autoload '+gist/open-current "../modules/tools/gist/autoload/gist" "\


\(fn)" t nil)

(autoload '+gist/kill-cache "../modules/tools/gist/autoload/gist" "\
Clears the gist cache. Necessary when a faulty cache causes gist.el to be
entirely unuseable.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../modules/tools/impatient-mode/autoload"
;;;;;;  "../modules/tools/impatient-mode/autoload.el" (23288 273
;;;;;;  528339 665000))
;;; Generated autoloads from ../modules/tools/impatient-mode/autoload.el

(autoload '+impatient-mode/toggle "../modules/tools/impatient-mode/autoload" "\
TODO

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../modules/tools/impatient-mode/autoload" '("+impatient-mode--cleanup-impatient-mode")))

;;;***

;;;### (autoloads nil "../modules/tools/make/autoload" "../modules/tools/make/autoload.el"
;;;;;;  (23288 273 529597 768000))
;;; Generated autoloads from ../modules/tools/make/autoload.el

(autoload '+make/run "../modules/tools/make/autoload" "\
Run a make task in the current project.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../modules/tools/neotree/autoload" "../modules/tools/neotree/autoload.el"
;;;;;;  (23288 273 530400 775000))
;;; Generated autoloads from ../modules/tools/neotree/autoload.el

(autoload '+neotree/toggle "../modules/tools/neotree/autoload" "\
Toggle the neotree window.

\(fn)" t nil)

(autoload '+neotree/collapse-or-up "../modules/tools/neotree/autoload" "\
Collapse an expanded directory node or go to the parent node.

\(fn)" t nil)

(autoload '+neotree/collapse "../modules/tools/neotree/autoload" "\
Collapse a neotree node.

\(fn)" t nil)

(autoload '+neotree/expand-or-open "../modules/tools/neotree/autoload" "\
Expand or open a neotree node.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../modules/tools/password-store/autoload"
;;;;;;  "../modules/tools/password-store/autoload.el" (23288 273
;;;;;;  531454 162000))
;;; Generated autoloads from ../modules/tools/password-store/autoload.el

(autoload '+pass/open "../modules/tools/password-store/autoload" "\


\(fn)" t nil)

(defalias '+pass--get-entry (if (featurep 'auth-store-pass) #'auth-source-pass-parse-entry #'auth-pass-parse-entry))

(autoload '+pass-get-field "../modules/tools/password-store/autoload" "\


\(fn ENTRY FIELDS)" nil nil)

(autoload '+pass-get-user "../modules/tools/password-store/autoload" "\


\(fn ENTRY)" nil nil)

(autoload '+pass-get-secret "../modules/tools/password-store/autoload" "\


\(fn ENTRY)" nil nil)

(autoload '+pass/ivy "../modules/tools/password-store/autoload" "\


\(fn &optional BROWSE-URL)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../modules/tools/password-store/autoload" '("+pass-ivy-action--")))

;;;***

;;;### (autoloads nil "../modules/tools/term/autoload" "../modules/tools/term/autoload.el"
;;;;;;  (23288 273 533496 764000))
;;; Generated autoloads from ../modules/tools/term/autoload.el

(autoload '+term/open "../modules/tools/term/autoload" "\
Open a terminal buffer in the current window. If PROJECT-ROOT (C-u) is
non-nil, cd into the current project's root.

\(fn &optional PROJECT-ROOT)" t nil)

(autoload '+term/open-popup "../modules/tools/term/autoload" "\
Open a terminal popup window. If PROJECT-ROOT (C-u) is non-nil, cd into the
current project's root.

\(fn &optional PROJECT-ROOT)" t nil)

(autoload '+term/open-popup-in-project "../modules/tools/term/autoload" "\
Open a terminal popup window in the root of the current project.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../modules/tools/upload/autoload" "../modules/tools/upload/autoload.el"
;;;;;;  (23288 273 534739 668000))
;;; Generated autoloads from ../modules/tools/upload/autoload.el

(autoload '+upload/local "../modules/tools/upload/autoload" "\
TODO

\(fn &optional FORCE-P)" t nil)

(autoload '+upload/remote-download "../modules/tools/upload/autoload" "\
TODO

\(fn)" t nil)

(autoload '+upload/diff "../modules/tools/upload/autoload" "\
TODO

\(fn)" t nil)

(autoload '+upload/browse "../modules/tools/upload/autoload" "\
TODO

\(fn)" t nil)

(autoload '+upload/check-remote "../modules/tools/upload/autoload" "\
TODO

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../modules/ui/hl-todo/autoload" "../modules/ui/hl-todo/autoload.el"
;;;;;;  (23288 273 540203 632000))
;;; Generated autoloads from ../modules/ui/hl-todo/autoload.el

(autoload '+hl-todo|use-face-detection "../modules/ui/hl-todo/autoload" "\
Use a different, more primitive method of locating todo keywords.

This is useful for major modes that don't use or have a valid syntax-table entry
for comment start/end characters.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "../modules/ui/nav-flash/autoload" "../modules/ui/nav-flash/autoload.el"
;;;;;;  (23288 273 541163 863000))
;;; Generated autoloads from ../modules/ui/nav-flash/autoload.el

(autoload '+doom*blink-cursor-maybe "../modules/ui/nav-flash/autoload" "\
Blink current line if the window has moved.

\(fn ORIG-FN &rest ARGS)" nil nil)

(autoload '+doom/blink-cursor "../modules/ui/nav-flash/autoload" "\
Blink current line using `nav-flash'.

\(fn &rest _)" t nil)

;;;***

(provide 'autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; autoloads.el ends here
