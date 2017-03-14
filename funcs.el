;;; See https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org#anatomy-of-a-layer

;;; This file contains all the defined functions used in the layer.
;;; This file is loaded after packages.el and before config.el.
;;; It is good practice to guard the definition of functions to make sure a package is actually used.
;;; (Supposedly, none of the built-in Spacemacs packages seem to bother.)

(defun pd-left-rotate (list)
  "Move the first element to the end of the list."
  (append (cdr list) (list (car list))))

(defun pd-right-rotate (list)
  "Move the last element to the front of the list."
  (append (last list) (butlast list)))

(defun wd-beginning-of-line (point)
  "Return the beginning of line position for the specified POINT.
If POINT is nil, return it for the current line."
  (if point
      (goto-char point))
  (point-at-bol))

(defun wd-end-of-line (point)
  "Return the end of line position for the specified POINT.
If POINT is nil, return it for the current line."
  (if point
      (goto-char point))
  (point-at-eol))

(defun pd-sort-lines (reverse beg end &optional cmp)
  "Sort lines in the region using the specified comparison function.

Pass non-nil for REVERSE to reverse the order, BEG and END are the two points
that specify the region to sort. CMP is a binary comparison predicate to be used
for ordering the lines, it will be passed two strings. You may pass nil, in
which case the function STRING< is used."
  (or cmp (setq cmp 'string<))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (sort-subr nil 'forward-line 'end-of-line nil nil
                 ;; We get passed a pair such as (700 . 800) for r1 and another for r2
                 (lambda (r1 r2)
                   (let (
                         (s1 (buffer-substring-no-properties (car r1) (cdr r1)))
                         (s2 (buffer-substring-no-properties (car r2) (cdr r2))))
                     ;; (message "Got s1=%s and s2=%s" s1 s2)
                     (funcall cmp s1 s2)))))))


(defun pd-utf8 ()
  "Inserts a utf-8 file coding marker."
  (interactive)
  (insert "-*- coding: utf-8 -*-"))

(defun pd-timestamp()
  "Inserts an Emacs timestamp at point."
  (interactive)
  (insert "Time-stamp: <>"))

(defun pd-untabify-buffer ()
  "Run untabify on the entire buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun pd-indent-buffer ()
  "Run indent on the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun pd-group-number (num &optional size char)
  "Format NUM as string grouped to SIZE with CHAR."
  ;; Based on code for `math-group-float' in calc-ext.el
  (let* ((size (or size 3))
         (char (or char ","))
         (str (if (stringp num)
                  num
                (number-to-string num)))
         ;; omitting any trailing non-digit chars
         ;; NOTE: Calc supports BASE up to 36 (26 letters and 10 digits ;)
         (pt (or (string-match "[^0-9a-zA-Z]" str) (length str))))
    (while (> pt size)
      (setq str (concat (substring str 0 (- pt size))
                        char
                        (substring str (- pt size)))
            pt (- pt size)))
    str))

(defun pd-sort-paragraph-dwim (&optional special-c-sort)
  "Sorts the current paragraph and leaves point after the last line.

If a prefix argument is supplied, and if the paragraph starts
with '#include', then it is sorted specially to ensure that
system library includes such as #include <...> appear before
#include \"...\"."
  (interactive "P")
  (let* ((bounds (bounds-of-thing-at-point 'paragraph))
         (beg (car bounds))
         (end (cdr bounds)))
    (when special-c-sort
      (setq start (buffer-substring-no-properties (+ 1 beg) (+ beg 9)))
      (unless (string= start "#include")
        (setq special-c-sort nil)))
    (if special-c-sort
        (pd-sort-lines nil beg end 'pd-c-cmp-includes)
      (sort-lines nil beg end))
    (goto-char end)))

(defun pd-find-first-paragraph-starting (s)
  "Returns the (BEG . END) point of the first paragraph, if any, that starts
with the specified string S.

For some reason this function tends to return leading whitespace. I consider
this to be a bug."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((p (re-search-forward s nil t)))
      (when p
        (bounds-of-thing-at-point 'paragraph)))))

(defun pd-cleanup-programming-buffer ()
  "Reindent, untabify, delete trailing whitespace, sort c includes and C++
usings; recommended for programming modes only.

Also not recommended when working with other people's code because it will
re-indent the entire buffer."
  (interactive)
  (pd-indent-buffer)
  (pd-untabify-buffer)
  (delete-trailing-whitespace)
  (when (or (derived-mode-p 'c-mode) (derived-mode-p 'c++-mode))
    (pd-c-sort-includes))
  (when (derived-mode-p 'c++-mode)
    (pd-cpp-sort-usings))
  (message "Reindent, untabify, delete trailing whitespace."))

(defun pd-compile-without-confirmation ()
  "Runs last compilation without asking for confirmation."
  (interactive)
  (save-window-excursion
    (compile compile-command))
  (pop-to-buffer (get-buffer "*compilation*")))

(defun pd-compile-clean-one-shot ()
  "Runs make clean, but restores compile command after it."
  (interactive)
  (let (oldcc compile-command)
    (save-window-excursion
      (compile "make clean"))
    (pop-to-buffer (get-buffer "*compilation*"))
    (setq compile-command oldcc)))

(defun pd-hide-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  ;; From http://stackoverflow.com/questions/730751/hiding-m-in-emacs
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun pd-turn-off-trailing-whitespace-display ()
  "Turns off the buffer-local variable show-trailing-whitespace."
  (interactive)
  (set-variable 'show-trailing-whitespace nil))

(defmacro pd-run-once-when-idle (idle-secs &rest body)
  "Run the form BODY once, after Emacs has been idle for IDLE-SECS."
  `(run-with-idle-timer ,idle-secs nil (lambda () ,@body)))

(defun pd-focus-in-hook-execute ()
  "A function that is called once when the FOCUS-IN-HOOK is executed."
  (when (> (length pd-focus-in-hook) 0)
    ;; (message "pd-focus-in-hook-execute: Executing %d pd-focus-in-hook functions." (length pd-focus-in-hook))
    (run-hooks 'pd-focus-in-hook)
    ;; (message "pd-focus-in-hook-execute: Execution complete.")
    (remove-hook 'focus-in-hook 'pd-focus-in-hook-execute)))

(defun pd-log-sys-info ()
  "Logs system info."
  (message "Host = %s, system-type=%s, window-system=%s" system-name system-type window-system))

(defun pd-log (message)
  "Logs a message with incremental time since last message and
source file name as a prefix."
  (interactive)
  (when (equal pd-first-log-time 0)
    (setq pd-last-log-time (float-time))
    (setq pd-first-log-time pd-last-log-time))
  (let ((cum-secs (- (float-time) pd-first-log-time))
        (inc-secs (- (float-time) pd-last-log-time)))
    (message "%9.5fs : %8.5fs inc. : %s : %s" cum-secs inc-secs (file-name-nondirectory load-file-name) message))
  (setq pd-last-log-time (float-time)))

(defun pd-log-requires-complete ()
  "Logs a 'Requires complete' message for a file."
  (interactive)
  (pd-log "Requires complete."))

(defun pd-log-loading-complete ()
  "Logs a 'Loading complete' message for a file."
  (interactive)
  (pd-log "Loading complete."))

(defun pd-get-full-path (relative-path)
  "Return the full path of relative-path, relative to caller's file location.
This does not always work, if files load other files you can be relative to
the original file. In other words, it does not give the path of a lisp file
on disk.

Example: If you have this line
 (pd-get-full-path \"../xyz.el\")
in the file at
 /home/jane/emacs/emacs_lib.el
then the return value is
 /home/jane/xyz.el
Regardless how or where emacs_lib.el is called.

A call (pd-get-full-path \"\") will get the directory of the
executing lisp file.

This function solves 2 problems.

If you have file A, that calls the `load' on a file at B, and B
calls `load' on file C using a relative path, then Emacs will
complain about unable to find C. Because, emacs does not switch
current directory with `load'.

To solve this problem, when your code only knows the relative
path of another file C, you can use the variable `load-file-name'
to get the current file's full path, then use that with the
relative path to get a full path of the file you are interested.

To know the current file's full path, emacs has 2 ways:
`load-file-name' and `buffer-file-name'. If the file is loaded by
`load', then `load-file-name' works but `buffer-file-name'
doesn't. If the file is called by `eval-buffer', then
`load-file-name' is nil. You want to be able to get the current
file's full path regardless the file is run by `load' or
interactively by `eval-buffer'."
  (expand-file-name "" (concat (file-name-directory (or load-file-name buffer-file-name)) relative-path)))
