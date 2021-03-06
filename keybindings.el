





;; When started in daemon mode, window-system will be nil (not 'w32), but
;; system-type will be 'cygwin, so we just assume that means we are using the
;; Win32 GUI version.
(when (equal system-type 'cygwin)
  (pd-log "On Cygwin, make APPS function as a true hyper key.")
  (setq w32-pass-apps-to-system nil w32-apps-modifier 'hyper)
  ;; The following helps in the terminal, however APPS still does not function
  ;; a true hyper key in the terminal because you cannot keep it pressed while
  ;; repeatedly pressing another key. See also pd-bind-key in this file.
  (define-key local-function-key-map (kbd "<print>") 'event-apply-hyper-modifier))


;; ******************* Global Function keys ********************

;; ******************* Letter/main section keys ********************
;; (define-key global-map (kbd "C-; SPC")   'helm-all-mark-rings)
;; (define-key global-map (kbd "C-; a")     'helm-apropos)
;; (define-key global-map (kbd "C-; c")     'helm-colors)
;; (define-key global-map (kbd "C-; f")     'helm-find)
;; (define-key global-map (kbd "C-; i")     'insert-char)
;; (define-key global-map (kbd "C-; m")     'helm-man-woman)
;; (define-key global-map (kbd "C-; o")     'helm-occur)
;; (define-key global-map (kbd "C-; r")     'helm-all-mark-rings)
;; (define-key global-map (kbd "C-; s")     'helm-semantic-or-imenu)
;; (define-key global-map (kbd "M-#")       'hydra-windows/body)
;; (define-key global-map (kbd "C-\\")      'hs-toggle-hiding)
;; (define-key global-map (kbd "C-|")       'hs-show-all)

;; ;; (define-key global-map (kbd "M-[")       'backward-sexp)  Screws up in terminal Emacs
;; ;; fine-key global-map (kbd "M-]")       'forward-sexp)
;; (define-key global-map (kbd "M-{")       'endless/backward-paragraph)     ;; Replace standard bindings for bp and fp with better versions.
;; (define-key global-map (kbd "M-}")       'endless/forward-paragraph)
;; (define-key global-map (kbd "C-x C-t")   'pd-ansi-term)



(defun pd-bind-key (keyseq func)
  "Helper function to bind keys to both <apps> and H-."
  (let ((e1 (concat "<print> " keyseq))
        (e2 (format "H-%s%s" (substring keyseq 0 1)
                    (if (equal 1 (length keyseq))
                        ""
                      (concat " " (substring keyseq 1 2)))))
        )
    (define-key global-map (kbd e1) func)
    (define-key global-map (kbd e2) func)))

;; Consider arrow keys too.
;; Want for bookmarks, macros, ggtags, rectangles?
;; (pd-bind-key "b"  'cycle-buffer-backward-permissive)
;; (pd-bind-key "d"  'dired-jump)
;; (pd-bind-key "f"  'cycle-buffer-permissive)
;; (pd-bind-key "g"  'magit-status)
;; (pd-bind-key "o"  'pd-duplicate-line-or-region)
;; (pd-bind-key "p"  'pd-cleanup-programming-buffer)
;; (pd-bind-key "s"  'pd-sort-paragraph-dwim)
;; (pd-bind-key "t"  'pd-ansi-term)
;; (pd-bind-key "w"  'pd-copy-current-line)
;;(pd-log "Hyper/apps keys defined.")

;; (pd-bind-key "rb" 'pd-revert-buffer)
;; (pd-bind-key "rj" 'jump-to-register)
;; (pd-bind-key "rp" 'point-to-register)
;; (pd-bind-key "rw" 'window-configuration-to-register)

;; (define-key global-map (kbd "C-# au") 'pd-cpp-add-using)
;; (define-key global-map (kbd "C-# cl") 'pd-copy-current-line)
;; (define-key global-map (kbd "C-# cp") 'pd-cleanup-programming-buffer)
;; (define-key global-map (kbd "C-# df") 'pd-delete-file-and-buffer)
;; (define-key global-map (kbd "C-# dl") 'pd-duplicate-line-or-region)
;; (define-key global-map (kbd "C-# dw") 'delete-trailing-whitespace)
;; (define-key global-map (kbd "C-# ib") 'pd-indent-buffer)
;; (define-key global-map (kbd "C-# ir") 'indent-region)
;; (define-key global-map (kbd "C-# jr") 'jump-to-register)
;; (define-key global-map (kbd "C-# mb") 'mark-whole-buffer)
;; (define-key global-map (kbd "C-# mf") 'mark-defun)
;; (define-key global-map (kbd "C-# mp") 'mark-paragraph)
;; (define-key global-map (kbd "C-# pr") 'point-to-register)
;; (define-key global-map (kbd "C-# rb") 'pd-revert-buffer)
;; (define-key global-map (kbd "C-# rf") 'pd-rename-file-and-buffer)
;; (define-key global-map (kbd "C-# sp") 'pd-sort-paragraph-dwim)
;; (define-key global-map (kbd "C-# ut") 'pd-untabify-buffer)
;; (define-key global-map (kbd "C-# wr") 'window-configuration-to-register)
;; (pd-log "C-# mnemonic keys defined.")

;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
;; (define-key helm-map (kbd "C-i")   'helm-execute-persistent-action) ; make TAB works in terminal
;; (define-key helm-map (kbd "C-z")   'helm-select-action) ; list actions using C-z
;; (pd-log "Helm mode keys defined.")

;; ******************* C/C++ mode keys ********************
;; Create a keymap with Visual Studio compatible keymappings.
;; See http://ergoemacs.org/emacs/elisp_menu_for_major_mode.html
;; for what is going on here.
;; (defvar pd-vs-minor-mode-map nil "Keymap for Visual Studio compatibility.")

;; ;; TODO: We should really setup the gud commands only in a gud-mode-hook.

;; (when (not pd-vs-minor-mode-map)
;;   (setq pd-vs-minor-mode-map (make-sparse-keymap))
;;   ;;  (define-key pd-vs-minor-mode-map (kbd "<f5>") 'gud-run) ; continue (gdb command = continue)
;;   ;;   C-F5 = run without debugging
;;   ;;   S-F5 = stop debugging
;;   ;;  CS-F5 = restart
;;   (define-key pd-vs-minor-mode-map (kbd "<f6>") 'pd-compile-without-confirmation)
;;   (define-key pd-vs-minor-mode-map (kbd "<S-f6>") 'pd-compile-clean-one-shot)
;;   (define-key pd-vs-minor-mode-map (kbd "<C-f6>") 'compile) ; make -k, the original compile command.
;;   (define-key pd-vs-minor-mode-map (kbd "<f7>") 'ff-find-other-file) ; View.ToggleDesigner in VS.
;;   (define-key pd-vs-minor-mode-map (kbd "<S-f7>") (lambda () (interactive) (ff-find-other-file t))) ; in a split window.
;;   (define-key pd-vs-minor-mode-map (kbd "<f8>") 'next-error)
;;   (define-key pd-vs-minor-mode-map (kbd "<S-f8>") 'previous-error)
;;   ;;(define-key pd-vs-minor-mode-map (kbd "<f9>") 'gud-break) ; toggle breakpoint (gdb command = break)
;;   ;;  CS-F9 = delete all breakpoints = typing d.
;;   ;; (define-key pd-vs-minor-mode-map (kbd "<f10>") 'gud-next)  ; step over (gdb command = next)
;;   ;; (define-key pd-vs-minor-mode-map (kbd "<C-f10>") 'gud-until)  ; run to cursor (gdb command = advance)
;;   ;; (define-key pd-vs-minor-mode-map (kbd "<CS-f10>") 'gud-jump)  ; set next statement (gdb command = jump)
;;   ;; (define-key pd-vs-minor-mode-map (kbd "<f11>") 'gud-step)  ; step in (gdb command = step)
;;   ;; (define-key pd-vs-minor-mode-map (kbd "<S-f11>") 'gud-finish)  ; step out (gdb command = finish)
;;   ;;    F12 = go to definition
;;   ;; "  C-Brk = cancel current build
;;   )

;; (define-minor-mode pd-vs-minor-mode
;;   "A minor mode to establish Visual Studio compatible key mappings."
;;   nil " vs" 'pd-vs-minor-mode-map)

;; (add-hook 'c-mode-common-hook (lambda () (pd-vs-minor-mode 1)))
;; (add-hook 'compilation-mode-hook (lambda () (pd-vs-minor-mode 1)))
;; (add-hook 'gdb-mode-hook (lambda () (tool-bar-mode 1)))

;; (pd-log "pd-vs-minor-mode defined and enabled for C, gdb and compilation.")


;; (pd-log-loading-complete)
;;(provide 'pd-keys)


;; Emacs understands the following modifiers:
;;    M- (meta)      Alt on my keyboard
;;    C- (control)   Ctrl keys on my keyboard
;;    S- (shift)     Shift keys on my keyboard
;;    s- (super)     Note lower case 's'. On Linux, this is usually bound to
;;                   the Windows key automatically. On Windows, the Windows key
;;                   is monopolized by Windows (see below).
;;    H- (hyper)     Not usually bound to any key by default.
;;    A- (alt)       Not usually bound to any key by default. Note that the
;;                   Alt key sends Meta! Best to avoid A- mappings, use H- and
;;                   s- instead.
;;
;; To find out what the name of a key is, the easiest way is to type the key,
;; then type C-h l (view-lossage), which shows you the last 100 keys pressed.
;; See unbound.el for how to find unbound keys.
;;
;; Keynames for a PC keyboard
;; ==========================
;; See https://www.gnu.org/software/emacs/manual/html_node/emacs/Function-Keys.html#Function-Keys
;;
;; Function keys:
;;    [f5] (must be in lower case, f1 is help by default)
;; Cursor arrow keys:
;;    [left], [up], [right], [down]
;; The small pad between qwerty and numeric:
;;    [home], [end], [next (is PgDn)], [prior (is PgUp)], [insert], [delete]
;; Numeric keypad arithmetic operators:
;;    [kp-add], [kp-subtract], [kp-multiply], [kp-divide]
;; Numbers on the numeric pad:
;;    [kp-0], [kp-1] ... [kp-9]
;; The other 2 keys on the numeric pad:
;;    [kp-enter], [kp-decimal]
;; Keypad keys available when NUM LOCK is pressed:
;;    [kp-home], [kp-end], [kp-next], [kp-prior]
;;    [kp-insert], [kp-delete]
;;    [kp-left], [kp-up], [kp-right], [kp-down]
;;
;; "apps" is usually known as the "menu" key, next to Ctrl-right.
;; It can also be known as "print" on Cygin, because the sequence ESC [ 2 9 ~
;; is mapped to that in lisp/term/xterm/el.gz.
;;
;; Key Stealing
;; ============
;; Key stealing is when a key combination is grabbed by the operating system or
;; the window manager before Emacs even sees the keypress, which makes it
;; impossible to bind the key in Emacs. On Linux, it is usually the window
;; manager that is grabbing the key. Use the window manager's tools to free up
;; these keys so that they can be used in Emacs.
;;
;; On Windows, this happens with the two Windows keys: many WinKey+letter
;; combinations are reserved, and the count goes up with each release of
;; Windows. So these keys are unavailable *even if* you use the
;; w32-pass-lwindow-to-system technique described below. To make these key
;; combinations available a registry hack must be used to disable all the
;; default bindings - use a .reg file with this text:
;;
;; Windows Registry Editor Version 5.00
;;
;;[HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer]
;;"NoWinKeys"=dword:00000001
;;
;; BUT DO NOT DO THIS. It makes it hard to work in a business environment with
;; projectors, etc.
;;
;; The apps key used to be stolen in mintty, but the commit
;; 429cb080e6bfee6136227ca5d41ea61494b36c2d on 9 Nov 15 made it possible to pass
;; apps through to the underlying program. See
;; http://emacs.stackexchange.com/questions/18245/making-terminal-emacs-treat-apps-aka-menu-key-as-super-modifier
;;
;; Recommendations
;; ===============
;; * Restrict repeatable keys (those that you might want to press several times
;;   quickly in succession) to C- and M-.
;; * Do not use the Windows keys.
;; * Make the apps/menu key send the "<apps>" leader.
;; * Do not use the super s- prefix.
;;
;; A full size keyboard is CTRL    WIN ALT SPACE ALTGR WIN APPS  CTRL
;; My work laptop is       CTRL FN WIN ALT SPACE ALTGR           CTRL
;; My home laptop is       CTRL FN WIN ALT SPACE ALTGR           CTRL
;; MK Disco is             CTRL    WIN ALT SPACE ALT   WIN FN    CTRL (FN is really APPS) This is standard US layout.
;; So the common set is    CTRL    WIN ALT SPACE ALTGR           CTRL
;;
;; Reserving WIN for Windows, we are left with
;;
;;                         C-          M-                        C-
;;
;; WIN is usually super in Linux, so super is a good modifier to bind keys to,
;; assuming we can make a proper super key in Windows. In Linux, it should be
;; possible to do this with xkb quite easily, see http://www.charvolant.org/%7Edoug/xkb/html/node5.html
;;
;; Some standard keybindings
;; =========================
;; C-c <letter>       ; always reserved for your own bindings
;; C-f, C-b, C-n, C-p : move one character or line
;; C-v, M-v           : forward, backward one screen
;; M-f, M-b           : forward, backward by 1 word
;; C-M-f, C-M-b       : forward/backward by 1 sexp
;; C-SPC              : set mark and activate region
;; C-SPC C-SPC        : just set the mark
;; C-x C-x            : exchange-point-and-mark
;; C-a, C-e           : beginning/end of a line
;; M-a, M-e           : beginning/end of a sentence
;; C-M-a, C-M-e       : beginning/end of defun
;; M-<, M->           : beginning/end of buffer
;; M-r                : move-to-window-line-top-bottom
;; C-d, M-d           : kill next character/word
;; Delback, M-Delback : kill prev char/word
;; C-k, M-k           : kill to end of line/sentence
;; C-M-v, C-M-V       : scroll other window down/up
;; C-o                : open-line
;; C-s, C-r           : isearch-forward/backward, M-s/r does regexps.
;; C-t, M-t, C-x C-t  : transpose chars/words/lines
;; C-_                : undo
;; f3, f4             : start, end/run keyboard macro
;; C-x (,  C-x )      : start, end/run keyboard macro
;; C-x `              : next-error
;; C-x <left>/<right> : previous-buffer and next-buffer
;; C-x 8 RET          : insert characters by name
;; M-$, M-%           : ispell-word, query-replace foo
;; M-\, M-^, M-z      : delete horz space, delete-indentation, zap-to-char
;; M-c, M-l, M-u      : capitalize/lower/upper word
;; M-m                : back-to-indentation
;; M-h                : mark-paragraph
;;
;; Techniques
;; ===============
;; How to unset a key globally:
;;   (global-unset-key "\C-x\C-v")
;;
;; Modify a mode-local map:
;;   (define-key lisp-mode-map "\C-xl"; 'make-symbolic-link)
;;
;; Modify a mode-local map after the mode has started (necessary for
;; some modes where the keymap is created after the mode has started):
;;   (add-hook 'texinfo-mode-hook
;;     '(lambda () (define-key texinfo-mode-map "\C-cp"
;;       'backward-paragraph) (define-key texinfo-mode-map "\C-cn"
;;       'forward-paragraph)) (...any command whatsoever here ...))
;;
;; Make Win/Apps keys send super/hyper etc.
;; (when (equal window-system 'w32)
;;   (setq w32-pass-lwindow-to-system nil
;;      w32-lwindow-modifier 'super))
;; (when (equal window-system 'w32)
;;   (setq w32-pass-rwindow-to-system nil
;;      w32-rwindow-modifier 'super))
;;
;; Mintty has been hacked by the maintainer to allow the APPS/MENU key to be
;; passed through to the underlying program. Adding the line "Key_Menu=29" to
;; the .minttyrc file causes terminal Emacs to see it as the "<print"> key. The
;; hack was on 2015-11-19 in commit 429cb080e6bfee6136227ca5d41ea61494b36c2d.
;; Given this hack, we can make APPS send the s- (super) prefix like this in
;; both W32 and terminal Emacs. Unfortunately, APPS still does not work like the
;; Alt or Control keys in the terminal, if you hold it down by itself you get
;; lots of input in the output buffer. It does work ok in GUI Emacs though.
;; (define-key global-map (kbd "s-h") (lambda () (interactive) (message "hello from menu key via s- prefix")))
;;
;; Alternatively, we can turn it into a leader key like this.
;; See http://ergoemacs.org/emacs/emacs_menu_app_keys.html
;; (if (equal system-type 'cygwin)
;;     (if (equal window-system 'w32)
;;         (setq w32-pass-apps-to-system nil
;;               w32-apps-modifier nil)
;;       ;; force all alternatives to <apps> so we can write one set of keybindings.
;;       (define-key key-translation-map (kbd "<print>") (kbd "<apps>"))
;;       (define-key key-translation-map (kbd "<menu>") (kbd "<apps>"))))
;;
;; (if (equal system-type 'gnu/linux)
;;     (define-key key-translation-map (kbd "<menu>") (kbd "<apps>")))
;;
;; ;; On X, e.g. in Mint, make the left Windows key, which is normally super,
;; ;; send hyper instead.
;; (setq x-super-keysym 'hyper)
;;
;; Over ssh to Linux, window-system is 'x and system-type is 'gnu/linux.
;;
;; Helpful links
;; =============
;; http://emacs.stackexchange.com/questions/1020/problems-with-keybindings-when-using-terminal
;; http://unix.stackexchange.com/questions/116629/how-do-keyboard-input-and-text-output-work/116630#116630
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Translation-Keymaps.html#Translation-Keymaps
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Modifier-Keys.html
;; http://stackoverflow.com/questions/10730775/emacs-create-key-modifier   (for faking using event-apply-hyper-modifier etc.)
;; http://ergoemacs.org/emacs/emacs_hyper_super_keys.html
;;
;; and especially
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Keyboard-Events.html#Keyboard-Events




;; How to add a key to a hydra/transient state.
;; See https://github.com/syl20bnr/spacemacs/blob/b7e51d70aa3fb81df2da6dc16d9652a002ba5e6b/layers/%2Bspacemacs/spacemacs-ui-visual/packages.el
;; (setq spacemacs-window-manipulation-transient-state-add-bindings
;;   '(("g" spacemacs/toggle-golden-ratio)))


;; ^ is a special char that has zero width in the resulting docstring.
;; In [_a_], we will lose two characters when rendered, hence the ^^ in the lines above.

(spacemacs|define-transient-state pd-window-manipulation
  :title "Phil's Window Manipulation Transient State."
  :doc (concat "M-arrow = select, M-S-arrow = move, S-arrow = resize. 0-9 = select window N.

Split^^              Buffers^^      Windows^^          Files^^         Other
─────^^───────────── ───────^^───── ───────^^───────── ─────^^──────── ─────^^──────────────────
[___] vertical       [_n_] next     [_c_] close        [_b_] helm mini [_u_] restore prev layout
[_V_] vert & follow  [_p_] previous [_o_] close others [_f_] helm find [_U_] restore next layout
[_|_] horizontal     [_k_] kill     [_r_] rotate fwd   [_d_] dired     [_F_] go to other frame
[_H_] horiz & follow [_s_] save     [_R_] rotate bwd   [_t_] terminal  [_g_] golden-ratio-mode")
  :bindings
  ;; Split
  ("_" split-window-below)
  ("v" split-window-below)
  ("V" split-window-below-and-focus)
  ("-" split-window-below-and-focus)
  ("|" split-window-right)
  ("h" split-window-right)
  ("H" split-window-right-and-focus)
  ("/" split-window-right-and-focus)
  ;; Buffers
  ("n" cycle-buffer-permissive)
  ("p" cycle-buffer-backward-permissive)
  ("k" kill-buffer)
  ("s" save-buffer)
  ;; Windows
  ("c" delete-window)
  ("o" delete-other-windows)
  ("r" spacemacs/rotate-windows)
  ("R" spacemacs/rotate-windows-backward)
  ;; Files
  ("b" helm-mini)
  ("f" helm-find-files)
  ("d" dired-jump)
  ("t" pd-ansi-term)
  ;; Other
  ("q" nil :exit t)
  ("u" winner-undo)
  ("U" winner-redo)
  ("F" other-frame)
  ("g" spacemacs/toggle-golden-ratio)
  ;; Select
  ("<down>"    evil-window-down)
  ("<M-down>"  evil-window-down)
  ("<up>"      evil-window-up)
  ("<M-up>"    evil-window-up)
  ("<left>"    evil-window-left)
  ("<M-left>"  evil-window-left)
  ("<right>"   evil-window-right)
  ("<M-right>" evil-window-right)
  ("0" select-window-0)
  ("1" select-window-1)
  ("2" select-window-2)
  ("3" select-window-3)
  ("4" select-window-4)
  ("5" select-window-5)
  ("6" select-window-6)
  ("7" select-window-7)
  ("8" select-window-8)
  ("9" select-window-9)
  ;; Move
  ("<M-S-down>"  evil-window-move-very-bottom)
  ("<M-S-up>"    evil-window-move-very-top)
  ("<M-S-left>"  evil-window-move-far-left)
  ("<M-S-right>" evil-window-move-far-right)
  ;; Resize
  ("[" spacemacs/shrink-window-horizontally)
  ("]" spacemacs/enlarge-window-horizontally)
  ("{" spacemacs/shrink-window)
  ("}" spacemacs/enlarge-window)
  ("<S-left>"  spacemacs/shrink-window-horizontally)
  ("<S-right>" spacemacs/enlarge-window-horizontally)
  ("<S-up>"    spacemacs/shrink-window)
  ("<S-down>"  spacemacs/enlarge-window)
  )

;; (setq spacemacs-pd-window-manipulation-transient-state-add-bindings
;; '(("g" spacemacs/toggle-golden-ratio)))

(spacemacs/set-leader-keys "w." 'spacemacs/pd-window-manipulation-transient-state/body)
