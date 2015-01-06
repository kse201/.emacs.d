;; init.el
;;
;;;------------------------------
(eval-when-compile (require 'cl))
(require 'cl)

;; local init.el
(when (file-exists-p "~/.emacs.d/local.el")
  (autoload 'local "~/.emacs.d/local.el")
  )

;; Language.
(set-language-environment 'Japanese)
;; Coding system.
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;; fileformant
(when (eq system-type 'darwin)
  (require 'ucs-normalize)
  (setq file-name-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8))
(when (eq system-type 'windows-nt)
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932))

(require 'server)
(unless (server-running-p)
  (server-start))
;;; ------------------------------
;;; @ Function
;;; open inits
(defun edit-init ()
  "edit init.el"
  (interactive)
  (find-file "~/.emacs.d"))
;;; open scratch
(defun edit-scratch ()
  "edit *scratch*"
  (interactive)
  (switch-to-buffer "*scratch*"))

;;; function for add load-path
(defun add-to-load-path (&rest paths)
  "define the function which add load-path"
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; easy key-bind registeration
;; http://pod.hatenablog.com/entry/2012/12/10/204538
(defun define-many-keys (key-map key-table)
  "easy key-bind registeration"
  (loop for (key . cmd) in key-table
        do (define-key key-map (read-kbd-macro key) cmd)))

(let ((default-directory "~/.emacs.d/elisp/"))
  (setq load-path (cons default-directory load-path))
  (normal-top-level-add-subdirs-to-load-path))

(defun jbr-init ()
  "Called from term-setup-hook after the default
  terminal setup is
  done or directly from startup if term-setup-hook not
  used.  The value
  0xF030 is the command for maximizing a window."
  (interactive)
  (w32-send-sys-command #xf030)
  (ecb-redraw-layout)
  (calendar))

;; nicely window splitting
(defun good-split-window ()
  "nicely window splitting"
  (interactive)
  (if (< (window-width) (* (window-height) 1.5) )
      (split-window-vertically)
    (split-window-horizontally)))

(defun my-other-window ()
  "Auto resize window when 'other-window"
  (interactive)
  (other-window 1)
  (let (( max-width (truncate (* (screen-width) 0.5))))
    (if (< (window-width) max-width)
        (enlarge-window-horizontally (- max-width (window-width))))))

(defun move-to-mark ()
  "move the marked place"
  (interactive)
  (let ((pos (point)))
    (goto-char (mark))
    (push-mark pos)))

;;; Network configuration
;; proxy setting
;; ref: http://e-arrows.sakura.ne.jp/2010/12/emacs-anywhere.html
(defun machine-ip-address (dev)
  "Return IP address of a network device."
  (let ((info (network-interface-info dev)))
    (if info
        (format-network-address (car info) t))))
(defvar *network-interface-names* '("en1" "wlan0")
  "Candidates for the network devices.")

(defun officep ()
  "Am I in the office? If I am in the office, my IP address must start with '10.x.x.x'."
  (let ((ip (some #'machine-ip-address *network-interface-names*)))
    (and ip
         (eq 0 (string-match "^10\\." ip)))))
(defun proxy-settting ()
  (interactive)
  (if (officep)
      (progn
        (setq url-proxy-services '(("http" . "10.0.58.8:8080")))
                                        ;(setq w3m-command-arguments
                                        ;     (nconc w3m-command-arguments
                                        ;     '("-o" "http_proxy=http://10.0.58.8:8080/")))
        )
    (progn
      (setq url-proxy-services nil))))
;; move real line-head
;; http://e-arrows.sakura.ne.jp/2010/02/vim-to-emacs.html
(defun my-beginning-of-indented-line (current-point)
  (interactive "d")
  (if (string-match
       "^[ \t]+$"
       (save-excursion
         (buffer-substring-no-properties
          (progn (beginning-of-line) (point))
          current-point)))
      (beginning-of-line)
    (back-to-indentation)))
;; DONT delete *scratch* buffer
(defun my-make-scratch (&optional arg)
  (interactive)
  (progn
    (set-buffer (get-buffer-create "*scratch*"))
    (funcall initial-major-mode)
    (erase-buffer)
    (when (and initial-scratch-message (not inhibit-startup-message))
      (insert initial-scratch-message))
    (or arg (progn (setq arg 0)
                   (switch-to-buffer "*scratch*")))
    (cond ((= arg 0) (message "*scratch* is cleared up."))
          ((= arg 1) (message "another *scratch* is created")))))
(add-hook 'kill-buffer-query-functions
    (lambda ()
            (if (string= "*scratch*" (buffer-name))
                (progn (my-make-scratch 0) nil)
              t)))

(add-hook 'after-save-hook
          (lambda ()
            (unless (member (get-buffer "*scratch*") (buffer-list))
              (my-make-scratch 1))))
;; non beep
(defun my-bell-function ()
  (unless (memq this-command
                '(isearch-abort abort-recursive-edit exit-minibuffer
                                keyboard-quit mwheel-scroll down up next-line previous-line
                                backward-char forward-char))
    (ding)))

(defun open-current-dir-with-finder ()
  "open current file's directory"
  (interactive)
  (shell-command (concat "open .")))

;;; http://shibayu36.hatenablog.com/entry/2012/12/18/161455
(defun split-window-vertically-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-vertically)
    (progn
      (split-window-vertically
       (- (window-height) (/ (window-height) num_wins)))
      (split-window-vertically-n (- num_wins 1)))))
(defun split-window-horizontally-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-horizontally)
    (progn
      (split-window-horizontally
       (- (window-width) (/ (window-width) num_wins)))
      (split-window-horizontally-n (- num_wins 1)))))
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (if (>= (window-body-width) 270)
        (split-window-horizontally-n 3)
      (split-window-horizontally)))
  (other-window 1))

;;; http://d.hatena.ne.jp/yascentur/20110621/1308585547
(defun split-window-vertically-n (num_wins)
  "vertical splitting the window N divide "
  (interactive "p")
  (if (= num_wins 2)
      (split-window-vertically)
    (progn
      (split-window-vertically
       (- (window-height) (/ (window-height) num_wins)))
      (split-window-vertically-n (- num_wins 1)))))
(defun split-window-horizontally-n (num_wins)
  "horizontal splitting the window N divide "
  (interactive "p")
  (if (= num_wins 2)
      (split-window-horizontally)
    (progn
      (split-window-horizontally
       (- (window-width) (/ (window-width) num_wins)))
      (split-window-horizontally-n (- num_wins 1)))))

;; http://qiita.com/items/3e6fb470e6f80bae046e
(defun package-require (feature &optional filename packagename noerror)
  "`require' の代わりに使う関数。
PACKAGENAME(or FEATURE) が未インストール時は、`require' する前に
`package-install' によるパッケージインストールを試みる。
NOERROR が non-nil ならば、PACKAGENAME(or FEATURE) が存在しなかったり、
`require' が失敗した時に `error' ではなく、nil を返す。(`require' の第三引数相当の挙動)"
  (unless package--initialized (package-initialize))
  (unless package-archive-contents (package-refresh-contents))
  (let ((pname (or packagename feature)))
    (if (assq pname package-archive-contents)
        (let nil
          (unless (package-installed-p pname)
            (unless package-archive-contents (package-refresh-contents))
            (package-install pname))
          (or (require feature filename t)
              (if noerror nil
                (error "Package `%s' does not provide the feature `%s'"
                       (symbol-name pname) (symbol-name feature)))))
      (if noerror nil
        (error "Package `%s' is not available for installation"
               (symbol-name feature))))))

;; http://qiita.com/items/61b8eeac2ebcf5993419
(global-set-key (kbd "M-u") 'camel-to-snake-backward-word)
(defun camel-to-snake-backward-word ()
  (interactive)
  (let ((case-fold-search nil)
        (s (buffer-substring
            (point) (save-excursion (forward-word -1) (point)))))
    (delete-region (point) (progn (forward-word -1) (point)))
    (insert (funcall (if (= (string-to-char s) (downcase (string-to-char s)))
                         'downcase 'upcase)
                     (replace-regexp-in-string
                      "\\([A-Z]\\)" "_\\1"
                      (store-substring s 0 (downcase (string-to-char s))))))))
;;; ------------------------------------------------------------------

(add-to-load-path "conf" "public_repos" "elpa" "elisp" "themes")
;;; @ auto-install
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/")
  ;; install-elisp.el compatible mode
  (auto-install-compatibility-setup)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;;; @ init-loader
(require 'init-loader "~/.emacs.d/elisp/init-loader")
(when (require 'init-loader nil t)
  (init-loader-load "~/.emacs.d/inits")
  (defun init-loader-re-load (re dir &optional sort)
    (let ((load-path (cons dir load-path)))
      (dolist (el (init-loader--re-load-files re dir sort))
        (condition-case e
            (let ((time (car (benchmark-run (load (file-name-sans-extension el))))))
              (init-loader-log (format "loaded %s. %s" (locate-library el) time)))
          (error
           (init-loader-error-log (format "%s. %s" (locate-library el) (error-message-string e)))
           ))))))

;; http://qiita.com/items/8f1d3342180d42ad9f78
;;; Get current path and put it to clipboard
(defun put-current-path-to-clipboard ()
  (interactive)
  (let ((file-path buffer-file-name)
        (dir-path default-directory))
    (cond (file-path
           (kill-new (expand-file-name file-path))
           (message "This file path is on the clipboard!"))
          (dir-path
           (kill-new (expand-file-name dir-path))
           (message "This directory path is on the clipboard!"))
          (t
           (error-message-string "Fail to get path name.")
           ))))
(global-set-key (kbd "C-c C-c p") 'put-current-path-to-clipboard)
;;; ------------------------------
(global-auto-revert-mode 1)
;;; C-x C-u/C-l upper / lower
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; auto spell check
(defun spell-check ()
  (setq-default flyspell-mode t)
  (setq ispell-dictionaryonary "american")
  (setq ispell-program-name "aspell")
  (setq truncate-lines nil)
  (setq truncate-partial-width-windows nil))

(defalias 'yes-or-no-p 'y-or-n-p)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(set-locale-environment nil)
(show-paren-mode 1)

;; http://d.hatena.ne.jp/syohex/20121225/1356449561
;; look command with auto-complete
(defun my/ac-look ()
  "`look' command with auto-completelook"
  (interactive)
  (unless (executable-find "look")
    (error "Please install `look' command"))
  (let ((word (thing-at-point 'word)))
    (unless word
      (error "not found word"))
    (let ((cmd (format "look %s" word)))
      (with-temp-buffer
        (call-process-shell-command cmd nil t)
        (split-string-and-unquote (buffer-string) "\n")))))

(defun ac-look ()
  (interactive)
  (let ((ac-menu-height 50)
        (ac-candidate-limit t))
    (auto-complete '(ac-source-look))))

(defvar ac-source-look
  '((candidates . my/ac-look)
    (requires . 2)))

(global-set-key (kbd "C-M-l") 'ac-look)
;; ---------------------------------

;; search by selected word
(defadvice isearch-mode
  (around isearch-mode-default-string
          (forward &optional regexp op-fun recursive-edit word-p) activate)
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        ad-do-it
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    ad-do-it))

(defadvice save-buffers-kill-terminal (before my-save-buffers-kill-terminal activate)
  (when (process-list)
    (dolist (p (process-list))
      (set-process-query-on-exit-flag p nil))))
(setq gc-cons-threshold (* 5242880 2))
(setq message-log-max 10000)

;;; ------------------------------
;;; @ etc settings
(setq shell-file-name "/bin/zsh")
(setq enable-recursice-minibuffers t)

(setq use-dialog-box nil)
(defalias 'message-box 'message)
(setq history-length 10000)
(setq echo-keystrokes 0.1)
(setq large-file-worning-threshold (* 25 1024 1024))
(defadvice abort-recursive-edit (before minibuffer-save activate)
  (when (eq (selected-window) (active-minibuffer-window))
    (add-to-history minibuffer-history-variable (minibuffer-contents))))

(setq max-specpdl-size 6000)
(setq max-lisp-eval-depth 1000)
(savehist-mode 1)

;;; @ key bind ------------------------------ 

(auto-insert-mode t)

;;; ido.el
(ido-mode 1)
(ido-everywhere 1)

(setq eval-expression-print-length nil)

(eval-when-compile
  (require 'cl))

(setq require-final-newline t)
(setq next-line-add-newlines nil)
(setq delete-auto-save-files t)
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(if (string-match "^23\." emacs-version)
    (partial-completion-mode t))
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        ))

(load "saveplace")
(setq-default save-place t)

;; ignore byte-complie warnings
(setq byte-compile-warnings '(not nresolved
                                  free-vars
                                  callargs
                                  redefine
                                  obsolete
                                  noruntime
                                  cl-funcitons
                                  interactive-only))

(windmove-default-keybindings 'super)

;; C-c <left> / C-c <right>
(when (fboundp 'winner-mode)
  (winner-mode t))

(setq ring-bell-function 'my-bell-function)
(setq ring-bell-function 'ignore)

;; http://qiita.com/items/f0db094fde6640143f42
(if (file-directory-p (expand-file-name "~/bin"))
    (progn
      (add-to-list 'exec-path (expand-file-name "~/bin"))
      (setenv "PATH" (mapconcat 'identity exec-path ":"))))

(global-auto-revert-mode 1)

(custom-set-variables
 '(delete-by-moving-to-trash t)
 '(trash-directory "~/.Trash"))

(add-to-list 'Info-directory-list "~/.emacs.d/info")

(put 'set-goal-column 'disabled nil)
