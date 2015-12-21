;; here be PACKAGES
;; package paths and default dir
(setq homedir-file "~/.emacs.d/homedir.el")
(load homedir-file)

(setq spell-file "~/.emacs.d/spell.el")
(load spell-file)

;; load paths
(add-to-list 'load-path "~/.emacs.d/org-mode/lisp")
(add-to-list 'load-path "~/.emacs.d/org-mode/contrib/lisp")
(add-to-list 'load-path "~/.emacs.d/elpa")
(add-to-list 'load-path "~/.emacs.d/elpa/ace-jump-mode")
(add-to-list 'load-path "~/.emacs.d/elpa/adoc-mode/")
(add-to-list 'load-path "~/.emacs.d/elpa/markup-faces/")
(require 'markup-faces)
(require 'adoc-mode)
(require 'undo-tree)
(require 'nav)
(require 'ido)
(require 'yafolding)
(require 'back-button)
(require 'saveplace)
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(require 'package)
(add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
(package-initialize)

;; helm mode needs to go after package-initialize
(require 'helm-config)
;; (setq helm-split-window-in-side-p t)
(helm-mode 1)
(autoload 'helm-company "helm-company") ;; Not necessary if using ELPA package
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-/") 'helm-company)
     (define-key company-active-map (kbd "C-/") 'helm-company)))

;; markdown mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)

(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; adoc mode
(add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))

;; global company mode
(add-hook 'after-init-hook 'global-company-mode)

;; show line nos on all buffers
(global-linum-mode)

;; saveplace
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places/file")
(setq save-place-forget-unreadable-files nil)

;;FUNCTION definitions here be

;; The actual expansion function
(defun try-expand-by-dict (old)
  ;; old is true if we have already attempted an expansion
  (unless (bound-and-true-p ispell-minor-mode)
    (ispell-minor-mode 1))

  ;; english-words.txt is the fallback dicitonary
  (if (not ispell-alternate-dictionary)
      (setq ispell-alternate-dictionary (file-truename "~/.emacs.d/misc/english-words.txt")))
  (let ((lookup-func (if (fboundp 'ispell-lookup-words)
                       'ispell-lookup-words
                       'lookup-words)))
    (unless old
      (he-init-string (he-lisp-symbol-beg) (point))
      (if (not (he-string-member he-search-string he-tried-table))
        (setq he-tried-table (cons he-search-string he-tried-table)))
      (setq he-expand-list
            (and (not (equal he-search-string ""))
                 (funcall lookup-func (concat (buffer-substring-no-properties (he-lisp-symbol-beg) (point)) "*")))))
    (if (null he-expand-list)
      (if old (he-reset-string))
      (he-substitute-string (car he-expand-list))
      (setq he-expand-list (cdr he-expand-list))
      t)
    ))

(setq hippie-expand-try-functions-list
      '(;; try-expand-dabbrev
        ;; try-expand-dabbrev-all-buffers
        try-expand-by-dict))

;; for commenting and uncommenting lines
(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

;; line-wraping
(global-visual-line-mode 1)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

 ;; something about the search function 
(defun my-goto-match-beginning ()
    (when isearch-forward (goto-char isearch-other-end)))
(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)


(defun insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
		 ((not prefix) "%d.%m.%Y")
		 ((equal prefix '(4)) "%Y-%m-%d")
		 ((equal prefix '(16)) "%A, %d. %B %Y")))
	(system-time-locale "de_DE"))
    (insert (format-time-string format))))


;; today's date
(defun insert-date-and-time ()
  "Inserts standard date time string." 
  (interactive)
  (insert (format-time-string "%H %M")))

;; macro end-of-line-then-new-line
(fset 'end-of-line-then-new-line
   "\C-e\C-j")

;; macro align-to-four
(fset 'align-to-four
   "\C-a\C-t\C-e\C-u4\234")

;; various MODES go here

;; back button
;; C-x C-<SPC>	go back in global-mark-ring, respects prefix arg
;; C-x C-<left>	go back in global-mark-ring
;; C-x C-<right>	go forward in global-mark-ring
;; C-x <SPC>	go back in (buffer-local) mark-ring, respects prefix arg
;; C-x <left>	go back in (buffer-local) mark-ring
;; C-x <right>	go forward in (buffer-local) mark-ring
(back-button-mode 1)

;; PASTE WILL OVERWRITE SELECTION
(delete-selection-mode 1)

;; undo tree
(global-undo-tree-mode)

;; modify default dir for emacs file backups
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))

;; python tools
(elpy-enable)

;; easy window-jumping
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; disable tool-bar
(tool-bar-mode -1)

;; better C-x C-f
(ido-mode t)

;; start emacs server
(server-start)

;; when closing emacs saves the layout and open buffers
(desktop-save-mode 1)

;; shows matching brackets
(show-paren-mode 1) 

;; sharing clipboard with the window manager
(setq x-select-enable-clipboard t)
(setq save-interprogram-paste-before-kill t)

;; for saving window layouts
(autoload 'save-current-configuration "revive" "Save status" t)
(autoload 'resume "revive" "Resume Emacs" t)
(autoload 'wipe "revive" "Wipe Emacs" t)

;; C-x Shift-s to save current window layout
;; C-x Shift-f to return to saved window layout
;; ALSO: You can have separate saved window layouts:
;; C-u <nr> C-x Shift-s
;; And then
;; C-u <nr> C-x Shift-f
(define-key ctl-x-map "S" 'save-current-configuration)
(define-key ctl-x-map "F" 'resume)
(define-key ctl-x-map "K" 'wipe)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-t") 'set-mark-command)
(global-set-key (kbd "C-'") 'comment-or-uncomment-region-or-line)
(global-set-key [f8] 'nav-toggle)

;; windows-like keybindings: C-c C-v C-x
(cua-mode)

;; this needs to go AFTER the cua-mode
(global-set-key (kbd "C-SPC") 'dabbrev-expand)

;; C-j should function as RET in Python Elpy mode
;; commented out on Linux. doesn't work. default behaviour is ok
;; (eval-after-load 'elpy
;;   '(define-key elpy-mode-map (kbd "C-j") 'newline))

;; copy full path to file to clipboard
(global-set-key (kbd "M-c") 'my-put-file-name-on-clipboard)

;; add Shift-TAB to run other-window
(global-set-key (kbd "<C-tab>") 'other-window)
;; binding from somewhere
(global-set-key (kbd "<S-iso-lefttab>") 'other-window)
;; binding from putty terminal for Shift-Tab
(global-set-key (kbd "<backtab>") 'other-window)

;; keybinding for my macro
(global-set-key (kbd "C-o") 'end-of-line-then-new-line)

;; keybinding for align-to-four macro
(global-set-key (kbd "C-M-=") 'align-to-four)

;; keybindings for TAB and S-TAB for indentation
(global-set-key (kbd "M--") 'elpy-nav-move-iblock-left)
(global-set-key (kbd "M-=") 'elpy-nav-move-iblock-right)

;; keybindings for scrolling
(global-set-key (kbd "C-M-n") 'scroll-up-command)
(global-set-key (kbd "C-M-p") 'scroll-down-command)

;; keybindings for goto-line
(global-set-key (kbd "M-g") 'goto-line)

;; keybindings for font increase and decrease
(global-set-key (kbd "<C-kp-add>") 'text-scale-increase)
(global-set-key (kbd "<C-kp-subtract>") 'text-scale-decrease)

;; keybindings for inserting today's date or date & time
(global-set-key (kbd "C-c .") 'insert-date)
(global-set-key (kbd "C-c ,") 'insert-date-and-time)

;; keybindings for yafolding mode
(global-unset-key (kbd "<C-S-return>"))
(global-unset-key (kbd "<C-return>"))
(global-unset-key (kbd "<C-M-return>"))

(defvar yafolding-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-S-return>") #'yafolding-toggle-element)
    (define-key map (kbd "<C-M-return>") #'yafolding-toggle-all)
    map))

;; ace jump mode
(define-key global-map (kbd "C-;") 'ace-jump-mode)
;; make sure insert quoted is mapped
(define-key global-map (kbd "C-q") 'quoted-insert)

;; global undo tree keybindings
(define-key global-map (kbd "C-x u") 'undo-tree-visualize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~\\.emacs.d\\bookmarks")
 '(custom-enabled-themes (quote (tsdh-dark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(global-set-key (kbd "M-/") 'hippie-expand)

(setq org-src-fontify-natively t)

