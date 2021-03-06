;;; packages.el --- my Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 Peng Liu
;;
;; Author: myme5261314 <myme5261314@gmail.com>
;; URL: https://github.com/myme5261314/spacemacs-my
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq my-packages '(lispy company
                          cmake-font-lock
                          cmake-mode
                          flycheck
                          markdown-mode
                          swiper
                          ;; counsel
                          magit
                          git-messenger
                          helm-flyspell
                          helm
                          ;; helm-ls-git
                          keyfreq
                          ;; worf
                          ;; flycheck-package
                          ;; (org :location built-in)
                          ;; nodejs-repl
                          ;; js2-mode
                          ;; js2-refactor
                          visual-regexp
                          visual-regexp-steroids
                          helm-gtags
                          persp-mode
                          json-mode
                          ;; racket-mode
                          ;; yasnippet
                          helm-ag
                          hungry-delete
                          ;; flyspell
                          find-file-in-project
                          hl-anything
                          projectile
                          wrap-region
                          web-mode
                          ;; tagedit
                          ;; js-comint
                          ctags-update
                          evil-vimish-fold
                          beacon
                          (dired-mode :location built-in)
                          ;; js-doc
                          ;; post extension names go here
                          ; (doxymacs :location local)
                          ;; nodejs-repl-eval don't support es6 and js2-mode also don't support it
                          ;; so I use js-comit instead.
                          ;; (nodejs-repl-eval :location local)
                          ;; plain-org-wiki
                          (whitespace :location built-in)
                          erc
                          smartparens
                          peep-dired
                          org-tree-slide
                          org-bullets
                          ;; evil-escape
                          (cc-mode :location built-in)
                          youdao-dictionary
                          multiple-cursors
                          company-c-headers
                          ;; hydra
                          ;; org-octopress
                          helm-github-stars
                          evil
                          deft
                          elfeed
                          lua-mode
                          ycmd
                          (vue-mode :location (recipe
                                               :fetcher github
                                               :repo "codefalling/vue-mode"))
                          ;; mwe-log-commands
                          org-pomodoro
                          discover-my-major
                          popwin
                          ;; ox-reveal
                          ;; org-mac-link
                          ;; ace-window
                          avy
                          ;; 4clojure
                          ;; persp-mode
                          ;; (gulpjs :location (recipe :fetcher github :repo "my/emacs-gulpjs"))
                          ;; osx-dictionary
                          litable
                          pangu-spacing
                          ;; ace-window
                          fill-column-indicator
                          ;; chinese-fonts-setup
                          ))

(defun my/init-peep-dired ()
  ;;preview files in dired
  (use-package peep-dired
    :defer t
    :commands (peep-dired-next-file peep-dired-prev-file):bind
    (:map dired-mode-map
          ("P" . peep-dired))))

(defun my/post-init-smartparens ()
  (progn
    (defun wrap-sexp-with-new-round-parens ()
      (interactive)
      (insert "()")
      (backward-char)
      (sp-forward-slurp-sexp))
    (global-set-key (kbd "C-(")
                    'wrap-sexp-with-new-round-parens)
    (with-eval-after-load 'smartparens
      (evil-define-key 'normal
        sp-keymap
        (kbd ")>")
        'sp-forward-slurp-sexp
        (kbd ")<")
        'sp-forward-barf-sexp
        (kbd "(>")
        'sp-backward-barf-sexp
        (kbd "(<")
        'sp-backward-slurp-sexp))))

(defun my/post-init-erc ()
  (progn
    (defun my-erc-hook (match-type nick message)
      "Shows a growl notification, when user's nick was mentioned. If the buffer is currently not visible, makes it sticky."
      (unless (posix-string-match "^\\** *Users on #" message)
        (my/growl-notification (concat "ERC: : "
                                       (buffer-name (current-buffer)))
                               message
                               t)))
    (add-hook 'erc-text-matched-hook 'my-erc-hook)
    (spaceline-toggle-erc-track-off)))

(defun my/post-init-whitespace ()
  (progn
    ;; ;; http://emacsredux.com/blog/2013/05/31/highlight-lines-that-exceed-a-certain-length-limit/
    (setq whitespace-line-column fill-column) ;; limit line length
    ;;https://www.reddit.com/r/emacs/comments/2keh6u/show_tabs_and_trailing_whitespaces_only/
    (setq whitespace-display-mappings
          ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
          '((space-mark 32
                        [183]
                        [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」

            (newline-mark 10
                          [182 10]) ; 10 LINE FEED

            (tab-mark 9
                      [187 9]
                      [9655 9]
                      [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
            ))
    (setq whitespace-style '(face tabs trailing tab-mark))
    ;; (setq whitespace-style '(face lines-tail))
    ;; show tab;  use untabify to convert tab to whitespace
    ;; (setq spacemacs-show-trailing-whitespace nil)
    (setq-default tab-width 4)
    ;; set-buffer-file-coding-system -> utf8 to convert dos to utf8
    (setq inhibit-eol-conversion t)
    (add-hook 'prog-mode-hook 'whitespace-mode)
    ;; (global-whitespace-mode +1)
    (with-eval-after-load 'whitespace
      (progn
        (set-face-attribute 'whitespace-tab nil :background "#Adff2f"
                            :foreground "#00a8a8"
                            :weight 'bold)
        (set-face-attribute 'whitespace-trailing nil
                            :background "#e4eeff"
                            :foreground "#183bc8"
                            :weight 'normal)))
    (diminish 'whitespace-mode)))

(defun my/init-dired-mode ()
  (use-package dired-mode
    :init (progn
            (require 'dired-x)
            (require 'dired-aux)
            (setq dired-listing-switches "-alh")
            (setq dired-guess-shell-alist-user '(("\\.pdf\\'" "open")
                                                 ("\\.docx\\'" "open")
                                                 ("\\.\\(?:djvu\\|eps\\)\\'" "open")
                                                 ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'"
                                                  "open")
                                                 ("\\.\\(?:xcf\\)\\'" "open")
                                                 ("\\.csv\\'" "open")
                                                 ("\\.tex\\'" "open")
                                                 ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\)\\(?:\\.part\\)?\\'"
                                                  "open")
                                                 ("\\.\\(?:mp3\\|flac\\)\\'" "open")
                                                 ("\\.html?\\'" "open")
                                                 ("\\.md\\'" "open")))
            ;; always delete and copy recursively
            (setq dired-recursive-deletes 'always)
            (setq dired-recursive-copies 'always)
            (defvar dired-filelist-cmd '(("vlc" "-L")))
            (defun dired-get-size ()
              (interactive)
              (let ((files (dired-get-marked-files)))
                (with-temp-buffer
                  (apply 'call-process "/usr/bin/du" nil t nil
                         "-sch" files)
                  (message "Size of all marked files: %s"
                           (progn
                             (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
                             (match-string 1))))))
            (defun dired-start-process (cmd &optional file-list)
              (interactive (let ((files (dired-get-marked-files t current-prefix-arg)))
                             (list (dired-read-shell-command "& on %s: " current-prefix-arg
                                                             files)
                                   files)))
              (let (list-switch)
                (start-process cmd
                               nil
                               shell-file-name
                               shell-command-switch
                               (format "nohup 1>/dev/null 2>/dev/null %s \"%s\""
                                       (if (and (> (length file-list) 1)
                                                (setq list-switch (cadr (assoc cmd dired-filelist-cmd))))
                                           (format "%s %s" cmd list-switch)
                                         cmd)
                                       (mapconcat #'expand-file-name file-list "\" \"")))))
            (defun dired-open-term ()
              "Open an `ansi-term' that corresponds to current directory."
              (interactive)
              (let* ((current-dir (dired-current-directory))
                     (buffer (if (get-buffer "*zshell*")
                                 (switch-to-buffer "*zshell*")
                               (ansi-term "/bin/zsh" "zshell")))
                     (proc (get-buffer-process buffer)))
                (term-send-string proc
                                  (if (file-remote-p current-dir)
                                      (let ((v (tramp-dissect-file-name current-dir t)))
                                        (format "ssh %s@%s\n"
                                                (aref v 1)
                                                (aref v 2)))
                                    (format "cd '%s'\n" current-dir)))))
            (defun dired-copy-file-here (file)
              (interactive "fCopy file: ")
              (copy-file file default-directory))
            ;;dired find alternate file in other buffer
            (defun my-dired-find-file ()
              "Open buffer in another window"
              (interactive)
              (let ((filename (dired-get-filename nil t)))
                (if (car (file-attributes filename))
                    (dired-find-alternate-file)
                  (dired-find-file-other-window))))
            ;; do command on all marked file in dired mode
            (defun my/dired-do-command (command)
              "Run COMMAND on marked files. Any files not already open will be opened.
After this command has been run, any buffers it's modified will remain
open and unsaved."
              (interactive "CRun on marked files M-x ")
              (save-window-excursion (mapc (lambda (filename)
                                             (find-file filename)
                                             (call-interactively command))
                                           (dired-get-marked-files))))
            (defun my/dired-up-directory ()
              "goto up directory and resue buffer"
              (interactive)
              (find-alternate-file ".."))
            (evilified-state-evilify-map dired-mode-map
              :mode dired-mode
              :bindings (kbd "C-k")'my/dired-up-directory
              "<RET>"
              'dired-find-alternate-file
              "E"
              'dired-toggle-read-only
              "C"
              'dired-do-copy
              "<mouse-2>"
              'my-dired-find-file
              "`"
              'dired-open-term
              "p"
              'peep-dired-prev-file
              "n"
              'peep-dired-next-file
              "z"
              'dired-get-size
              "c"
              'dired-copy-file-here)):defer
              t))

(defun my/init-occur-mode ()
  (defun occur-non-ascii ()
    "Find any non-ascii characters in the current buffer."
    (interactive)
    (occur "[^[:ascii:]]"))
  (defun occur-dwim ()
    "Call `occur' with a sane default."
    (interactive)
    (push (if (region-active-p)
              (buffer-substring-no-properties (region-beginning)
                                              (region-end))
            (let ((sym (thing-at-point 'symbol)))
              (when (stringp sym)
                (regexp-quote sym))))
          regexp-history)
    (call-interactively 'occur))
  (bind-key* "M-s o" 'occur-dwim)
  (evilified-state-evilify occur-mode occur-mode-map
    "RET" 'occur-mode-goto-occurrence))

(defun my/init-beacon ()
  (use-package beacon
    :init (progn
            (spacemacs|add-toggle beacon
              :status beacon-mode
              :on (beacon-mode):off
              (beacon-mode -1)
              :documentation "Enable point highlighting after scrolling"
              :evil-leader "otb")
            (spacemacs/toggle-beacon-on)):config
            (spacemacs|hide-lighter beacon-mode)))

(defun my/init-evil-vimish-fold ()
  (use-package evil-vimish-fold
    :init (vimish-fold-global-mode 1):config
    (progn
      (define-key evil-normal-state-map (kbd "zf") 'vimish-fold)
      (define-key evil-visual-state-map (kbd "zf") 'vimish-fold)
      (define-key evil-normal-state-map (kbd "zd") 'vimish-fold-delete)
      (define-key evil-normal-state-map (kbd "za") 'vimish-fold-toggle))))

(defun my/init-ctags-update ()
  (use-package ctags-update
    :init (progn
            ;; (add-hook 'js2-mode-hook 'turn-on-ctags-auto-update-mode)
            (define-key evil-normal-state-map (kbd "gf") (lambda ()
                                                           (interactive)
                                                           (find-tag (find-tag-default-as-regexp))))
            (define-key evil-normal-state-map (kbd "gb") 'pop-tag-mark)
            (define-key evil-normal-state-map (kbd "gn") (lambda ()
                                                           (interactive)
                                                           (find-tag last-tag t)))):defer
                                                           t
                                                           :config (spacemacs|hide-lighter ctags-auto-update-mode)))

;; nodejs-repl is much better now.
;; (defun my/init-js-comint ()
;;   (use-package js-comint
;;     :init
;;     (progn
;;       ;; http://stackoverflow.com/questions/13862471/using-node-js-with-js-comint-in-emacs
;;       (setq inferior-js-mode-hook
;;             (lambda ()
;;               ;; We like nice colors
;;               (ansi-color-for-comint-mode-on)
;;               ;; Deal with some prompt nonsense
;;               (add-to-list
;;                'comint-preoutput-filter-functions
;;                (lambda (output)
;;                  (replace-regexp-in-string "\033\\[[0-9]+[GKJ]" "" output)))))
;;       (setq inferior-js-program-command "node"))))

(defun my/post-init-web-mode ()
  (setq company-backends-web-mode '((company-dabbrev-code company-keywords company-etags) company-files
                                    company-dabbrev)))



(defun my/init-wrap-region ()
  (use-package wrap-region
    :init (progn
            (wrap-region-global-mode t)
            (wrap-region-add-wrappers '(("$" "$")
                                        ("{-" "-}" "#")
                                        ("/" "/" nil ruby-mode)
                                        ("/* " " */"
                                         "#"
                                         (java-mode javascript-mode css-mode js2-mode))
                                        ("`" "`" nil
                                         (markdown-mode ruby-mode))))
            (add-to-list 'wrap-region-except-modes 'dired-mode)
            (add-to-list 'wrap-region-except-modes 'web-mode)):defer
            t
            :config (spacemacs|hide-lighter wrap-region-mode)))

(defun my/post-init-projectile ()
  (with-eval-after-load 'projectile
    (progn
      (setq projectile-completion-system 'ivy)
      (add-to-list 'projectile-other-file-alist
                   '("html" "js")) ;; switch from html -> js
      (add-to-list 'projectile-other-file-alist
                   '("js" "html")) ;; switch from js -> html
      (spacemacs/set-leader-keys "pf" 'my/open-file-with-projectile-or-counsel-git))))

;; spacemacs distribution disabled this package, because it has overlay bug.
;; I hack the implementation here. on default, the hl-highlight-mode is disabled.
(defun my/post-init-hl-anything ()
  (progn
    (hl-highlight-mode -1)
    (spacemacs|add-toggle toggle-hl-anything
      :status hl-highlight-mode
      :on (hl-highlight-mode):off
      (hl-highlight-mode -1)
      :documentation "Toggle highlight anything mode."
      :evil-leader "ths")))

(defun my/init-find-file-in-project ()
  (use-package find-file-in-project :defer t
    :init))



(defun my/post-init-hungry-delete ()
  ;; (add-hook 'prog-mode-hook 'hungry-delete-mode)
  (global-hungry-delete-mode t))


(defun my/post-init-helm-ag ()
  (progn
    (setq helm-ag-use-agignore t)
    ;; This settings use .agignore file to ignore items, and it don't respect to .hgignore, .gitignore
    ;; when there are some git repositories are in .gitignore file, this options is very useful.
    ;;And the .agignore file while be searched at PROJECT_ROOT/.agignore and ~/.agignore
    ;; Thanks to 'man ag' and 'customize-group<RET> helm-ag' for finding the solution... Always RTFM.
    (setq helm-ag-command-option " -U")))



(defun my/post-init-yasnippet ()
  (progn
    (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
    (mapc #'(lambda (hook)
              (remove-hook hook 'spacemacs/load-yasnippet))
          '(prog-mode-hook org-mode-hook markdown-mode-hook))
    (defun my/load-yasnippet ()
      (unless yas-global-mode
        (progn
          (yas-global-mode 1)
          (setq my-snippet-dir (expand-file-name "~/.spacemacs.d/snippets"))
          (setq yas-snippet-dirs my-snippet-dir)
          (yas-load-directory my-snippet-dir)
          (setq yas-wrap-around-region t)))
      (yas-minor-mode 1))
    (spacemacs/add-to-hooks 'my/load-yasnippet
                            '(prog-mode-hook markdown-mode-hook org-mode-hook))
    ;; https://github.com/capitaomorte/yasnippet/issues/557
    (add-hook 'yas-minor-mode-hook
              (lambda ()
                (yas-activate-extra-mode 'fundamental-mode)))))

(defun my/post-init-json-mode ()
  (add-to-list 'auto-mode-alist
               '("\\.tern-project\\'" . json-mode)))


(defun my/post-init-helm-gtags ()
  (with-eval-after-load 'helm-gtags
    (progn
      (evil-make-overriding-map helm-gtags-mode-map
                                'normal)
      (add-hook 'helm-gtags-mode-hook #'evil-normalize-keymaps))))

(defun my/init-visual-regexp-steroids ()
  (use-package visual-regexp-steroids :init))

(defun my/init-visual-regexp ()
  (use-package visual-regexp :init))

(defun my/init-nodejs-repl ()
  (use-package nodejs-repl :init :defer
    t))

(defun my/init-flycheck-package ()
  (use-package flycheck-package))

(defun my/init-lispy ()
  "Initialize lispy"
  (use-package lispy
    :defer t
    :diminish (lispy-mode):init
    (progn
      (defun my/turn-on-lispy ()
        (interactive)
        (add-hook 'emacs-lisp-mode-hook 'lispy-mode))
      (run-with-idle-timer 1 nil #'my/turn-on-lispy)
      ;; (add-hook 'lispy-mode-hook 'spacemacs/toggle-aggressive-indent-on)
      (add-hook 'eval-expression-minibuffer-setup-hook
                'lispy-mode)
      ;; (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
      ;; (add-hook 'spacemacs-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'clojure-mode-hook
                (lambda ()
                  (lispy-mode 1)))
      (add-hook 'scheme-mode-hook
                (lambda ()
                  (lispy-mode 1)))
      (add-hook 'cider-repl-mode-hook
                (lambda ()
                  (lispy-mode 1))))))


(defun my/post-init-company ()
  (with-eval-after-load 'company
    (progn
      (setq company-minimum-prefix-length 1 company-idle-delay
            0.08)
      (spacemacs|add-company-backends :modes shell-script-mode)
      (spacemacs|add-company-backends :modes makefile-bsdmake-mode)
      (spacemacs|add-company-backends :modes sh-mode)
      (spacemacs|add-company-backends :modes lua-mode)
      (spacemacs|add-company-backends :modes nxml-mode)
      (spacemacs|add-company-backends :modes text-mode)
      (spacemacs|add-company-backends :modes conf-unix-mode))))

(defun my/init-cmake-font-lock ()
  (use-package cmake-font-lock :defer t))

(defun my/init-google-c-style ()
  (use-package google-c-style
    :init (add-hook 'c-mode-common-hook 'google-set-c-style)))

(defun my/post-init-cmake-mode ()
  (progn
    (spacemacs/declare-prefix-for-mode 'cmake-mode
      "mh" "docs")
    (spacemacs/set-leader-keys-for-major-mode
      'cmake-mode "hd" 'cmake-help)
    (defun cmake-rename-buffer ()
      "Renames a CMakeLists.txt buffer to cmake-<directory name>."
      (interactive)
      (when (and (buffer-file-name)
                 (string-match "CMakeLists.txt" (buffer-name)))
        (setq parent-dir (file-name-nondirectory (directory-file-name (file-name-directory (buffer-file-name)))))
        (setq new-buffer-name (concat "cmake-" parent-dir))
        (rename-buffer new-buffer-name t)))
    (add-hook 'cmake-mode-hook
              (function cmake-rename-buffer))))


(defun my/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (progn
      ;; (setq flycheck-display-errors-function 'flycheck-display-error-messages)
      (setq flycheck-display-errors-delay 0.2)
      ;; (remove-hook 'c-mode-hook 'flycheck-mode)
      ;; (remove-hook 'c++-mode-hook 'flycheck-mode)
      )))

;; configs for writing
(defun my/post-init-markdown-mode ()
  (progn
    (add-to-list 'auto-mode-alist
                 '("\\.mdown\\'" . markdown-mode))
    (with-eval-after-load 'markdown-mode
      (progn
        (spacemacs|add-company-backends :modes markdown-mode)
        (defun my/markdown-to-html ()
          (interactive)
          (start-process "grip"
                         "*gfm-to-html*"
                         "grip"
                         (buffer-file-name)
                         "5000")
          (browse-url (format "http://localhost:5000/%s.%s"
                              (file-name-base)
                              (file-name-extension (buffer-file-name)))))
        (spacemacs/set-leader-keys-for-major-mode
          'gfm-mode-map "p" 'my/markdown-to-html)
        (spacemacs/set-leader-keys-for-major-mode
          'markdown-mode "p" 'my/markdown-to-html)
        (evil-define-key 'normal
          markdown-mode-map
          (kbd "TAB")
          'markdown-cycle)))))


(defun my/init-keyfreq ()
  (use-package keyfreq
    :init (progn
            (keyfreq-mode t)
            (keyfreq-autosave-mode 1))))

(defun my/post-init-swiper ()
  "Initialize my package"
  (progn
    (defun my-swiper-search (p)
      (interactive "P")
      (let ((current-prefix-arg nil))
        (call-interactively (if p #'spacemacs/swiper-region-or-symbol
                              #'swiper))))
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (use-package recentf
      :config (setq recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG" "github.*txt$"
                                      ".*png$"))(setq recentf-max-saved-items 60))
    (evilified-state-evilify ivy-occur-mode ivy-occur-mode-map)
    (use-package ivy
      :defer t
      :config (progn
                (spacemacs|hide-lighter ivy-mode)
                ;; http://blog.binchen.org/posts/use-ivy-to-open-recent-directories.html
                (defun counsel-goto-recent-directory ()
                  "Recent directories"
                  (interactive)
                  (unless recentf-mode
                    (recentf-mode 1))
                  (let ((collection (delete-dups (append (mapcar 'file-name-directory recentf-list)
                                                         ;; fasd history
                                                         (if (executable-find "fasd")
                                                             (split-string (shell-command-to-string "fasd -ld")
                                                                           "\n"
                                                                           t))))))
                    (ivy-read "directories:" collection :action 'dired
                              :caller 'counsel-goto-recent-directory)))
                (spacemacs/set-leader-keys "fad" 'counsel-goto-recent-directory)
                (define-key ivy-minibuffer-map (kbd "C-c o") 'ivy-occur)
                (define-key ivy-minibuffer-map (kbd "s-o") 'ivy-dispatching-done)
                (define-key ivy-minibuffer-map (kbd "C-s-j") 'ivy-immediate-done)
                (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
                (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)))
    (define-key global-map (kbd "C-s") 'my-swiper-search)))


(defun my/post-init-magit ()
  (progn
    (with-eval-after-load 'magit
      (progn
        (add-to-list 'magit-no-confirm 'stage-all-changes)
        (define-key magit-log-mode-map (kbd "W") 'magit-copy-section-value)
        (define-key magit-status-mode-map (kbd "s-1") 'magit-jump-to-unstaged)
        (define-key magit-status-mode-map (kbd "s-2") 'magit-jump-to-untracked)
        (define-key magit-status-mode-map (kbd "s-3") 'magit-jump-to-staged)
        (define-key magit-status-mode-map (kbd "s-4") 'magit-jump-to-stashes)
        (setq magit-completing-read-function 'magit-builtin-completing-read)
        ;; http://emacs.stackexchange.com/questions/6021/change-a-branchs-upstream-with-magit/6023#6023
        (magit-define-popup-switch 'magit-push-popup
          ?u "Set upstream" "--set-upstream")
        ;; (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
        ;; (add-hook 'magit-section-set-visibility-hook '(lambda (section) (let ((section-type (magit-section-type section)))
        ;;                                                              (if (or (eq 'untracked section-type)
        ;;                                                                      (eq 'stashes section-type))
        ;;                                                                  'hide))))
        ))
    ;; Githu PR settings
    ;; "http://endlessparentheses.com/create-github-prs-from-emacs-with-magit.html"
    (setq magit-repository-directories '("~/cocos2d-x/"))
    (setq magit-push-always-verify nil)
    (defun my/magit-visit-pull-request ()
      "Visit the current branch's PR on GitHub."
      (interactive)
      (let ((remote-branch (magit-get-current-branch)))
        (cond
         ((null remote-branch)
          (message "No remote branch"))
         (t (browse-url (format "https://github.com/%s/pull/new/%s"
                                (replace-regexp-in-string "\\`.+github\\.com:\\(.+\\)\\.git\\'"
                                                          "\\1"
                                                          (magit-get "remote"
                                                                     (magit-get-remote)
                                                                     "url"))
                                remote-branch))))))
    (eval-after-load 'magit
      '(define-key magit-mode-map (kbd "C-c g") #'my/magit-visit-pull-request))
    (setq magit-process-popup-time 10)))

(defun my/post-init-git-messenger ()
  (use-package git-messenger
    :defer t
    :config (progn
              (defun my/github-browse-commit ()
                "Show the GitHub page for the current commit."
                (interactive)
                (use-package github-browse-file
                  :commands (github-browse-file--relative-url))
                (let* ((commit git-messenger:last-commit-id)
                       (url (concat "https://github.com/"
                                    (github-browse-file--relative-url)
                                    "/commit/"
                                    commit)))
                  (github-browse--save-and-view url)
                  (git-messenger:popup-close)))
              (define-key git-messenger-map (kbd "f") 'my/github-browse-commit))))

(defun my/post-init-helm-flyspell ()
  (progn
    ;; "http://emacs.stackexchange.com/questions/14909/how-to-use-flyspell-to-efficiently-correct-previous-word/14912#14912"
    (defun my/flyspell-goto-previous-error (arg)
      "Go to arg previous spelling error."
      (interactive "p")
      (while (not (= 0 arg))
        (let ((pos (point))
              (min (point-min)))
          (if (and (eq (current-buffer) flyspell-old-buffer-error)
                   (eq pos flyspell-old-pos-error))
              (progn
                (if (= flyspell-old-pos-error min)
                    ;; goto beginning of buffer
                    (progn
                      (message "Restarting from end of buffer")
                      (goto-char (point-max)))
                  (backward-word 1))
                (setq pos (point))))
          ;; seek the next error
          (while (and (> pos min)
                      (let ((ovs (overlays-at pos))
                            (r '()))
                        (while (and (not r)
                                    (consp ovs))
                          (if (flyspell-overlay-p (car ovs))
                              (setq r t)
                            (setq ovs (cdr ovs))))
                        (not r)))
            (backward-word 1)
            (setq pos (point)))
          ;; save the current location for next invocation
          (setq arg (1- arg))
          (setq flyspell-old-pos-error pos)
          (setq flyspell-old-buffer-error (current-buffer))
          (goto-char pos)
          (call-interactively 'helm-flyspell-correct)
          (if (= pos min)
              (progn
                (message "No more miss-spelled word!")
                (setq arg 0))))))
    ;; http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html#comment-2440958792
    (define-key ctl-x-map "\C-i" #'endless/ispell-word-then-abbrev)
    (defun endless/ispell-word-then-abbrev (p)
      "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global."
      (interactive "P")
      (let (bef aft)
        (save-excursion
          (while (progn
                   (backward-word)
                   (and (setq bef (thing-at-point 'word))
                        (not (ispell-word nil 'quiet)))))
          (setq aft (thing-at-point 'word)))
        (when (and aft
                   bef
                   (not (equal aft bef)))
          (setq aft (downcase aft))
          (setq bef (downcase bef))
          (define-abbrev (if p local-abbrev-table global-abbrev-table)
            bef
            aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef
                   aft
                   (if p "loc" "glob")))))
    (setq save-abbrevs 'silently)
    (setq-default abbrev-mode t)
    (bind-key* "C-;" 'my/flyspell-goto-previous-error)
    (global-set-key (kbd "C-c s")
                    'helm-flyspell-correct)))

(defun my/post-init-helm ()
  (progn
    (global-set-key (kbd "C-s-y")
                    'helm-show-kill-ring)
    ;; See https://github.com/bbatsov/prelude/pull/670 for a detailed
    ;; discussion of these options.
    (setq helm-split-window-in-side-p t helm-move-to-line-cycle-in-source
          t helm-ff-search-library-in-sexp t helm-ff-file-name-history-use-recentf
          t helm-buffer-max-length 45)
    (setq helm-completing-read-handlers-alist '((describe-function . ido)
                                                (describe-variable . ido)
                                                (debug-on-entry . helm-completing-read-symbols)
                                                (find-function . helm-completing-read-symbols)
                                                (find-tag . helm-completing-read-with-cands-in-buffer)
                                                (ffap-alternate-file . nil)
                                                (tmm-menubar . nil)
                                                (dired-do-copy . nil)
                                                (dired-do-rename . nil)
                                                (dired-create-directory . nil)
                                                (find-file . ido)
                                                (copy-file-and-rename-buffer . nil)
                                                (rename-file-and-buffer . nil)
                                                (w3m-goto-url . nil)
                                                (ido-find-file . nil)
                                                (ido-edit-input . nil)
                                                (mml-attach-file . ido)
                                                (read-file-name . nil)
                                                (yas/compile-directory . ido)
                                                (execute-extended-command . ido)
                                                (minibuffer-completion-help . nil)
                                                (minibuffer-complete . nil)
                                                (c-set-offset . nil)
                                                (wg-load . ido)
                                                (rgrep . nil)
                                                (read-directory-name . ido)))))


(defun my/init-helm-ls-git ()
  (use-package helm-ls-git
    :init (progn
            ;;beautify-helm buffer when long file name is present
            (setq helm-ls-git-show-abs-or-relative 'relative))))


;;configs for EVIL mode


(defun my/init-worf ()
  (use-package worf
    :defer t
    :init (add-hook 'org-mode-hook 'worf-mode)))


;;In order to export pdf to support Chinese, I should install Latex at here: https://www.tug.org/mactex/
;; http://freizl.github.io/posts/2012-04-06-export-orgmode-file-in-Chinese.html
;;http://stackoverflow.com/questions/21005885/export-org-mode-code-block-and-result-with-different-styles
(defun my/post-init-org ()
  (with-eval-after-load 'org
    (progn
      ;; https://github.com/syl20bnr/spacemacs/issues/2994#issuecomment-139737911
      ;; (when (configuration-layer/package-usedp 'company)
      ;;   (spacemacs|add-company-hook org-mode))
      ;; System locale to use for formatting time values.
      (setq system-time-locale "C") ; Make sure that the weekdays in the
                                        ; time stamps of your Org mode files and
                                        ; in the agenda appear in English.
      (spacemacs|disable-company org-mode)
      (spacemacs/set-leader-keys-for-major-mode
        'org-mode "," 'org-priority)
      (require 'org-compat)
      (require 'org)
      ;; (add-to-list 'org-modules "org-habit")
      (add-to-list 'org-modules 'org-habit)
      (require 'org-habit)
      (with-eval-after-load 'org-agenda
        (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro)
        (spacemacs/set-leader-keys-for-major-mode
          'org-agenda-mode "." 'spacemacs/org-agenda-transient-state/body))
      ;; the %i would copy the selected text into the template
      ;;http://www.howardism.org/Technical/Emacs/journaling-org.html
      ;;add multi-file journal
      (setq org-capture-templates '(("t" "Todo"
                                     entry
                                     (file+headline "~/org-notes/gtd.org" "Workspace")
                                     "* TODO [#B] %?\n  %i\n"
                                     :empty-lines 1)
                                    ("n" "notes"
                                     entry
                                     (file+headline "~/org-notes/notes.org" "Quick notes")
                                     "* TODO [#C] %?\n  %i\n %U"
                                     :empty-lines 1)
                                    ("b" "Blog Ideas"
                                     entry
                                     (file+headline "~/org-notes/notes.org" "Blog Ideas")
                                     "* TODO [#B] %?\n  %i\n %U"
                                     :empty-lines 1)
                                    ("w" "work"
                                     entry
                                     (file+headline "~/org-notes/gtd.org" "Cocos2D-X")
                                     "* TODO [#A] %?\n  %i\n %U"
                                     :empty-lines 1)
                                    ("c" "Chrome"
                                     entry
                                     (file+headline "~/org-notes/notes.org" "Quick notes")
                                     "* TODO [#C] %?\n %(my/retrieve-chrome-current-tab-url)\n %i\n %U"
                                     :empty-lines 1)
                                    ("l" "links"
                                     entry
                                     (file+headline "~/org-notes/notes.org" "Quick notes")
                                     "* TODO [#C] %?\n  %i\n %a \n %U"
                                     :empty-lines 1)
                                    ("j" "Journal Entry"
                                     entry
                                     (file+datetree "~/org-notes/journal.org")
                                     "* %?"
                                     :empty-lines 1)))
      ;;An entry without a cookie is treated just like priority ' B '.
      ;;So when create new task, they are default 重要且紧急

      (setq org-agenda-custom-commands '(("w" . "任务安排")
                                         ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
                                         ("wb" "重要且不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
                                         ("wc" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")
                                         ("b" "Blog" tags-todo "BLOG")
                                         ("p" . "项目安排")
                                         ("pw" tags-todo "PROJECT+WORK+CATEGORY=\"cocos2d-x\"")
                                         ("Q" . "Custom queries") ;; gives label to "Q"
                                         ("Qa" "Archive search"
                                          ;; search
                                          org-search-view
                                          ""
                                          ((org-agenda-files (file-expand-wildcards "~/org-notes/archive/*.org_archive"))))
                                         ("Qw" "Website search"
                                          search
                                          ""
                                          ((org-agenda-files (file-expand-wildcards "~/org-notes/website/*.org"))))
                                         ("Qb" "Projects and Archive"
                                          search
                                          ""
                                          ((org-agenda-text-search-extra-files (file-expand-wildcards "~/org-notes/archive/*.org_archive"))))
                                         ;; searches both projects and archive directories
                                         ("QA" "Archive tags search"
                                          ;; org-tags-view
                                          tags
                                          ""
                                          ((org-agenda-files (file-expand-wildcards "~/org-notes/archive/*.org_archive"))))
                                         ;; ...other commands here
                                         ("pl" tags-todo "PROJECT+DREAM+CATEGORY=\"my\"")
                                         ("W" "Weekly Review"
                                          ((stuck "") ;; review stuck projects as designated by org-stuck-projects
                                           (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
                                           ))))
      (defvar my-website-html-preamble "<div class='nav'>
<ul>
<li><a href='http://my.com'>博客</a></li>
<li><a href='/index.html'>Wiki目录</a></li>
</ul>
</div>")
      (defvar my-website-html-blog-head " <link rel='stylesheet' href='css/site.css' type='text/css'/> \n
    <link rel=\"stylesheet\" type=\"text/css\" href=\"/css/worg.css\"/>")
      (setq org-publish-project-alist `(("blog-notes" :base-directory "~/org-notes"
                                         :base-extension "org"
                                         :publishing-directory "~/org-notes/public_html/"
                                         :recursive t
                                         :html-head ,my-website-html-blog-head
                                         :publishing-function org-html-publish-to-html
                                         :headline-levels 4
                                         :auto-preamble t
                                         :exclude "gtd.org"
                                         :exclude-tags ("ol" "noexport"):section-numbers
                                         nil
                                         :html-preamble ,my-website-html-preamble
                                         :author "my"
                                         :email "guanghui8827@gmail.com"
                                         :auto-sitemap t
                                         :sitemap-filename "index.org"
                                         :sitemap-title "我的wiki"
                                         :sitemap-sort-files anti-chronologically
                                         :sitemap-file-entry-format "%t")
                                        ("blog-static" :base-directory "~/org-notes"
                                         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
                                         :publishing-directory "~/org-notes/public_html/"
                                         :recursive t
                                         :publishing-function org-publish-attachment)
                                        ("blog" :components ("blog-notes" "blog-static"))))
      (defun zilong/org-summary-todo (n-done n-not-done)
        "Switch entry to DONE when all subentries are done, to TODO otherwise."
        (let (org-log-done org-log-states) ; turn off logging
          (org-todo (if (= n-not-done 0)
                        "DONE"
                      "TODO"))))
      (add-hook 'org-after-todo-statistics-hook
                'zilong/org-summary-todo)
      ;; used by zilong/org-clock-sum-today-by-tags
      (defun zilong/filter-by-tags ()
        (let ((head-tags (org-get-tags-at)))
          (member current-tag head-tags)))
      (defun zilong/org-clock-sum-today-by-tags (timerange &optional tstart tend noinsert)
        (interactive "P")
        (let* ((timerange-numeric-value (prefix-numeric-value timerange))
               (files (org-add-archive-files (org-agenda-files)))
               (include-tags '("WORK" "EMACS" "DREAM" "WRITING" "MEETING"
                               "LIFE" "PROJECT" "OTHER"))
               (tags-time-alist (mapcar (lambda (tag)
                                          `(,tag . 0))
                                        include-tags))
               (output-string "")
               (tstart (or tstart
                           (and timerange
                                (equal timerange-numeric-value 4)
                                (- (org-time-today)
                                   86400))
                           (and timerange
                                (equal timerange-numeric-value 16)
                                (org-read-date nil nil nil "Start Date/Time:"))
                           (org-time-today)))
               (tend (or tend
                         (and timerange
                              (equal timerange-numeric-value 16)
                              (org-read-date nil nil nil "End Date/Time:"))
                         (+ tstart 86400)))
               h
               m
               file
               item
               prompt
               donesomething)
          (while (setq file (pop files))
            (setq org-agenda-buffer (if (file-exists-p file)
                                        (org-get-agenda-file-buffer file)
                                      (error "No such file %s" file)))
            (with-current-buffer org-agenda-buffer
              (dolist (current-tag include-tags)
                (org-clock-sum tstart tend 'zilong/filter-by-tags)
                (setcdr (assoc current-tag tags-time-alist)
                        (+ org-clock-file-total-minutes
                           (cdr (assoc current-tag tags-time-alist)))))))
          (while (setq item (pop tags-time-alist))
            (unless (equal (cdr item) 0)
              (setq donesomething t)
              (setq h (/ (cdr item)
                         60)
                    m
                    (- (cdr item)
                       (* 60 h)))
              (setq output-string (concat output-string
                                          (format "[-%s-] %.2d:%.2d\n"
                                                  (car item)
                                                  h
                                                  m)))))
          (unless donesomething
            (setq output-string (concat output-string "[-Nothing-] Done nothing!!!\n")))
          (unless noinsert
            (insert output-string))
          output-string))
      (define-key org-mode-map (kbd "s-p") 'org-priority)
      (define-key evil-normal-state-map (kbd "C-c C-w") 'org-refile)
      (spacemacs/set-leader-keys-for-major-mode
        'org-mode "owh" 'plain-org-wiki-helm "owf"
        'plain-org-wiki)
      (setq org-default-notes-file "~/org-notes/gtd.org")
      (setq org-agenda-files (quote ("~/org-notes")))
      (setq org-mobile-directory "~/Dropbox/应用/MobileOrg")
      (setq org-mobile-inbox-for-pull "~/org-notes/gtd.org")
      (setq org-directory "~/org-notes")
      (setq org-refile-use-outline-path 'file)
      (setq org-outline-path-complete-in-steps nil)
      (setq org-refile-targets '((nil :maxlevel .
                                      4)
                                 (org-agenda-files :maxlevel .
                                                   4)))
      ;; config stuck project
      (setq org-stuck-projects '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))
      (setq org-agenda-inhibit-startup t) ;; ~50x speedup
      (setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup
      (setq org-agenda-window-setup 'current-window)
      (setq org-log-done t)
      ;; 加密文章
      ;; "http://coldnew.github.io/blog/2013/07/13_5b094.html"
      ;; org-mode 設定
      (require 'org-crypt)
      ;; 當被加密的部份要存入硬碟時，自動加密回去
      (org-crypt-use-before-save-magic)
      ;; 設定要加密的 tag 標籤為 secret
      (setq org-crypt-tag-matcher "secret")
      ;; 避免 secret 這個 tag 被子項目繼承 造成重複加密
      ;; (但是子項目還是會被加密喔)
      (setq org-tags-exclude-from-inheritance (quote ("secret")))
      ;; 用於加密的 GPG 金鑰
      ;; 可以設定任何 ID 或是設成 nil 來使用對稱式加密 (symmetric encryption)
      (setq org-crypt-key nil)
      ;; (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
      (setq org-todo-keywords (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                                      (sequence "WAITING(w@/!)" "SOMEDAY(S)" "|"
                                                "CANCELLED(c@/!)" "MEETING(m)" "PHONE(p)"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Org clock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; Change task state to STARTED when clocking in
      (setq org-clock-in-switch-to-state "STARTED")
      ;; Save clock data and notes in the LOGBOOK drawer
      (setq org-clock-into-drawer t)
      ;; Removes clocked tasks with 0:00 duration
      (setq org-clock-out-remove-zero-time-clocks
            t) ;; Show the clocked-in task - if any - in the header line
      (setq org-tags-match-list-sublevels nil)
      ;; http://wenshanren.org/?p=327
      ;; change it to helm
      (defun my/org-insert-src-block (src-code-type)
        "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
        (interactive (let ((src-code-types '("emacs-lisp" "python" "C" "sh" "java" "js"
                                             "clojure" "C++" "css" "calc" "asymptote" "dot"
                                             "gnuplot" "ledger" "lilypond" "mscgen" "octave"
                                             "oz" "plantuml" "R" "sass" "screen" "sql"
                                             "awk" "ditaa" "haskell" "latex" "lisp" "matlab"
                                             "ocaml" "org" "perl" "ruby" "scheme" "sqlite")))
                       (list (ido-completing-read "Source code type: "
                                                  src-code-types))))
        (progn
          (newline-and-indent)
          (insert (format "#+BEGIN_SRC %s\n" src-code-type))
          (newline-and-indent)
          (insert "#+END_SRC\n")
          (previous-line 2)
          (org-edit-src-code)))
      (add-hook 'org-mode-hook
                '(lambda ()
                   ;; keybinding for editing source code blocks
                   ;; keybinding for inserting code blocks
                   (local-set-key (kbd "C-c i s")
                                  'my/org-insert-src-block)))
      (require 'ox-publish)
      (add-to-list 'org-latex-classes
                   '("ctexart" "\\documentclass[11pt]{ctexart}
                                        [NO-DEFAULT-PACKAGES]
                                        \\usepackage[utf8]{inputenc}
                                        \\usepackage[T1]{fontenc}
                                        \\usepackage{fixltx2e}
                                        \\usepackage{graphicx}
                                        \\usepackage{longtable}
                                        \\usepackage{float}
                                        \\usepackage{wrapfig}
                                        \\usepackage{rotating}
                                        \\usepackage[normalem]{ulem}
                                        \\usepackage{amsmath}
                                        \\usepackage{textcomp}
                                        \\usepackage{marvosym}
                                        \\usepackage{wasysym}
                                        \\usepackage{amssymb}
                                        \\usepackage{booktabs}
                                        \\usepackage[colorlinks,linkcolor=black,anchorcolor=black,citecolor=black]{hyperref}
                                        \\tolerance=1000
                                        \\usepackage{listings}
                                        \\usepackage{xcolor}
                                        \\lstset{
                                        %行号
                                        numbers=left,
                                        %背景框
                                        framexleftmargin=10mm,
                                        frame=none,
                                        %背景色
                                        %backgroundcolor=\\color[rgb]{1,1,0.76},
                                        backgroundcolor=\\color[RGB]{245,245,244},
                                        %样式
                                        keywordstyle=\\bf\\color{blue},
                                        identifierstyle=\\bf,
                                        numberstyle=\\color[RGB]{0,192,192},
                                        commentstyle=\\it\\color[RGB]{0,96,96},
                                        stringstyle=\\rmfamily\\slshape\\color[RGB]{128,0,0},
                                        %显示空格
                                        showstringspaces=false
                                        }
                                        "
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")
                     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
      ;; {{ export org-mode in Chinese into PDF
      ;; @see http://freizl.github.io/posts/tech/2012-04-06-export-orgmode-file-in-Chinese.html
      ;; and you need install texlive-xetex on different platforms
      ;; To install texlive-xetex:
      ;;    `sudo USE="cjk" emerge texlive-xetex` on Gentoo Linux
      ;; }}

      (setq org-latex-default-class "ctexart")
      (setq org-latex-pdf-process '("xelatex -interaction nonstopmode -output-directory %o %f"
                                    "xelatex -interaction nonstopmode -output-directory %o %f"
                                    "xelatex -interaction nonstopmode -output-directory %o %f"
                                    "rm -fr %b.out %b.log %b.tex auto"))
      (setq org-latex-listings t)
      ;;reset subtask
      (setq org-default-properties (cons "RESET_SUBTASKS" org-default-properties))
      (defun org-reset-subtask-state-subtree ()
        "Reset all subtasks in an entry subtree."
        (interactive "*")
        (if (org-before-first-heading-p)
            (error "Not inside a tree")
          (save-excursion
            (save-restriction (org-narrow-to-subtree)
                              (org-show-subtree)
                              (goto-char (point-min))
                              (beginning-of-line 2)
                              (narrow-to-region (point)
                                                (point-max))
                              (org-map-entries '(when (member (org-get-todo-state) org-done-keywords)
                                                  (org-todo (car org-todo-keywords))))))))
      (defun org-reset-subtask-state-maybe ()
        "Reset all subtasks in an entry if the `RESET_SUBTASKS' property is set"
        (interactive "*")
        (if (org-entry-get (point)
                           "RESET_SUBTASKS")
            (org-reset-subtask-state-subtree)))
      (defun org-subtask-reset ()
        (when (member org-state org-done-keywords) ;; org-state dynamically bound in org.el/org-todo
          (org-reset-subtask-state-maybe)
          (org-update-statistics-cookies t)))
      ;; (add-hook 'org-after-todo-state-change-hook 'org-subtask-reset)
      (setq org-plantuml-jar-path (expand-file-name "~/.spacemacs.d/plantuml.jar"))
      (setq org-ditaa-jar-path "~/.spacemacs.d/ditaa.jar")
      (org-babel-do-load-languages 'org-babel-load-languages
                                   '((perl . t)
                                     (ruby . t)
                                     (sh . t)
                                     (js . t)
                                     (python . t)
                                     (emacs-lisp . t)
                                     (plantuml . t)
                                     (C . t)
                                     (ditaa . t))))))

(defun my/post-init-css-mode ()
  (progn
    (dolist (hook '(css-mode-hook sass-mode-hook less-mode-hook))
      (add-hook hook 'rainbow-mode))
    (defun css-imenu-make-index ()
      (save-excursion
        (imenu--generic-function '((nil "^ *\\([^ ]+\\) *{ *$" 1)))))
    (add-hook 'css-mode-hook
              (lambda ()
                (setq imenu-create-index-function 'css-imenu-make-index)))))

(defun my/post-init-tagedit ()
  (add-hook 'web-mode-hook
            (lambda ()
              (tagedit-mode 1))))

;; For each extension, define a function my/init-<extension-name>
;;
; (defun my/init-doxymacs ()
  ; "Initialize doxymacs"
  ; (use-package doxymacs
    ; :init (add-hook 'c-mode-common-hook 'doxymacs-mode):config
    ; (progn
      ; (defun my-doxymacs-font-lock-hook ()
        ; (if (or (eq major-mode 'c-mode)
                ; (eq major-mode 'c++-mode))
            ; (doxymacs-font-lock)))
      ; (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
      ; (spacemacs|hide-lighter doxymacs-mode))))

(defun my/init-plain-org-wiki ()
  (use-package plain-org-wiki
    :init (setq pow-directory "~/org-notes")))



(defun my/init-sr-speedbar ()
  (progn
    (require 'sr-speedbar)))


(defun my/post-init-command-log ()
  (with-eval-after-load 'command-log-mode
    (setq clm/log-command-exceptions* (append clm/log-command-exceptions*
                                              '(evil-next-visual-line evil-previous-visual-line)))))

(defun my/post-init-pangu-spacing ()
  (progn
    ;; add toggle options
    (spacemacs|add-toggle toggle-pangu-spaceing
      :status pangu-spacing-mode
      :on (global-pangu-spacing-mode):off
      (global-pangu-spacing-mode -1)
      :documentation "Toggle pangu spacing mode"
      :evil-leader "ots")
    (add-hook 'markdown-mode-hook
              '(lambda ()
                 (set (make-local-variable 'pangu-spacing-real-insert-separtor)
                      t)))))

(defun my/init-litable ()
  (use-package litable :init :defer
    t))

(defun my/init-osx-dictionary ()
  (use-package osx-dictionary
    :init (progn
            (evilified-state-evilify osx-dictionary-mode
              osx-dictionary-mode-map)
            (setq osx-dictionary-use-chinese-text-segmentation
                  t)
            (global-set-key (kbd "C-c d")
                            'osx-dictionary-search-pointer))))

; (defun my/post-init-popwin ()
  ; (progn
    ; (push "*my/run-current-file output*" popwin:special-display-config)
    ; (delete "*Async Shell Command*" 'popwin:special-display-config)))

(defun my/post-init-avy ()
  (progn
    (global-set-key (kbd "C-s-'")
                    'avy-goto-char-2)))

(defun my/post-init-ace-window ()
  (global-set-key (kbd "C-x C-o")
                  #'ace-window))

(defun my/init-discover-my-major ()
  (use-package discover-my-major
    :defer t
    :init (progn
            (spacemacs/set-leader-keys (kbd "mhm")
              'discover-my-major)
            (evilified-state-evilify makey-key-mode makey-key-mode-get-key-map))))

(defun my/post-init-ycmd ()
  (progn
    (setq ycmd-tag-files 'auto)
    (setq ycmd-request-message-level -1)
    (set-variable 'ycmd-server-command
                  `("python" ,(expand-file-name "~/Github/ycmd/ycmd/__main__.py")))
    (setq company-backends-c-mode-common '((company-c-headers company-dabbrev-code company-keywords
                                                              company-gtags :with company-yasnippet) company-files
                                                              company-dabbrev))
    (my|toggle-company-backends company-ycmd)
    (eval-after-load 'ycmd
      '(spacemacs|hide-lighter ycmd-mode))
    (spacemacs/set-leader-keys-for-major-mode
      'c-mode "tb" 'my/company-toggle-company-ycmd)
    (spacemacs/set-leader-keys-for-major-mode
      'c++-mode "tb" 'my/company-toggle-company-ycmd)))

(defun my/post-init-lua-mode ()
  (progn
    (add-hook 'lua-mode-hook 'evil-matchit-mode)
    (add-hook 'lua-mode-hook 'smartparens-mode)
    (setq lua-indent-level 4)
;;; add lua language, basic, string and table keywords.
    (with-eval-after-load 'lua-mode
      (push '(lua-mode "setmetatable" "local" "function"
                       "and" "break" "do" "else" "elseif" "self"
                       "resume" "yield" "end" "false" "for" "function"
                       "goto" "if" "nil" "not" "or" "repeat" "return"
                       "then" "true" "until" "while" "__index" "dofile"
                       "getmetatable" "ipairs" "pairs" "print" "rawget"
                       "status" "rawset" "select" "_G" "assert" "collectgarbage"
                       "error" "pcall" "coroutine" "rawequal" "require"
                       "load" "tostring" "tonumber" "xpcall" "gmatch"
                       "gsub" "rep" "reverse" "sub" "upper" "concat"
                       "pack" "insert" "remove" "unpack" "sort" "lower")
            company-keywords-alist))
    (spacemacs/set-leader-keys-for-major-mode
      'lua-mode "<tab>" 'hs-toggle-hiding "gg" 'helm-gtags-dwim
      "gr" 'helm-gtags-find-rtag "gs" 'helm-gtags-find-symbol
      "gf" 'helm-gtags-find-files)))

(defun my/post-init-elfeed ()
  (use-package elfeed
    :init (global-set-key (kbd "C-x w")
                          'elfeed):defer
                          t
                          :config (progn
                                    (setq elfeed-feeds '(""
                                                         ;; "http://nullprogram.com/feed/"
                                                         ;; "http://z.caudate.me/rss/"
                                                         ;; "http://irreal.org/blog/?feed=rss2"
                                                         ;; "http://feeds.feedburner.com/LostInTheTriangles"
                                                         ;; "http://tonybai.com/feed/"
                                                         ;; "http://planet.emacsen.org/atom.xml"
                                                         ;; "http://feeds.feedburner.com/emacsblog"
                                                         ;; "http://blog.binchen.org/rss.xml"
                                                         ;; "http://oremacs.com/atom.xml"
                                                         ;; "http://blog.gemserk.com/feed/"
                                                         ;; "http://www.masteringemacs.org/feed/"
                                                         ;; "http://t-machine.org/index.php/feed/"
                                                         ;; "http://gameenginebook.blogspot.com/feeds/posts/default"
                                                         ;; "http://feeds.feedburner.com/ruanyifeng"
                                                         ;; "http://coolshell.cn/feed"
                                                         ;; "http://blog.devtang.com/atom.xml"
                                                         ;; "http://emacsist.com/rss"
                                                         ;; "http://puntoblogspot.blogspot.com/feeds/2507074905876002529/comments/default"
                                                         ;; "http://angelic-sedition.github.io/atom.xml"
                                                         ))
                                    ;; (evilify elfeed-search-mode elfeed-search-mode-map)
                                    (evilified-state-evilify-map elfeed-search-mode-map
                                      :mode elfeed-search-mode
                                      :bindings "G"'elfeed-update "g" 'elfeed-search-update--force)
                                    (defun my/elfeed-mark-all-as-read ()
                                      (interactive)
                                      (mark-whole-buffer)
                                      (elfeed-search-untag-all-unread))
                                    (define-key elfeed-search-mode-map (kbd "R") 'my/elfeed-mark-all-as-read)
                                    (defadvice elfeed-show-yank
                                        (after elfeed-show-yank-to-kill-ring activate
                                               compile)
                                      "Insert the yanked text from x-selection to kill ring"
                                      (kill-new (x-get-selection)))
                                    (ad-activate 'elfeed-show-yank))))

(defun my/post-init-evil ()
  (progn
    (setcdr evil-insert-state-map nil)
    (define-key evil-insert-state-map [escape] 'evil-normal-state)
    (push "TAGS" spacemacs-useless-buffers-regexp)
    ;; ;; change evil initial mode state
    (loop for
          (mode . state)
          in
          '((shell-mode . normal))
          do
          (evil-set-initial-state mode state))
    ;;mimic "nzz" behaviou in vim
    (defadvice evil-ex-search-next
        (after advice-for-evil-search-next activate)
      (evil-scroll-line-to-center (line-number-at-pos)))
    (defadvice evil-ex-search-previous
        (after advice-for-evil-search-previous activate)
      (evil-scroll-line-to-center (line-number-at-pos)))
    (define-key evil-normal-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
    (define-key evil-normal-state-map (kbd "Y") 'my/yank-to-end-of-line)
    ;; rebind g,k to gj and gk
    (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
    (define-key evil-normal-state-map (kbd "[ SPC") (lambda ()
                                                      (interactive)
                                                      (evil-insert-newline-above)
                                                      (forward-line)))
    (define-key evil-normal-state-map (kbd "] SPC") (lambda ()
                                                      (interactive)
                                                      (evil-insert-newline-below)
                                                      (forward-line -1)))
    (define-key evil-normal-state-map (kbd "[ b") 'spacemacs/previous-useful-buffer)
    (define-key evil-normal-state-map (kbd "] b") 'spacemacs/next-useful-buffer)
    ;; (define-key evil-insert-state-map "\C-e" 'end-of-line)
    ;; (define-key evil-insert-state-map "\C-n" 'next-line)
    ;; (define-key evil-insert-state-map "\C-k" 'kill-line)

    (define-key evil-emacs-state-map (kbd "s-f") 'forward-word)
    (define-key evil-insert-state-map (kbd "s-f") 'forward-word)
    (define-key evil-emacs-state-map (kbd "s-b") 'backward-word)
    (define-key evil-insert-state-map (kbd "s-b") 'backward-word)
    (spacemacs/set-leader-keys "bi" 'ibuffer)
    (define-key evil-ex-completion-map "\C-a"
      'move-beginning-of-line)
    (define-key evil-ex-completion-map "\C-b"
      'backward-char)
    (define-key evil-ex-completion-map "\C-k"
      'kill-line)
    (define-key minibuffer-local-map (kbd "C-w") 'evil-delete-backward-word)
    (define-key evil-visual-state-map (kbd ">") 'my/shift-right-visual)
    (define-key evil-visual-state-map (kbd "<") 'my/shift-left-visual)
    (define-key evil-visual-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
    ;; (define-key evil-visual-state-map (kbd "x") 'er/expand-region)
    ;; (define-key evil-visual-state-map (kbd "X") 'er/contract-region)
    (define-key evil-visual-state-map (kbd "C-r") 'my/evil-quick-replace)
    ;; in spacemacs, we always use evilify miscro state
    (evil-add-hjkl-bindings package-menu-mode-map
      'emacs)
    ;; (define-key evil-emacs-state-map (kbd "C-w h") 'evil-window-left)
    (define-key evil-emacs-state-map (kbd "C-w") 'evil-delete-backward-word)
    ;; (define-key evil-emacs-state-map (kbd "C-w j") 'evil-window-down)
    ;; (define-key evil-emacs-state-map (kbd "C-w k") 'evil-window-up)
    ;; (define-key evil-emacs-state-map (kbd "C-w l") 'evil-window-right)

    ;; for emacs shell mode
    ;; (define-key evil-emacs-state-map (kbd "s-b") 'ido-switch-buffer)
    ;; (define-key evil-emacs-state-map (kbd "s-f") 'ido-find-file)
    (evil-define-key 'emacs
      term-raw-map
      (kbd "C-w")
      'evil-delete-backward-word)
    (spacemacs/set-leader-keys "fR" 'my/rename-file-and-buffer)
    (spacemacs/set-leader-keys "bms" 'bookmark-set)
    (spacemacs/set-leader-keys "bmr" 'bookmark-rename)
    (spacemacs/set-leader-keys "bmd" 'bookmark-delete)
    ;; This will break visual column edit
    ;; enable hybrid editing style
    ;; (defadvice evil-insert-state (around my/holy-mode activate)
    ;;   "Preparing the holy water flasks."
    ;;   (evil-emacs-state))
    ;; disable c-[ temporally
    ;; (define-key input-decode-map [?\C-\[] (kbd "<C-[>"))
    ;; (bind-keys ("<C-[>" . evil-normal-state))
    ;; (setq evil-emacs-state-cursor '("chartreuse3" (bar . 2)))
    ;; (define-key evil-emacs-state-map [escape] 'evil-normal-state)
    ))

(defun my/init-helm-github-stars ()
  (use-package helm-github-stars
    :defer t
    :config (progn
              (setq helm-github-stars-username "myme5261314")
              (setq helm-github-stars-cache-file "~/.emacs.d/.cache/hgs-cache"))))

;; (defun my/init-org-octopress ()
;;   (use-package org-octopress
;;     :commands (org-octopress):init
;;     :defer t
;;     :config (progn
;;               (evilified-state-evilify org-octopress-summary-mode
;;                 org-octopress-summary-mode-map)
;;               (add-hook 'org-octopress-summary-mode-hook
;;                         #'(lambda ()
;;                             (local-set-key (kbd "q")
;;                                            'bury-buffer)))
;;               (setq org-blog-dir "~/4gamers.cn/")
;;               (setq org-octopress-directory-top org-blog-dir)
;;               (setq org-octopress-directory-posts (concat org-blog-dir "source/_posts"))
;;               (setq org-octopress-directory-org-top org-blog-dir)
;;               (setq org-octopress-directory-org-posts (concat org-blog-dir "blog"))
;;               (setq org-octopress-setup-file (concat org-blog-dir "setupfile.org"))
;;               (defun my/org-save-and-export ()
;;                 (interactive)
;;                 (org-octopress-setup-publish-project)
;;                 (org-publish-project "octopress" t))
;;               (spacemacs/set-leader-keys "op" 'my/org-save-and-export))))

(defun my/post-init-lispy ()
  (with-eval-after-load 'lispy
    (progn
      (define-key lispy-mode-map (kbd "s-1") 'lispy-describe-inline)
      (define-key lispy-mode-map (kbd "s-2") 'lispy-arglist-inline))))

(defun my/init-hydra ()
  (use-package hydra
    :init (progn
            ;; major mode hydra is really cool, don't need to switch mode anymore
            ;; C-c [a-z] and s-[a-z] is very quick to pressed even in emacs-state and F1-F9 is also the same
            ;; If the command will change the buffer, they should be put in these groups.
            ;; otherwise, use which-key + spacems + user defined key mappings in evil normal mode
            (defhydra hydra-yasnippet
              (:color blue :hint nil)
              "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
         _a_ll
"
              ("d" yas-load-directory)
              ("e" yas-activate-extra-mode)
              ("i" yas-insert-snippet)
              ("f" yas-visit-snippet-file :color blue)
              ("n" yas-new-snippet)
              ("t" yas-tryout-snippet)
              ("l" yas-describe-tables)
              ("g" yas/global-mode)
              ("m" yas/minor-mode)
              ("a" yas-reload-all))
            ;; (bind-key* "<f3>" 'hydra-yasnippet/body)
            (defhydra hydra-apropos
              (:color blue)
              "Apropos"
              ("a" apropos "apropos")
              ("c" apropos-command "cmd")
              ("d" apropos-documentation "doc")
              ("e" apropos-value "val")
              ("l" apropos-library "lib")
              ("o" apropos-user-option "option")
              ("u" apropos-user-option "option")
              ("v" apropos-variable "var")
              ("i" info-apropos "info")
              ("t" tags-apropos "tags")
              ("z" hydra-customize-apropos/body "customize"))
            (defhydra hydra-customize-apropos
              (:color blue)
              "Apropos (customize)"
              ("a" customize-apropos "apropos")
              ("f" customize-apropos-faces "faces")
              ("g" customize-apropos-groups "groups")
              ("o" customize-apropos-options "options"))
            (bind-key* "<f4>" 'hydra-apropos/body))))

(defun my/post-init-company-c-headers ()
  (progn
    (setq company-c-headers-path-system (quote ("/usr/include/" "/usr/local/include/" "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1")))
    (setq company-c-headers-path-user (quote ("/Users/my/cocos2d-x/cocos/platform" "/Users/my/cocos2d-x/cocos"
                                              "." "/Users/my/cocos2d-x/cocos/audio/include/")))))

(defun my/post-init-nodejs-repl ()
  (progn
    (spacemacs/declare-prefix-for-mode 'js2-mode
      "ms" "REPL")
    (spacemacs/set-leader-keys-for-major-mode
      'js2-mode "sb" 'nodejs-repl-eval-buffer "sf"
      'nodejs-repl-eval-function "sd" 'nodejs-repl-eval-dwim)))

(defun my/post-init-visual-regexp-steroids ()
  (progn
    (define-key global-map (kbd "C-c r") 'vr/replace)
    (define-key global-map (kbd "C-c q") 'vr/query-replace)))

(defun my/init-multiple-cursors ()
  (use-package multiple-cursors
    :init (progn
            (bind-key* "C-s-l" 'mc/edit-lines)
            (bind-key* "C-s-f" 'mc/mark-all-dwim)
            (bind-key* "C-s-." 'mc/mark-next-like-this)
            (bind-key* "C-s-," 'mc/mark-previous-like-this)
            (bind-key* "s->" 'mc/unmark-next-like-this)
            (bind-key* "s-<" 'mc/unmark-previous-like-this)
            (bind-key* "C-c C-s-." 'mc/mark-all-like-this)
            ;; http://endlessparentheses.com/multiple-cursors-keybinds.html?source=rss
            (define-prefix-command 'endless/mc-map)
            ;; C-x m is usually `compose-mail'. Bind it to something
            ;; else if you use this command.
            (define-key ctl-x-map "m" 'endless/mc-map)
;;; Really really nice!
            (define-key endless/mc-map "i" #'mc/insert-numbers)
            (define-key endless/mc-map "h" #'mc-hide-unmatched-lines-mode)
            (define-key endless/mc-map "a" #'mc/mark-all-like-this)
;;; Occasionally useful
            (define-key endless/mc-map "d" #'mc/mark-all-symbols-like-this-in-defun)
            (define-key endless/mc-map "r" #'mc/reverse-regions)
            (define-key endless/mc-map "s" #'mc/sort-regions)
            (define-key endless/mc-map "l" #'mc/edit-lines)
            (define-key endless/mc-map "\C-a" #'mc/edit-beginnings-of-lines)
            (define-key endless/mc-map "\C-e" #'mc/edit-ends-of-lines))))

;; (defun my/post-init-persp-mode ()
;;   (when (fboundp 'spacemacs|define-custom-layout)
;;     (spacemacs|define-custom-layout "@Cocos2D-X"
;;       :binding "c"
;;       :body
;;       (find-file "~/cocos2d-x/cocos/ui/UIWidget.cpp")
;;       (split-window-right)
;;       (find-file "~/cocos2d-x/cocos/cocos2d.cpp"))))

(defun my/post-init-youdao-dictionary ()
  (spacemacs/set-leader-keys "oy" 'youdao-dictionary-search-at-point+))

(defun my/post-init-cc-mode ()
  (progn
    (setq company-backends-c-mode-common '((company-dabbrev-code :with company-keywords
                                                                 company-gtags) company-files
                                                                 company-dabbrev))
    ;; http://stackoverflow.com/questions/23553881/emacs-indenting-of-c11-lambda-functions-cc-mode
    (defadvice c-lineup-arglist
        (around my activate)
      "Improve indentation of continued C++11 lambda function opened as argument."
      (setq ad-return-value (if (and (equal major-mode 'c++-mode)
                                     (ignore-errors (save-excursion
                                                      (goto-char (c-langelem-pos langelem))
                                                      ;; Detect "[...](" or "[...]{". preceded by "," or "(",
                                                      ;;   and with unclosed brace.
                                                      (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
                                0 ; no additional indent
                              ad-do-it))) ; default behavior

    (setq c-default-style "linux") ;; set style to "linux"
    (setq c-basic-offset 4)
    (c-set-offset 'substatement-open 0)
    (with-eval-after-load 'c++-mode
      (define-key c++-mode-map (kbd "s-.") 'company-ycmd)))
  ;; company backend should be grouped
  )


(defun my/post-init-evil-escape ()
  (setq evil-escape-delay 0.2))

;; (defun my/post-init-org-bullets ()
;;   (setq org-bullets-bullet-list '("🐉" "🐠" "🐬" "🐤")))

(defun my/post-init-find-file-in-project ()
  (progn
    (defun my/search-in-fireball ()
      (interactive)
      (helm-do-ag (expand-file-name "~/Github/fireball/")))
    (spacemacs/set-leader-keys "os" 'my/search-in-fireball)
    ;; If you use other VCS (subversion, for example), enable the following option
    ;;(setq ffip-project-file ".svn")
    ;; in MacOS X, the search file command is CMD+p
    (bind-key* "s-p" 'find-file-in-project)
    ;; for this project, I'm only interested certain types of files
    ;; (setq-default ffip-patterns '("*.html" "*.js" "*.css" "*.java" "*.xml" "*.js"))
    ;; if the full path of current file is under SUBPROJECT1 or SUBPROJECT2
    ;; OR if I'm reading my personal issue track document,
    (defadvice find-file-in-project
        (before my-find-file-in-project activate compile)
      (when (ffip-current-full-filename-match-pattern-p
             "\\(/fireball\\)")
        ;; set the root directory into "~/projs/PROJECT_DIR"
        (setq-local ffip-project-root "~/Github/fireball")
        ;; well, I'm not interested in concatenated BIG js file or file in dist/
        (setq-local ffip-find-options "-not -size +64k -not -iwholename '*/bin/*'")
        ;; do NOT search files in below directories, the default value is better.
        ;; (setq-default ffip-prune-patterns '(".git" ".hg" "*.svn" "node_modules" "bower_components" "obj"))
        )
      (when (ffip-current-full-filename-match-pattern-p
             "\\(/cocos2d-x\\)")
        ;; set the root directory into "~/projs/PROJECT_DIR"
        (setq-local ffip-project-root "~/cocos2d-x")
        ;; well, I'm not interested in concatenated BIG js file or file in dist/
        (setq-local ffip-find-options "-not -size +64k -not -iwholename '*/bin/*'")
        ;; do NOT search files in below directories, the default value is better.
        ;; (setq-default ffip-prune-patterns '(".git" ".hg" "*.svn" "node_modules" "bower_components" "obj"))
        ))
    (ad-activate 'find-file-in-project)))

(defun my/post-init-deft ()
  (progn
    (setq deft-use-filter-string-for-filename
          t)
    (spacemacs/set-leader-keys-for-major-mode
      'deft-mode "q" 'quit-window)
    (setq deft-extension "org")
    (setq deft-directory "~/org-notes")))

(defun my/post-init-org-pomodoro ()
  (progn
    (add-hook 'org-pomodoro-finished-hook
              '(lambda ()
                 (my/growl-notification "Pomodoro Finished"
                                        "☕️ Have a break!" t)))
    (add-hook 'org-pomodoro-short-break-finished-hook
              '(lambda ()
                 (my/growl-notification "Short Break" "🐝 Ready to Go?"
                                        t)))
    (add-hook 'org-pomodoro-long-break-finished-hook
              '(lambda ()
                 (my/growl-notification "Long Break" " 💪 Ready to Go?"
                                        t)))))

(defun my/init-org-tree-slide ()
  (use-package org-tree-slide
    :init (spacemacs/set-leader-keys "oto" 'org-tree-slide-mode)))


(defun my/post-init-prodigy ()
  (progn
    (prodigy-define-tag :name 'jekyll
      :env '(("LANG" "en_US.UTF-8")
             ("LC_ALL" "en_US.UTF-8")))
    ;; define service
    (prodigy-define-service :name "Preview cocos2d-x web"
      :command "python"
      :args '("-m" "SimpleHTTPServer" "6001"):cwd
      "~/cocos2d-x/web"
      :tags '(work):kill-signal'sigkill
      :kill-process-buffer-on-stop t)
    (prodigy-define-service :name "Preview creator engine"
      :command "python"
      :args '("-m" "SimpleHTTPServer" "6004"):cwd
      "~/Github/fireball/engine"
      :tags '(work):kill-signal'sigkill
      :kill-process-buffer-on-stop t)
    (prodigy-define-service :name "Hexo Server"
      :command "hexo"
      :args '("server"):cwd
      "~/4gamers.cn"
      :tags '(hexo server):kill-signal'sigkill
      :kill-process-buffer-on-stop t)
    (prodigy-define-service :name "Hexo Deploy"
      :command "hexo"
      :args '("deploy" "--generate"):cwd
      "~/4gamers.cn"
      :tags '(hexo deploy):kill-signal'sigkill
      :kill-process-buffer-on-stop t)
    (prodigy-define-service :name "Debug Fireball"
      :command "npm"
      :args '("start" "--" "--nologin" "/Users/my/Github/example-cases"):cwd
      "~/Github/fireball/"
      :tags '(work):kill-signal'sigkill
      :kill-process-buffer-on-stop t)
    (prodigy-define-service :name "Org wiki preview"
      :command "python"
      :args '("-m" "SimpleHTTPServer" "8088"):cwd
      "~/org-notes/public_html"
      :tags '(org-mode):init
      (lambda ()
        (browse-url "http://localhost:8088"))
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)))

(defun my/init-moz-controller ()
  (use-package moz-controller
    :init (moz-controller-global-mode t):diminish
    moz-controller-mode))

(defun my/init-vue-mode ()
  (use-package vue-mode
    :config
    ;; 0, 1, or 2, representing (respectively) none, low, and high coloring
    (setq mmm-submode-decoration-level 2)))
(defun my/post-init-fill-column-indicator ()
  )


(defun org/post-init-org-brain ()
  (use-package org-brain
    :ensure t
    :init
    (setq org-brain-path "~/org-notes/brain")
    (with-eval-after-load 'evil
      (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
    :config
    (setq org-id-track-globally t)
    (setq org-id-locations-file "~/org-notes/.brain-org-id-locations")
    (with-eval-after-load 'org-capture-templates
      (push '("b" "Brain" plain (function org-brain-goto-end)
              "* %i%?" :empty-lines 1)
            org-capture-templates)
      )
    (setq org-brain-visualize-default-choices 'all)
    (setq org-brain-title-max-length 24)
    )
)


(defun my/post-init-persp-mode ()
  (setq wg-morph-on nil)
  (setq persp-autokill-buffer-on-remove 'kill-weak)
  (add-hook 'after-init-hook
            #'(lambda ()
                (persp-mode 1))))
