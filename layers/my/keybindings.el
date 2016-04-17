;;; keybindings.el --- my Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 Peng Liu
;;
;; Author: myme5261314 <myme5261314@gmail.com>
;; URL: https://github.com/myme5261314/spacemacs-my
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(bind-key* "C-c l" 'my/insert-chrome-current-tab-url)

(global-set-key (kbd "s-/") 'hippie-expand)

;; A complementary binding to the apropos-command (C-h a)
(define-key 'help-command "A" 'apropos)


(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-l") 'find-library)

(define-key 'help-command (kbd "C-i") 'info-display-manual)

(global-set-key [(shift return)] 'my/smart-open-line)

(define-key global-map (kbd "<f1>") 'my/hotspots)
(define-key global-map (kbd "C-c y") 'youdao-dictionary-search-at-point+)

;; (global-set-key (kbd "C-.") 'company-capf)


;; some easy functions for navigate functions
;;C-M-a beginning-of-defun
;;C-M-e end-of-defun
;;C-M-h mark-defun
(global-set-key (kbd "C-s-h") 'mark-defun)

(global-set-key (kbd "s-l") 'goto-line)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "C-`") 'toggle-input-method)

(global-set-key (kbd "<f5>") 'my/run-current-file)

;; "http://endlessparentheses.com/transposing-keybinds-in-emacs.html?source=rss"
;; (global-set-key "\C-t" #'transpose-lines)
;; (define-key ctl-x-map "\C-t" #'transpose-chars)

(when (spacemacs/system-is-mac)
 (spacemacs/set-leader-keys "o!" 'my/iterm-shell-command))

(spacemacs|add-toggle toggle-shadowsocks-proxy-mode
  :status shadowsocks-proxy-mode
  :on (global-shadowsocks-proxy-mode)
  :off (global-shadowsocks-proxy-mode -1)
  :documentation "Toggle shadowsocks proxy mode."
  :evil-leader "ots")

(global-set-key (kbd "s-s") 'save-buffer)
(bind-key* "s-k" 'scroll-other-window-down)
(bind-key* "s-j"  'scroll-other-window)
(bind-key* "C-c /" 'company-files)

(bind-key* "s-r" 'my/browser-refresh--chrome-applescript)
(spacemacs/set-leader-keys "oac" 'my/browser-refresh--chrome-applescript)

(bind-key* "s-;" 'my/insert-semicolon-at-the-end-of-this-line)
(bind-key* "C-s-;" 'my/delete-semicolon-at-the-end-of-this-line)

(bind-key* "s-," 'my/insert-comma-at-the-end-of-this-line)
(bind-key* "C-s-," 'my/delete-comma-at-the-end-of-this-line)

(bind-key* "C-=" 'er/expand-region)


(bind-key* "M--" 'my/goto-match-paren)

(spacemacs/set-leader-keys "ol" 'my/load-my-layout)

(global-set-key (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "<f9>") 'org-capture)
(global-set-key (kbd "C-c b") 'org-iswitchb)
;; http://emacs.stackexchange.com/questions/220/how-to-bind-c-i-as-different-from-tab
(define-key input-decode-map [?\C-i] [C-i])
(define-key evil-normal-state-map (kbd "C-i") 'evil-jump-forward)
