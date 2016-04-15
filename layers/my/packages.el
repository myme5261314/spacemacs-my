(setq my-packages '(projectile ace-window
                               avy
                               pangu-spacing
                               (org :location built-in)
                               youdao-dictionary
                               window-numbering
                               helm
                               helm-config
                               fill-column-indicator
                               sr-speedbar))

(defun my/post-init-org ()
  (with-eval-after-load 'org
    (progn
      (setq org-agenda-files (quote ("~/org-notes")))
      (setq org-default-notes-file "~/org-notes/gtd.org")
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
                                    ;; ("b" "Blog Ideas" entry (file+headline "~/org-notes/notes.org" "Blog Ideas")
                                    ;;  "* TODO [#B] %?\n  %i\n %U"
                                    ;; :empty-lines 1)

                                    ("w" "work"
                                     entry
                                     (file+headline "~/org-notes/gtd.org" "Cocos2D-X")
                                     "* TODO [#A] %?\n  %i\n %U"
                                     :empty-lines 1)
                                    ("c" "Chrome"
                                     entry
                                     (file+headline "~/org-notes/notes.org" "Quick notes")
                                     "* TODO [#C] %?\n %(zilongshanren/retrieve-chrome-current-tab-url)\n %i\n %U"
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
                                         ;; ("b" "Blog" tags-todo "BLOG")
                                         ("p" . "项目安排")
                                         ;; ("pw" tags-todo "PROJECT+WORK+CATEGORY=\"cocos2d-x\"")
                                         ;; ("pl" tags-todo "PROJECT+DREAM+CATEGORY=\"zilongshanren\"")
                                         ("W" "Weekly Review"
                                          ((stuck "") ;; review stuck projects as designated by org-stuck-projects
                                           (tags-todo "PROJECT") ;; review all projects (assuming you use
                                           ;; todo keywords to designate projects)
                                           ))))
      ;; (defvar zilongshanren-website-html-preamble
      ;;         "<div class='nav'>
      ;; <ul>
      ;; <li><a href='http://zilongshanren.com'>博客</a></li>
      ;; <li><a href='/index.html'>Wiki目录</a></li>
      ;; </ul>
      ;; </div>")
      ;;       (defvar zilongshanren-website-html-blog-head
      ;;         " <link rel='stylesheet' href='css/site.css' type='text/css'/> \n
      ;;     <link rel=\"stylesheet\" type=\"text/css\" href=\"/css/worg.css\"/>")
      ;;       (setq org-publish-project-alist
      ;;             `(
      ;;               ("blog-notes"
      ;;                :base-directory "~/org-notes"
      ;;                :base-extension "org"
      ;;                :publishing-directory "~/org-notes/public_html/"

      ;;                :recursive t
      ;;                :html-head , zilongshanren-website-html-blog-head
      ;;                :publishing-function org-html-publish-to-html
      ;;                :headline-levels 4           ; Just the default for this project.
      ;;                :auto-preamble t
      ;;                :exclude "gtd.org"
      ;;                :exclude-tags ("ol" "noexport")
      ;;                :section-numbers nil
      ;;                :html-preamble ,zilongshanren-website-html-preamble
      ;;                :author "zilongshanren"
      ;;                :email "guanghui8827@gmail.com"
      ;;                :auto-sitemap t               ; Generate sitemap.org automagically...
      ;;                :sitemap-filename "index.org" ; ... call it sitemap.org (it's the default)...
      ;;                :sitemap-title "我的wiki"     ; ... with title 'Sitemap'.
      ;;                :sitemap-sort-files anti-chronologically
      ;;                :sitemap-file-entry-format "%t" ; %d to output date, we don't need date here
      ;;                )
      ;;               ("blog-static"
      ;;                :base-directory "~/org-notes"
      ;;                :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
      ;;                :publishing-directory "~/org-notes/public_html/"
      ;;                :recursive t
      ;;                :publishing-function org-publish-attachment
      ;;                )
      ;;               ("blog" :components ("blog-notes" "blog-static"))))
      (defun my/org-summary-todo (n-done n-not-done)
        "Switch entry to DONE when all subentries are done, to TODO otherwise."
        (let (org-log-done org-log-states) ; turn off logging
          (org-todo (if (= n-not-done 0)
                        "DONE"
                      "TODO"))))
      (add-hook 'org-after-todo-statistics-hook
                'my/org-summary-todo)
      ;; used by my/org-clock-sum-today-by-tags
      (defun my/filter-by-tags ()
        (let ((head-tags (org-get-tags-at)))
          (member current-tag head-tags)))
      (defun my/org-clock-sum-today-by-tags (timerange &optional tstart tend noinsert)
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
                                      (error "No such file %ds" file)))
            (with-current-buffer org-agenda-buffer
              (dolist (current-tag include-tags)
                (org-clock-sum tstart tend 'my/filter-by-tags)
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
      (setq org-mobile-directory "~/Dropbox/应用/MobileOrg")
      (setq org-mobile-inbox-for-pull "~/org-notes/gtd.org")
      (setq org-directory "~/org-notes"))))

(defun my/post-init-ace-window ()
  (global-set-key (kbd "C-x C-o") #'ace-window))
(defun my/post-init-avy ()
  (progn
    (global-set-key (kbd "C-s-'") 'avy-goto-char-2)))

(defun my/init-sr-speedbar ()
  (progn
    (require 'sr-speedbar)
    ))
