;; org-mode
;(add-to-list 'org-modules 'org-habits)
(require 'org)
(require 'dbus)
(setq org-clock-mode-line-total 'current)
(setq org-clock-into-drawer nil)
(setq org-default-priority 68)
(setq org-log-done 'time)
(setq org-directory "~/org/")
(setq org-ellipsis "⤵⤵⤵")
(setq org-todo-keyword-faces (quote(
                                     ("TODO" . "#e74c3c")
                                     ("TO_READ" . "#e74c3c")
                                     ("READING" . "SkyBlue1")
                                     ("FAILED" . "orange")
                                     ("SOMEDAY" . "plum"))))
(custom-set-variables
 '(calendar-week-start-day 1)
 '(holiday-general-holidays nil)
 '(holiday-islamic-holidays nil)
 '(holiday-hebrew-holidays nil)
 '(holiday-bahai-holidays nil)
 '(holiday-oriental-holidays nil)
 '(oriental-holidays nil)    ; get rid of Oriental holidays
 '(org-deadline-warning-days 14)
 '(org-archive-location "~/org/_archive.org::")
 '(org-agenda-include-diary t)
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-show-all-dates t)
 '(org-agenda-files (list (concat org-directory "gtd.org")
                          ))
 '(org-special-ctrl-a/e t)
 ;'(org-file-apps (("\\.fb2\\.zip\\'" . "fbreader %s")))
 '(org-file-apps (append '(
                           ("\\.fb2\\.zip\\'" . "fbreader %s")
                           ("\\.djvu::\\([0-9]+\\)\\'" . "evince  '%s' -p %1")
                           ) org-file-apps))
 '(org-annotate-file-storage-file (concat org-directory "annotated.org"))
 '(org-agenda-custom-commands
   (quote (("W" "Weekly Review"
            ((agenda "" ((org-agenda-ndays 7))) ;; review upcoming deadlines and appointments
             ;; type "l" in the agenda to review logged items 
             (stuck "") ;; review stuck projects as designated by org-stuck-projects
             (tags "@project")  ;; review all projects
             (todo "SOMEDAY")   ;; review someday/maybe items
             (todo "WAITING"))) ;; review waiting items 
           ;; ...other commands here
           ;("d" todo "DELEGATED" nil)
           ("c" todo "DONE|DEFERRED|CANCELLED" nil)
           ("u" tags "university" nil)
           ("p" "Project list" tags "TODO=\"\"+@project" nil)
           ("r" "" tags "release"
            ((org-agenda-prefix-format "%s - %t")
             (org-agenda-sorting-strategy '(time-up))))
           )))
 '(org-default-notes-file (concat org-directory "unsrt.org"))
 '(org-stuck-projects (quote ("+@project/-DONE-CANCELLED" ("TODO" "NEXT" "TO_READ" "READING") nil "")))
 '(org-tag-faces (quote (("@project" . "burlywood1"))))
 '(org-export-latex-default-packages-alist (quote (
                                                   ("AUTO" "inputenc" t)
                                                   ("T1" "fontenc" t)
                                                   ("" "fixltx2e" nil)
                                                   ("" "graphicx" t)
                                                   ("" "longtable" nil)
                                                   ("" "float" nil)
                                                   ("" "wrapfig" nil)
                                                   ("" "soul" t)
                                                   ("" "textcomp" t)
                                                   ("" "marvosym" t)
                                                   ("" "wasysym" t)
                                                   ("" "latexsym" t)
                                                   ("" "amssymb" t)
                                                   ("" "hyperref" nil)
                                                   "\\tolerance=1000" ("T2A" "fontenc" nil)
                                                   ("utf8x" "inputenc" nil)
                                                   ("" "indentfirst" nil)
                                                   ("" "cmap" nil)
                                                   ("" "tabularx" nil)
                                                   ("" "icomma" nil)
                                                   ("" "amssymb" nil)
                                                   ("intlimits" "amsmath" nil)
                                                   ("" "amsfonts" nil)))))

; diary
(setq calendar-week-start-day 1
      calendar-day-name-array ["Воскресенье" "Понедельник" "Вторник"
                               "Среда" "Четверг" "Пятница" "Суббота"]
      calendar-month-name-array ["Январь" "Февраль" "Март" "Апрель" "Май" 
                                 "Июнь" "Июль" "Август" "Сентябрь"
                                 "Октябрь" "Ноябрь" "Декабрь"])
(setq diary-file "~/org/cal")

(setq org-capture-templates
      `(("t" "TODO" entry (file ,(concat "~/org/" "gtd.org")) "* TODO %?\n%U" :empty-lines 0)
        ))

(defun gtd ()
  (delete-other-windows)
  (interactive)
  (find-file "~/org/gtd.org")
  (split-window-right)
  (find-file "~/org/time-log.org")
)

; ORG-mode clocking google-chart
(define-key org-mode-map [f4] 'org-chart-clocks-current-item)
(defun get-day-time-string (time-string)
  (format-time-string
   "%u"
   (seconds-to-time
    (org-float-time
     (apply 'encode-time (org-parse-time-string time-string))))))


(defun org-time-delta-seconds (time-string seconds)
  (format-time-string
   "%Y-%m-%d"
   (seconds-to-time
    (+ (org-float-time (apply 'encode-time
     (org-parse-time-string time-string)))
       seconds))))

(defun org-list-clocks-current-item (check-n-days)
  "Extract clocked hours per day of current item"
  (save-restriction
    (org-narrow-to-subtree)
    (let* ((next-day-secs (* 3600 25))
           (ts (format-time-string "%Y-%m-%d" (seconds-to-time (- (org-float-time (current-time)) (* check-n-days 3600 24)))))
           (te (org-time-delta-seconds ts next-day-secs))
           (res '())
           (dates '()))
      (while (< (org-float-time
                 (apply 'encode-time (org-parse-time-string ts)))
                (org-float-time (current-time)))
        (org-clock-sum ts te) ; slow
        (setq res (cons (/ org-clock-file-total-minutes 60.0) res)
              dates (cons (if (equal (get-day-time-string ts) "1")
                              "M"
                            (substring ts 8 10)) dates)
              ts te
              te (org-time-delta-seconds ts next-day-secs)))
      (list dates res))))

(defun org-chart-clocks-current-item ()
  "Request a Google Chart with the clocked time of the current item."
  (interactive)
  (let* ((x (org-list-clocks-current-item 40))
        (dates (reverse (car x)))
        (res (reverse (cadr x)))
        (active-days (length (remove-if (lambda (x) (= x 0)) res)))
        (sum-hours (reduce (lambda (a b) (+ a b)) res)))
    (browse-url (format "http://chart.apis.google.com/chart?chxl=1:|%s&chxr=0,0,8|1,0,105&chxt=y,x&chbh=15&chs=1000x300&cht=bvg&chco=80C65A&chds=0,8&chd=t:%s&chg=0,12.5&chma=|5,10&chtt=Clocked+Activity+(%d+days,+%d+hours)&chm=h,FF0000,0,%f,1"
                      (mapconcat (lambda (x) x) dates "|")
                      (mapconcat (lambda (x) (format "%s" x)) res  ",")
                      active-days
                      sum-hours
                      (/ sum-hours active-days 8)))))

(defun awesome-eval-lua-code (lua-code)
  (dbus-call-method
   :session
   "org.awesomewm.awful"
   "/org/awesomewm/awful"
   "org.awesomewm.awful.Remote"
   "Eval"
   lua-code)
  )

(defvar my-org-clock-timer)

(defun awesome-start-clocking ()
  (setq my-org-clock-timer (run-with-timer 0 15 'awesome-update-clocking-text))
  (awesome-update-clocking-text)
  )

(defun awesome-stop-clocking ()
  (awesome-clear-clocking-text)
  (cancel-timer my-org-clock-timer)
  (setq my-org-clock-timer nil)
  )

(defun awesome-update-clocking-text ()
  (awesome-eval-lua-code
   (format "myorgclock.text='%s'" (org-clock-get-clock-string)))
  )

(defun awesome-clear-clocking-text ()
  (awesome-eval-lua-code "myorgclock.text=''")
  )

(add-hook 'org-clock-in-hook 'awesome-start-clocking)
(add-hook 'org-clock-out-hook 'awesome-stop-clocking)
(add-hook 'org-clock-cancel-hook 'awesome-stop-clocking)

(provide 'my-org)
