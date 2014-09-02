;;; django-mode.el --- Major mode for Django web framework.

;; Copyright (C) 2010-2012 Greg V

;; Author: Greg V <floatboth@me.com>
;; Keywords: languages

;; This file is NOT part of GNU Emacs.

;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;   http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.

;;; Code:

(condition-case nil
    (require 'python)
  (error
   (require 'python-mode)))

(require 's)

(defvar django-template-regexp ".*\\(@render_to\\|render_to_response\\|TemplateResponse\\)(['\"]\\([^'\"]*\\)['\"].*
?")

(defvar django-view-regexp ".*(.+, ?['\"]\\([^'\",]+\\)['\"].*).*
?")

(defvar django-model-regexp "^[^.]* \\([^.,]+\\)\\(.objects\\|(\\).*
?")

(defun django-root (&optional dir home)
  ;; Copied from Rinari and modified accordingly.
  (projectile-project-root)

  ;; (or dir (setq dir default-directory))
  ;; (if (and (file-exists-p (expand-file-name "settings.py" dir))
           ;; (file-exists-p (expand-file-name "manage.py" dir)))
      ;; dir
    ;; (let ((new-dir (expand-file-name (file-name-as-directory "..") dir)))
      ;; regexp to match windows roots, tramp roots, or regular posix roots
      ;; (unless (string-match "\\(^[[:alpha:]]:/$\\|^/[^\/]+:\\|^/$\\)" dir)
        ;; (django-root new-dir)))))
      )

(defun django-jump-to-template ()
  (interactive)
  (let ((fname (replace-regexp-in-string django-template-regexp "\\2" (thing-at-point 'line))))
    (let ((projfname (concat (django-root) "templates/" fname))
          (appfname (concat default-directory "templates/" fname)))
      (if (file-exists-p appfname)
          (find-file appfname)
        (find-file projfname)))))

(defun django-jump-to-view ()
  (interactive)
  (let ((vname (replace-regexp-in-string django-view-regexp "\\1" (thing-at-point 'line))))
    (find-file (concat default-directory "views.py"))
    (set-text-properties 0 (length vname) nil vname)
    (re-search-forward (concat vname "(.*):
"))))

(defun django-jump-to-model ()
  (interactive)
  (let ((mname (replace-regexp-in-string django-model-regexp "\\1" (thing-at-point 'line))))
    (find-file (concat default-directory "models.py"))
    (re-search-forward (concat mname "(.*):
"))))

(defun django-jump ()
  (interactive)
  (if (string-match django-template-regexp (thing-at-point 'line))
      (django-jump-to-template))
  (if (string-match django-view-regexp (thing-at-point 'line))
      (django-jump-to-view))
  (if (string-match django-model-regexp (thing-at-point 'line))
      (django-jump-to-model)))


(defun django-python-command ()
  (if (boundp 'python-shell-interpreter)
      (concat python-shell-interpreter " " python-shell-interpreter-args)
    (mapconcat 'identity (cons python-python-command python-python-command-args) " ")))

(defun django-get-commands ()
  (cd-absolute (projectile-project-root)) ;; TODO: don't do this
  (if (file-exists-p "manage.py")
      (progn
         (let ( (help-output (shell-command-to-string "python manage.py -h")) )
          (setq dj-commands-str (with-temp-buffer (progn
                               (insert help-output)
                               (beginning-of-buffer)
                               (delete-region (point) (search-forward "Available subcommands:" nil nil nil))
                               ;; cleanup [auth] and stuff
                               (beginning-of-buffer)
                               (save-excursion
                                 (while (re-search-forward "\\[.*\\]" nil t)
                                   (replace-match "" nil nil)))
                               (buffer-string)
                               )))
          ;; get a list of commands from the output of manage.py -h
          ;; What would be the pattern to optimize this ?
          ;; (message "updated manage.py commands.") ;; called quite often.
          (setq dj-commands-str (s-split "\n" dj-commands-str))
          (setq dj-commands-str (-remove (lambda (x) (string= x "")) dj-commands-str))
          (setq dj-commands-str (mapcar (lambda (x) (s-trim x)) dj-commands-str))
          (sort dj-commands-str 'string-lessp)
           )
         )
      '()
    ))

;; TODO: can only be initialized in a django project.
(defvar django-command-list (django-get-commands))

(defun django-manage (command)
  ;; nil nil: enable user to exit with any command. Still, he can not edit a completed choice.
  (interactive (list (completing-read "Command... " django-command-list nil nil)))
  ;; Now ask to edit the command. How to do the two actions at once ?
  (setq command (read-shell-command "Run command like this: " command))
  (compile (concat (django-python-command) " " (django-root) "manage.py " command)))

(defun django-run-in-shell (command)
  "run the given command in a shell. Useful in development to use breakpoints."
  (setq django-shell-buffer-name (format "*shell %s*" (projectile-project-name)))
  (setq django-shell-buffer (get-buffer django-shell-buffer-name))
  ;; TODO: open shell if needed
  ;; stop shell execution if needed
  (set-buffer django-shell-buffer)
  (comint-kill-input)
  (insert command)
  (comint-send-input)
)


(defun django-get-make-commands ()
  "Extract the commands from the Makefile."
  (cd-absolute (django-root))  ;; TODO: I don't like it
  (with-temp-buffer
    (progn
      (insert-file-contents "Makefile")
  (save-restriction
    (setq dj-results '())               ; don't use global variables TODO:
    (goto-char 1)
    (let ((case-fold-search nil))
      (setq dj-results '())
      (while (search-forward-regexp "^[a-z0-9]+" nil t) ;; regexp is weak
        (progn
          (if (null dj-results) (setq dj-results (list (match-string 0)))
          (setq dj-results (cons (match-string 0) dj-results)))

        ))
      dj-results)))
))

(defun django-make (command)
  "Suggest make commands to run, based on a simple parsing of the Makefile."
  (interactive (list (ido-completing-read "make: " (django-get-make-commands))))
  (cd-absolute (django-root))           ;; TODO: I don't like absolute cd.
  (compile (concat "make" " " command))
)

;; Dynamic menu to list the MAKE commands.
(easy-menu-define django--make-menu global-map "Django make"
  '("Django make"))


(defun django--get-menu ()
  (easy-menu-create-menu
   "Django make"
   (mapcar (lambda (command)
             (vector  command
                     `(lambda () (interactive) (compile (concat "make" " " ,command)) t)))
           (django-get-make-commands))))

(easy-menu-add-item django--make-menu '() (django--get-menu))

(defun django--update-menu ()
  (easy-menu-add-item django--make-menu '() (django--get-menu)))

(add-hook 'menu-bar-update-hook 'django--update-menu)


;; menu for MANAGE.PY commands.
(easy-menu-define django--manage-menu global-map "Django make"
  '("Django manage"))

(defun django--get-menu-manage ()
  (easy-menu-create-menu
   "manage..."
   (mapcar (lambda (command)
             (vector  command
                     `(lambda () (interactive) (django-manage ,command) t)))
           (django-get-commands))))

(easy-menu-add-item django--manage-menu '() (django--get-menu-manage))

(defun django--update-menu-manage ()
  (easy-menu-add-item django--manage-menu '() (django--get-menu-manage)))

(add-hook 'menu-bar-update-hook 'django--update-menu-manage)


(defun django-syncdb ()
  (interactive)
  (django-manage "syncdb --noinput"))

(defun django-flush ()
  (interactive)
  (django-manage "flush --noinput"))

(defun django-reset (name)
  (interactive "sReset app:")
  (django-manage (concat "reset " name " --noinput")))

(defun django-migrate ()
  (interactive)
  (django-manage "migrate"))

(defun django-assets-rebuild ()
  (interactive)
  (django-manage "assets rebuild"))

(defun django-startapp (name)
  (interactive "sName:")
  (django-manage (concat "startapp " name)))

(defun django-makemessages ()
  (interactive)
  (django-manage "makemessages --all --symlinks"))

(defun django-compilemessages ()
  (interactive)
  (django-manage "compilemessages"))

(defun django-test (name)
  (interactive "sTest app:")
  (django-manage (concat "test " name)))

(defun django-shell ()
  (interactive)
  (term (concat (django-python-command) " " (django-root) "manage.py shell")))

(defun django-dbshell ()
  (interactive)
  (term (concat (django-python-command) " " (django-root) "manage.py dbshell")))

(defun django-insert-transpy (from to &optional buffer)
  ;; From http://garage.pimentech.net/libcommonDjango_django_emacs/
  ;; Modified a little
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char from)
      (iso-iso2sgml from to)
      (insert "_(")
      (goto-char (point-max))
      (insert ")")
      (point-max))))

;;;###autoload
(define-derived-mode django-mode python-mode "Django" "Major mode for Django web framework.")
(define-key django-mode-map (kbd "C-t") 'django-insert-transpy)
(define-key django-mode-map (kbd "C-x j") 'django-jump)
(define-key django-mode-map (kbd "C-c m") 'django-manage)
(define-key django-mode-map (kbd "C-c t") 'django-test)
(define-key django-mode-map (kbd "C-c s") 'django-syncdb)
(define-key django-mode-map (kbd "C-c a") 'django-startapp)
(add-hook 'django-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(django\\|models\\|forms\\|request\\)\\>" 1 font-lock-type-face)
                                      ("\\<\\(get_list_or_404\\|get_object_or_404\\|redirect\\|render_to_response\\)\\>" . font-lock-builtin-face))
                                    )))

(easy-menu-define django-menu django-mode-map "Django menu"
  '("Django"
    ["Start an app" django-startapp t]
    ["Run tests" django-test t]
    ["Sync database" django-syncdb t]
    ["Flush database" django-flush t]
    ["Reset database" django-reset t]
    ["Run database migrations" django-migrate t]
    ["Rebuild assets" django-assets-rebuild t]
    ["Make translations" django-makemessages t]
    ["Compile translations" django-compilemessages t]
    ["Open Python shell" django-shell t]
    ["Open database shell" django-dbshell t]
    ["Run other command" django-manage t]
    "-"
    ["Jump" django-jump t]
    ["Insert translation mark" django-insert-transpy t]))

(easy-menu-add django-menu django-mode-map)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\<\\(models\\|views\\|handlers\\|feeds\\|sitemaps\\|admin\\|context_processors\\|urls\\|settings\\|tests\\|assets\\|forms\\)\\.py\\'" . django-mode))

(provide 'django-mode)
;; django-mode.el ends here
