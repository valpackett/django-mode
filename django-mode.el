;;; django-mode.el --- Major mode for Django web framework.

;; Copyright (C) 2010 MyFreeWeb

;; Author: MyFreeWeb <me@myfreeweb.ru>
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

(require 'python-mode)

(setq django-template-regexp ".*\\(@render_to\\|render_to_response\\|TemplateResponse\\)(['\"]\\([^'\"]*\\)['\"].*
?"
      django-view-regexp ".*(.+, ?['\"]\\([^'\",]+\\)['\"].*).*
?")

(defun django-root (&optional dir home)
  ;; Copied from Rinari and modified accordingly.
  (or dir (setq dir default-directory))
  (if (and (file-exists-p (expand-file-name "settings.py" dir))
           (file-exists-p (expand-file-name "manage.py" dir)))
      dir
    (let ((new-dir (expand-file-name (file-name-as-directory "..") dir)))
      ;; regexp to match windows roots, tramp roots, or regular posix roots
      (unless (string-match "\\(^[[:alpha:]]:/$\\|^/[^\/]+:\\|^/$\\)" dir)
        (django-root new-dir)))))

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

(defun django-jump ()
  (interactive)
  ;; TODO: add more stuff - models, urls, etc
  (if (string-match django-template-regexp (thing-at-point 'line))
      (django-jump-to-template))
  (if (string-match django-view-regexp (thing-at-point 'line))
      (django-jump-to-view)))

(define-derived-mode django-mode python-mode "Django" "Major mode for Django web framework.")
(define-key django-mode-map (kbd "C-x j") 'django-jump)
(add-hook 'django-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(django\\|models\\|request\\)" 1 font-lock-type-face)
                                      ("\\<\\(get_list_or_404\\|get_object_or_404\\|redirect\\|render_to_response\\)" . font-lock-builtin-face))
                                    )))

(add-to-list 'auto-mode-alist '("\\<\\(models\\|views\\|handlers\\|feeds\\|sitemaps\\|admin\\|context_processors\\|urls\\|settings\\|tests\\|assets\\|forms\\).py" . django-mode))

;; A part from http://garage.pimentech.net/libcommonDjango_django_emacs/
;; Modified a little
(defun django-insert-transpy (from to &optional buffer)
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
(define-key django-mode-map (kbd "C-t") 'django-insert-transpy)
;; This part ends here

(provide 'django-mode)
;; django-mode.el ends here