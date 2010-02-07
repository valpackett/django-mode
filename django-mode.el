;; django-mode.el - Major mode for Django web framework.
;; Copyright 2010 MyFreeWeb
;;
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

(require 'python-mode)

(define-derived-mode django-mode python-mode "Django" "Major mode for Django web framework.")
(add-hook 'django-mode-hook
	  (lambda ()
	    (font-lock-add-keywords nil
				    '(("\\<\\(django\\|models\\|request\\)" 1 font-lock-type-face)
				      ("\\<\\(get_list_or_404\\|get_object_or_404\\|redirect\\|render_to_response\\)" . font-lock-builtin-face))
				    ))
)

(add-to-list 'auto-mode-alist '("\\<\\(models.py\\|views.py\\|handlers.py\\|feeds.py\\|sitemaps.py\\|admin.py\\|urls.py\\|settings.py\\|tests.py\\|assets.py\\)" . django-mode))

;;
;; a part from http://garage.pimentech.net/libcommonDjango_django_emacs/
;; little modified

(defun django-insert-trans (from to &optional buffer)
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char from)
      (iso-iso2sgml from to)
      (insert "{% trans \"")
      (goto-char (point-max))
      (insert "\" %}")
      (point-max))))
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
(add-hook 'sgml-mode-hook
	  (lambda ()
	    (local-set-key "\C-c\C-t" 'django-insert-trans)
	    (setq indent-tabs-mode nil)
	    ))
(add-hook 'django-mode-hook
	  '(lambda ()
	     (local-set-key "\C-c\C-t" 'django-insert-transpy)
	     ))
;; this part ends here

(provide 'django-mode)
;; django-mode.el ends here