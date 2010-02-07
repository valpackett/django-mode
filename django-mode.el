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

(define-derived-mode django-mode python-mode "Django" "Major mode for Django web framework.")
(add-hook 'django-mode-hook
	  (lambda ()
	    (font-lock-add-keywords nil
				    '(("\\<\\(django\\|models\\|request\\)" 1 font-lock-type-face)
				      ("\\<\\(get_list_or_404\\|get_object_or_404\\|redirect\\|render_to_response\\)" . font-lock-builtin-face))
				    ))
)

(add-to-list 'auto-mode-alist '("\\<\\(models.py\\|views.py\\|feeds.py\\|sitemaps.py\\|admin.py\\|urls.py\\|settings.py\\|tests.py\\|assets.py\\)" . django-mode))
;; django-mode.el ends here