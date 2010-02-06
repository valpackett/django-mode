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