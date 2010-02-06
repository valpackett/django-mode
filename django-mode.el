(define-derived-mode django-mode python-mode "Django" "Major mode for Django web framework."
	(font-lock-add-keywords nil
				'(("\\(django\\|models\\|request\\)" . font-lock-type-face)
				  ("\\(get_list_or_404\\|get_object_or_404\\|redirect\\|render_to_response\\)" . font-lock-builtin-face))
	)
)

(add-to-list 'auto-mode-alist '("\\(models.py\\|views.py\\|urls.py\\|settings.py\\|admin.py\\|tests.py\\|assets.py\\)" . django-mode))
;; django-mode.el ends here