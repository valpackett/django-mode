# Django mode for Emacs #
## How to install ##
1. Install [yasnippet](http://code.google.com/p/yasnippet/)
2. Add something like this to your Emacs config:
        (require 'django-html-mode)
        (require 'django-mode)
        (yas/load-directory "path-to/django-mode/snippets")
        (add-to-list 'auto-mode-alist '("\\.djhtml$" . django-html-mode))

## How to use ##
- Enjoy snippets
- `C-x j` for jumping to templates, eg. move your cursor to a line that contains `render_to_response('some.html')`, do the keystroke and it will open some.html. (it supports `@render_to` from [annoying](http://bitbucket.org/offline/django-annoying), classic `render_to_response` and the new 1.3 `TemplateResponse`)
