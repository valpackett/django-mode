# Django mode for Emacs

## How to install

This package is in MELPA: `M-x package-install RET django-mode RET`.

1. Install [yasnippet](http://code.google.com/p/yasnippet/)
2. Add something like this to your Emacs config:

```lisp
(require 'django-html-mode)
(require 'django-mode)
(yas/load-directory "path-to/django-mode/snippets")
(add-to-list 'auto-mode-alist '("\\.djhtml$" . django-html-mode))
```

## Usage
You can call individual commands or enable the django-mode and use keyboard shortcuts.

If you like it, you have a [hydra](https://github.com/abo-abo/hydra/)
that groups the available commands and allow to run many in a row,
with a few key pressed: `M-x django-hydra/body` or `C-c h`.

## Jumping
Move your cursor to a line that contains the thing you want to go and press `C-x j`.
Django-mode supports jumping to:

- templates, eg. `render_to_response('some.html')` will open some.html. (it supports `@render_to` from [annoying](http://bitbucket.org/offline/django-annoying), classic `render_to_response` and the new 1.3 `TemplateResponse`)
- views (from urls.py), urls.py and views.py must be in the same directory (no global urls.py for all apps, it's a bad practice after all!)
- models, from `Model.objects.*` or `Model(*)`, no support for `get_object_or_404(Model*` yet

## Inserting translation marks
Select a string you want to translate and press `C-t`. This works in both Python and templates.

## Running management commands
Check out the Django menu :)
BTW, default keybindings:

- `C-c t` runs tests
- `C-c s` runs syncdb
- `C-c a` creates an app (asking for a name first)
- `C-c m` asks for a command to run (with completion of all available commands and ido completion)
