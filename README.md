# flycheck-aspell-org

An Org plug-in for flycheck-aspell, the spell checking helper mode in
Emacs using flycheck.

The plug-in filters the aspell output to skip spelling errors that
occur within some Org document structures. These include

- Text marked as inline code (enclosed by the "`~`" markup)
- Text constituting part of link specification (but not description, `[[link][desc]]`)
- Text used within per-file keyword, except title (e.g., `#+keyword: ...`)
- Text inside block structures (which begin with `#+begin_` and end with `#+end_`; src, html, latex, example)

## Installation

Follow the [instruction in the flycheck-aspell's
README](https://github.com/leotaku/flycheck-aspell/) for
flycheck-aspell installation.

Then add the following to your `init.el`:

``` emacs-lisp
(use-package flycheck-aspell-org
  :straight (:host github :repo "okomestudio/flycheck-aspell-org")
  :after (flycheck-aspell)
  :demand t)
```

## Usage

When installed, flycheck-aspell-org activates automatically in all the
Org mode.

## TODOs

- [ ] Add a switch to de/activate the plugin
- [ ] Make the module a minor mode
