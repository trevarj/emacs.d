# emacs.d

![screenshot](./screenshots/config.png)

## Desktop themes

`trev/desktop-theme-load` follows the selected `trev-nix` theme bundle.
All eight core Modus variants load their installed native themes, ignoring
bundle-local Lisp sources.
Font preferences are reapplied after palette updates so reloads retain both
fonts and colors. Restart Emacs after updating the loader before selecting new themes.

Run the isolated theme regressions from this checkout:

```sh
emacs -Q --batch -l lisp/desktop-theme-tests.el -f ert-run-tests-batch-and-exit
```
