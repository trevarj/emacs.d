# emacs.d

![screenshot](./screenshots/config.png)

## Desktop themes

`trev/desktop-theme-load` resolves `~/.local/share/trev-themes/active` once per
reload. Niri uses `active -> current`, preserving its saved catalog selection;
Umbriel uses a complete private Noctalia generation under
`$XDG_RUNTIME_DIR/trev-themes/noctalia.XXXXXX`. Only owned, real private
directories and five owned, private regular files are accepted for Noctalia,
with exact selection/mode metadata and a complete generated Lisp footer.
Only initial deployments without `active` fall back to `current`; an invalid
or dangling active link fails without changing the enabled theme.

All eight core Modus variants load their installed native themes, ignoring
bundle-local Lisp sources. Generated themes, including repeated `noctalia`
revisions, load transactionally: errors and quits restore the prior definition,
theme order and appearance. The upstream footer cannot retain a theme search
path or delayed Org callback, change existing Org preferences, or eagerly load
Org. Iosevka/Noto font preferences and local semantic faces are reapplied after
palette updates and rollback.

The single after-init initializer starts the independently guarded Emacs server
before loading shared state; a server failure warns but does not prevent theme
initialization. `C-c T` launches the session-aware `theme-switch` asynchronously.
The switcher's reload skips an absent server and keeps
`emacsclient --alternate-editor=false`, never starting a fallback editor.
Restart Emacs after deploying loader changes; a Nix rebuild does not reload it.

Run the isolated theme regressions from this checkout:

```sh
emacs -Q --batch -l lisp/desktop-theme-tests.el -f ert-run-tests-batch-and-exit
```

The harness discovers `modus-themes`, `spacious-padding` and `diff-hl` under
this checkout's `elpa/` (or the invoking Emacs's installed load path); Emacs
provides ERT, use-package, server, diff-mode and Org. Each test runs in a fresh
batch subprocess with disposable HOME, XDG runtime and server directories.
It reads the real Modus `:preface`, `:config` and `:hook` source forms, never the
full init or a live server. Regressions cover active snapshots and validation,
dark/light generation reloads and Niri return, same-symbol error/quit rollback,
native Modus isolation, immediate/deferred Org effects, faces/fonts, startup
ordering/server failures and asynchronous picker behavior. Batch face checks
do not replace a real GUI rendering/session-switch test.
