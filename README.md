# emacs.d

![screenshot](./screenshots/config.png)

## Desktop themes

`trev/desktop-theme-load` resolves `~/.local/share/trev-themes/active` once per
reload. An explicit named-theme action selects `active -> current`, preserving
Niri's saved catalog selection; Umbriel publishes a complete private Noctalia
generation under `$XDG_RUNTIME_DIR/trev-themes/noctalia.XXXXXX`. Only owned,
real private directories and four owned, private regular files are accepted:
`umbriel.toml`, `noctalia-theme.el`, `emacs-theme` and `mode`, with exact
selection/mode metadata and a complete generated Lisp footer. Kitty is outside
this bundle and uses its native Noctalia template/hook in full Nix profiles.
Only initial deployments without `active` fall back to `current`; an invalid
or dangling active link fails without changing the enabled theme. Home
activation may initialize a missing runtime generation after reboot to current.

Noctalia stop/start preserves valid active colors. Full-profile Niri startup
restores its own desktop/wallpaper only, not shared application colors.
Explicit manual palette actions remain available; returning to Niri does not
automatically restore its named Emacs theme.

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
Restart Emacs with this loader **before applying a new Kitty-free bundle**.
A Nix rebuild does not reload it; do not reload the live init for verification.

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
dark/light four-asset generation reloads and explicit named-theme selection,
same-symbol error/quit rollback, native Modus isolation, immediate/deferred Org
effects, faces/fonts, startup ordering/server failures and asynchronous picker
behavior. The suite has 19 isolated ERT checks; batch face checks do not replace
a real GUI rendering/session-switch test.
