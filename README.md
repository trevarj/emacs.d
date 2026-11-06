# emacs.d

![screenshot](./screenshots/config.png)

## Desktop themes

`trev/desktop-theme-selection` resolves and validates
`~/.local/share/trev-themes/active` once per reload; `trev/desktop-theme-load`
then applies that snapshot with Emacs's built-in theme API.
An explicit named-theme action selects `active -> current`, preserving
Niri's saved catalog selection. Umbriel publishes a private generation under
`$XDG_RUNTIME_DIR/trev-themes/noctalia.XXXXXX`, containing only `umbriel.toml`.
The runtime directories and asset must be owned, real and private. Only an
initial deployment without `active` falls back to `current`; an invalid or
dangling active link fails without changing an enabled theme. Home activation
may repair a missing runtime generation after reboot to current.

For an active Noctalia generation, Emacs reads one data-only
`$XDG_CACHE_HOME/noctalia/emacs-palette.json` snapshot (default
`~/.cache/noctalia/emacs-palette.json`). Its exact schema is
`{"mode":"dark"|"light","colors":{...}}`: six six-digit RGB terminal fields
(`background`, `foreground`, `cursor`, `cursor_text_color`,
`selection_background`, `selection_foreground`) plus `color0` through `color15`.
Missing, duplicate, extra or malformed data is rejected before palette/theme
mutation. The cache must be an owned, private regular file, with owned real
parents that are not group/world-writable; existing cache-directory modes are
never changed. Noctalia's guarded `theme-switch` callback writes it even when
Emacs is absent, using a same-directory atomic replacement. The cache and active
link are individually atomic, not a cross-file transaction.

Dark palettes use installed native `modus-vivendi`; light palettes use
`modus-operandi`. A small theme-local adapter measures colors with
`modus-themes-contrast`, targeting **at least 7:1** for the editor text/surface
combinations it adapts. Already-readable text colors are kept; otherwise a
bounded search shifts them minimally toward white (dark mode) or black (light
mode), retaining hue differences where possible. An editor background with
insufficient highlight headroom (including a wrong-polarity background) moves
toward its mode's black/white endpoint to reserve 10:1 headroom. Dim, inactive
and active UI surfaces then step down that headroom,
instead of flattening all backgrounds or guessing fixed RGB blend percentages.

Comments, syntax, default/secondary text and mode-line text are checked against
the shared UI surfaces, including completion selection and the highlighted
line: those two faces inherit their foregrounds, unlike the explicitly paired
region. Region selection colors are kept when readable and repaired as a pair
otherwise. Searches, ordinary/refined diffs, semantic markers and other selected
paired native surfaces retain Modus's semantic hues with contrast repair.
Shared accent backgrounds are repaired against their main text for ad-hoc
`hi-*` highlights; the cursor is repaired against the derived editor background.
ANSI `color0`–`color15` remain exact terminal data, **not** repaired editor colors.
Native warmer/cooler/faint hues remain distinct rather than being aliases for a
single ANSI color. The common symbolic search-color preference is retained.
Iosevka/Noto font preferences and local semantic faces are reapplied.

This is not a proof about every package face or arbitrary later face/override
customization. Color contrast also cannot compensate for unsuitable fonts,
screen settings, hardware or ambient lighting. The installed Modus Info manual
explains the design in **Palette overrides**, **DIY Measure color contrast**,
**Is the contrast ratio about adjacent colors?**, **Difference between loading
and enabling**, and **What is the best setup for legibility?** Reloading the
native theme (not merely enabling it) applies changed palette overrides.
Noctalia never supplies executable Lisp to Emacs. Keep its independent native
Emacs template/hook disabled. Kitty's native template/hook still owns Kitty
separately; Emacs does not read the live Kitty output.
Emacs only supports the cursor's background color: its text color follows the
underlying text background. The cache's `cursor_text_color` is validated, but
cannot override that native behavior.

All eight named core Modus variants load their installed native themes, ignoring
bundle-local Lisp. Named generated Modus-derived themes retain their catalog
loader. Original per-theme overrides are restored before named selection;
returning to Noctalia reapplies the cached palette. Errors and quits restore
previous overrides, theme definitions/order and appearance, including refreshes
of the same native symbol.

Fresh Emacs processes restore a valid active Noctalia palette with the shell and
Emacs server offline. Cache presence alone does not select Noctalia: after reboot,
activation may repair the missing runtime selection to current. Noctalia
stop/start preserves valid active colors; full-profile Niri startup restores
only its own desktop/wallpaper, not shared application colors.

The after-init initializer starts the independently guarded Emacs server before
loading shared state; a server failure warns but does not prevent initialization.
`C-c T` launches session-aware `theme-switch` asynchronously. The switcher's
reload skips an absent server and uses `emacsclient --alternate-editor=false`,
never a fallback editor. Restart Emacs with this loader before applying the new
cache-based integration. A Nix rebuild does not reload it; never reload the live
init for verification.

Run the isolated theme regressions from this checkout:

```sh
emacs -Q --batch -l lisp/desktop-theme-tests.el -f ert-run-tests-batch-and-exit
```

The harness discovers `modus-themes`, `spacious-padding` and `diff-hl` under
this checkout's `elpa/` (or the invoking Emacs's installed load path); Emacs
provides ERT, use-package, server and diff-mode. Each test runs in a fresh batch
subprocess with disposable HOME, XDG cache/runtime and server directories.
It reads the real Modus `:preface`, `:config` and `:hook` source forms, never the
full init or a live server. Regressions measure effective dark/light text against
default, highlighted-line and selected-completion backgrounds; paired region,
search, mode-line, diff/refinement, ad-hoc highlight and marker faces; cursor
visibility after background normalization; low-contrast input and boundary
repairs; and retained hue differences. They also cover exact ANSI
transport, cold-process cache restoration without a shell/server, unsafe or
corrupt state, error/quit rollback, native/generated source isolation,
override restoration, faces/fonts, startup ordering/server failures and the
asynchronous picker.
Batch face checks do not replace real GUI evidence. Once desktop activation is
separately authorized, restart Emacs with the updated loader, apply Noctalia
dark/light palettes, inspect text on the listed highlighted surfaces and
unchanged fonts, then restart Emacs with Noctalia stopped to observe cache
restoration.
Explicitly select named native and generated themes and return to Noctalia;
do not expect a session switch alone to reset shared colors.
