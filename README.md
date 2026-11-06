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
toward its mode's black/white endpoint to reserve 16:1 headroom. The selected
row then retains about 8:1 endpoint headroom, rather than forcing every syntax
color to white/black.

Readable text alone does not make a selection visible. The adapter separately
targets these **background-to-background** distinctions in both modes:

- **Region: at least 3:1** against main, dim/popup, highlighted-line and selected
  completion surfaces, with its own **7:1 foreground/background pair**. Imported
  selection colors are kept when they satisfy those constraints; otherwise the
  background moves toward the regular text endpoint and the selected foreground
  toward the opposite endpoint. A region can reverse text polarity without
  changing unselected syntax colors.
- **Selected completion: at least 2:1** against the main background and **1.9:1**
  against the popup background. Native Modus selection is background-only:
  Vertico/Corfu append it after existing match and annotation faces. Keeping that
  behavior preserves Orderless match hues, bold matches and Marginalia/Corfu
  annotation foregrounds, all repaired for **7:1** on the selected row.
- **Highlighted line: at least 1.25:1** against main, deliberately weaker than
  selection. Comments, syntax and default/secondary text retain their foregrounds,
  repaired for **7:1** against default, highlighted-line and selected-row surfaces.
- **Mouse/secondary highlights: at least 1.25:1** against main, with **7:1**
  paired `fg-main` text. Their native teal/warm hues remain separate instead of
  sharing completion's neutral selected background.

The 2:1 and 1.25:1 boundary targets are deliberate visual distinctions, **not**
WCAG AAA text ratios or a claim of 3:1 non-text contrast everywhere. A stronger
opposite-polarity completion background would clash with existing colored matches
and annotations; adding a selected foreground cannot override those earlier faces
in Vertico/Corfu. Searches, ordinary/refined diffs, semantic markers and other
paired native surfaces retain Modus's separate hues with 7:1 text repair. Search
and mouse highlights are not aliases for the selected row; their hue differences
are not claims of 7:1 contrast between adjacent backgrounds. Shared accent
backgrounds are repaired against their main text for ad-hoc `hi-*` highlights;
the cursor is repaired against the derived editor background.
ANSI `color0`–`color15` remain exact terminal data, **not** repaired editor colors.
Native warmer/cooler/faint hues remain distinct rather than being aliases for a
single ANSI color. The common symbolic search-color preference is retained.
Iosevka/Noto font preferences and local semantic faces are reapplied.

This is not a proof about every package face or arbitrary later face/override
customization. Color contrast also cannot compensate for unsuitable fonts,
screen settings, hardware or ambient lighting. The installed Modus Info manual
explains the native mechanisms in **Palette overrides**, **DIY Make the region
preserve text colors plus other styles**, **DIY Make completion matches more or
less colorful**, **DIY Make mouse highlights more or less colorful**, **DIY Measure
color contrast**, and **Is the contrast ratio about adjacent colors?** The loaded
package's source defines the supported roles and face precedence; a different
profile's older manual may describe different options. Reloading the native theme
(not merely enabling it) applies changed palette overrides.
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

The harness discovers `modus-themes`, `spacious-padding`, `diff-hl`, `vertico`,
`corfu`, `orderless` and `marginalia` under this checkout's `elpa/` (or the invoking
Emacs's installed load path); Emacs provides ERT, use-package, server and diff-mode.
Each test runs in a fresh batch subprocess with disposable HOME, XDG cache/runtime
and server directories. It reads the real Modus `:preface`, `:config` and `:hook`
source forms, never the full init or a live server. Regressions measure the
separate background-boundary and text-readability targets above, including
configured completion matches/annotations and the current cream Noctalia
selection inputs. They also cover paired search, mode-line, diff/refinement,
ad-hoc highlight and marker faces; cursor visibility after background
normalization; low-contrast input and boundary repairs; retained hue differences;
and exact ANSI
transport, cold-process cache restoration without a shell/server, unsafe or
corrupt state, error/quit rollback, native/generated source isolation,
override restoration, faces/fonts, startup ordering/server failures and the
asynchronous picker.
Batch face checks do not replace real GUI evidence. Before deployment, use an
isolated Emacs with disposable HOME/XDG paths and only the extracted theme forms,
not the full init or a live server. Inspect synthetic dark/light and the actual
current Noctalia palette: region boundaries against nearby panels; selected and
unselected Vertico/Corfu rows containing matches and annotations; comments/syntax
on `hl-line`; separate search/mouse highlights; cursor and unchanged fonts in a
new frame. Once desktop activation is separately authorized, restart Emacs with
the updated loader before applying palettes. Check cache restoration after a
restart with Noctalia stopped, explicitly select named native/generated themes,
then return to Noctalia. Do not expect a session switch alone to reset shared
colors.
