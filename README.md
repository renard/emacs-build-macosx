# Build Emacs for OSX

This repository allows you to build
[Emacs](http://www.gnu.org/software/emacs/) for Mac OSX. If you are not
familiar with Emacs building process I suggest you to grab a ready-to-use
binary version at [Emacs For Mac OSX](http://emacsformacosx.com/).

## What this version does?

Ok this is an other [Emacs](http://www.gnu.org/software/emacs/) build but
what are the differences with
[Emacs For Mac OSX](http://emacsformacosx.com/)?

- You can choose the version you want to build from any commit within the
  whole Emacs history. Well this is true for most of them (Emacs >= 24).

- A stand alone `Emacs.App` is generated. This means you only need
  dependencies for the building process. Then you can deploy the `Emacs.App`
  on any OSX desktop. For the curious all non-system libraries are stored
  into the `Contents/Resources/lib` directory.

- Some very basic patches are applied:

  - Full screen support: Emacs supports OSX full screen mechanisms but lacks
    of a full screen toggle function. This patch adds `ns-toggle-fullscreen`
    which allows you to toggle full screen from Emacs itself.

  - Show git commit ID in `emacs-version`:
  ```
  GNU Emacs 25.0.50.1 (x86_64-apple-darwin, NS appkit-1404.13 Version 10.11.1 (Build 15B42), git sha1 d5ee655) of 2015-10-28
  ```
 All patches are included in `emacs-patches-directory` (which is located in
 the application bundle at `Contents/Resources/patches`). A list of all
 applied patched is stored in the `emacs-patches-list` variable.

 The `view-emacs-patches` function opens a `dired` buffer in
 `emacs-patches-list`. The `view-emacs-patch` opens a particular patch.


- Users Emacs directory: instead of just searching `init.el` in `~/.emacs.d`
  your configuration files will be searched in that order:

	- `~/.emacs.d-GIT_COMMIT` (i.e. `~/.emacs.d-10292e9`)
	- `~/.emacs.d-EMACS_VERSION` (i.e. `~/.emacs.d-24.4.50.1`)
	- `~/.emacs.d-EMACS_MAJOR_VERSION` (i.e. `~/.emacs.d-24`)
	- `~/.emacs.d-EMACS_MAJOR_MINOR_VERSION` (i.e. `~/.emacs.d-24.4`)
	- `~/.emacs.d`

  This is useful if you need to test your configuration against several
  versions at the same time.

- The C source files are included in `Contents/Resources/src` in the
  application bundle.

- A small emacs wrapper is deployed into `Contents/MacOS/bin/emacs` to
  call emacs from the CLI. You can link this file to your search path.

## Requirements

First you need to install following tools (you may want to use
[brew](http://brew.sh/) for that):

- `automake`
- `jpeg`
- `giflib`
- `libtiff`
- `imagemagick`: you need to activate some options:
  - `--with-hdri`
  - `--with-fontconfig`
  - `--with-ghostscript`
  - `--with-jp2`
  - `--with-liblqr`
  - `--with-rsvg`
  - `--with-libtiff`
  - `--with-libwmf`
  - `--with-little-cms`
  - `--with-little-cms2`
  - `--with-openexr`
  - `--with-perl`
  - `--with-quantum-depth-16`
  - `--with-webp`
- `gnutls`
- `librsvg`
- `texinfo`
- `pkg-config`
- `jansson`

You also need to checkout an Emacs git repository. I suggest you to use
[Savannah's](this) which is the most up-to-date.

	git clone git://git.savannah.gnu.org/emacs.git


## Building Emacs for OSX

Once all requirements are installed you can just run:

	./build-emacs

If you want to build a specific version you just can pass the commit id or
tag to the `build-emacs` script:

	./build-emacs emacs-24.4

## configuration report

Some options are only available on specific OS.

- GNU malloc: Not supported on darwin.
- relocating allocator for buffers: Not supported on darwin (does not work
  with system `malloc`).
- Xaw3d: Not supported on darwin.
- Xpm: Not supported on darwin.
- Cairo: Only for X11 environment.
- Sound: Only for GNU/Linux, FreeBSD, NetBSD, MinGW, Cygwin
- GPM: Only for GNU/Linux console.
- dbus: Only for GNU/Linux.
- gconf: Only for X11 environment.
- GSetting: Only for GNU/Linux.
- selinux: Only for GNU/Linux.
- freetype: Only for X11 environment.
- HarfBuzz: Only for X11 environment.
- m17n-flt: Only for X11 environment.
- otf: Only for X11 environment.
- xft: Only for X11 environment.
- systemd: Not supported on darwin.
- Xwidgets: Not tested.

See [](build/configure.ac) for details.

This is a summary of activated options given during the configure part:

```
Configured for 'x86_64-apple-darwin22.6.0'.

  Where should the build process find the source code?    .
  What compiler should emacs be built with?               gcc  -pipe -march=nocona
  Should Emacs use the GNU version of malloc?             no
    (The GNU allocators don't work with this system configuration.)
  Should Emacs use a relocating allocator for buffers?    no
  Should Emacs use mmap(2) for buffer allocation?         no
  What window system should Emacs use?                    nextstep
  What toolkit should Emacs use?                          none
  Where do we find X Windows header files?                Standard dirs
  Where do we find X Windows libraries?                   Standard dirs
  Does Emacs use -lXaw3d?                                 no
  Does Emacs use -lXpm?                                   no
  Does Emacs use -ljpeg?                                  yes
  Does Emacs use -ltiff?                                  yes
  Does Emacs use a gif library?                           yes -lgif
  Does Emacs use a png library?                           yes -L/usr/local/opt/libpng/lib -lpng16
  Does Emacs use -lrsvg-2?                                yes
  Does Emacs use -lwebp?                                  yes
  Does Emacs use -lsqlite3?                               yes
  Does Emacs use cairo?                                   no
  Does Emacs use -llcms2?                                 yes
  Does Emacs use imagemagick?                             no
  Does Emacs use native APIs for images?                  yes (ns)
  Does Emacs support sound?                               no
  Does Emacs use -lgpm?                                   no
  Does Emacs use -ldbus?                                  yes
  Does Emacs use -lgconf?                                 no
  Does Emacs use GSettings?                               no
  Does Emacs use a file notification library?             yes (kqueue)
  Does Emacs use access control lists?                    yes
  Does Emacs use -lselinux?                               no
  Does Emacs use -lgnutls?                                yes
  Does Emacs use -lxml2?                                  yes
  Does Emacs use -lfreetype?                              no
  Does Emacs use HarfBuzz?                                no
  Does Emacs use -lm17n-flt?                              no
  Does Emacs use -lotf?                                   no
  Does Emacs use -lxft?                                   no
  Does Emacs use -lsystemd?                               no
  Does Emacs use -ljansson?                               yes
  Does Emacs use -ltree-sitter?                           no
  Does Emacs use the GMP library?                         yes
  Does Emacs directly use zlib?                           yes
  Does Emacs have dynamic modules support?                yes
  Does Emacs use toolkit scroll bars?                     yes
  Does Emacs support Xwidgets?                            no
  Does Emacs have threading support in lisp?              yes
  Does Emacs support the portable dumper?                 yes
  Does Emacs support legacy unexec dumping?               no
  Which dumping strategy does Emacs use?                  pdumper
  Does Emacs have native lisp compiler?                   no
  Does Emacs use version 2 of the X Input Extension?      no
  Does Emacs generate a smaller-size Japanese dictionary? no
```

## Version

This works with GNU Emacs versions:

* 25
* 26
* 27
* 28
* 29

Some patches may be adapted in the `patches` directory.

## Copyright

Copyright © 2012-2020 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>.

See [LICENSE](LICENSE).
