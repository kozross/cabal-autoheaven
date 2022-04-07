# `cabal-autoheaven`

## What is this?

A helper for custom `Setup.hs` scripts to deal with Autotools builds in bundled
libraries.

## Why does this exist?

C libraries contain a lot of useful functionality, which can often either be
hard to replicate in Haskell, relies on low-level features not available in
Haskell (such as SIMD instructions), or would be a bad idea to replicate even if
it were easy ('rolling your own crypto'). Therefore, using the FFI and writing
bindings in such cases is the best choice.

GHC, and Cabal, can compile individual C files just fine. However, most
non-trivial libraries consist of many files, with possibly a lot of flags and
inter-dependencies. Typically, these are managed via a build system: a common
choice, [despite its many
deficiencies](https://web.archive.org/web/20141015085319/https://voices.canonical.com/jussi.pakkanen/2011/09/13/autotools),
are the Autotools. However, if your library of choice is built with the
Autotools, you have two unenviable choices:

* Expect it to be globally installed for your project to work (and Windows users
  now can't use it at all); or
* Try and unpick the Autotools logic so you can bundle it.

Both of these are completely unreasonable. However, in practice, you can drive
an Autotools build with only `sh` and `make`: GHC already comes with everything
else. This _does_ require a somewhat tricky `Setup.hs`, however.
`cabal-autoheaven` essentially deals with the tedious part of such a `Setup.hs`
for you.

## Why the name?

Due to [well-known and
stated](http://www.cs.cmu.edu/afs/club.cc.cmu.edu/usr/cmccabe/blog-notes/autotools_considered_harmful2.html)
problems with the Autotools, it has been nicknamed 'Autohell' by some. This
package is designed to rescue you from that particular hell in the context of a
Cabal build: hence the `autoheaven` part. The `cabal-` was added to help people
find this package, as `autoheaven` isn't very informative by itself.

## What're the goals of this project?

### Support bundling C libraries using the Autotools

If you need an Autotools-built library, `cabal-autoheaven` should make it easy
for you to bundle it, and have the build happen transparently as part of your
project.

### Ease of use

Aside from the location of the source of the bundled library, as well as POSIX
`sh` and `make` on the PATH, users should not need to think about any
peculiarities of the Autotools, on _any_ platform.

### Windows support

While having `sh` and `make` on your PATH is a bit more of an ask on Windows,
beyond this, support for Windows should be seamless.

## What're the non-goals of this project?

### Support for system-wide libraries, even optionally

Dynamic linking, especially in a cross-platform setting, is hellish. In fact,
it is one reason why linking to non-bundled libraries in Haskell with Cabal is
such a royal pain right now. While some would argue that having the option _not_
to use a bundled version of a library if requested, or if a system one exists,
is beneficial, this creates a huge amount of complexity. Supporting this would
be a huge amount of work.

Furthermore, there is another considerable downside. If you bundle a copy of a
library, you know exactly what version and features you are getting. Your OS may
have different ideas: they might provide a very old version of the library, or
one with patches incompatible with yours, or something equally strange. Ensuring
that this is checked for, and rejecting anything that doesn't suit, is such a
difficult task that it's not even clear how you would do this _in general_, much
less in the case of this library.

Between the massive added complexity, the problem of OS distributions ruining
assumptions from library authors, and the niche benefits of having such a thing,
`cabal-autoheaven` will not support system library use, _ever_.

### Supporting languages other than C (and C++)

While theoretically, nothing prevents this, it's an extremely painful
proposition. This is for several reasons:

* Each additional language requires (possibly multiple) checks for programs
  being available;
* Each language's build system is driven differently.
* There's no uniformity of _where_ build products land, or possibly even a good
  way of controlling this.

This would require specialist support, probably from someone who knows the
language in question, which is complicated just in itself. Furthermore, if you
want to FFI into a library from language X from Haskell, language X has to
expose a C API anyway, which isn't always easy or even possible.

While the Autotools theoretically support multiple languages (which makes _some_
of the above problems somewhat easier), unlike with C (and C++ by accident),
there's no guarantee that you'll have a compiler available which can compile
them. For C (and C++ by accident), we have bundled support with GHC, so at least
_that_ part doesn't have to be an issue.

As a result of all the above, we're focusing on C for now, with C++ support
possible in the future.

### Pure `make` builds

Some C libraries use `make`, and only `make`, as their 'build system'. In
theory, there's no reason why we couldn't have such a driver, as we already
require POSIX `make` to be on the PATH. However, in reality, this is either 
redundant or impossible. 

All uses of `make` as a build system amount to one of two possibilities:

* 'Chuck it all together and feed it to the compiler', which is what gets done
  in [`libhydrogen`](https://github.com/jedisct1/libhydrogen); or
* 'Reimplement the Autotools out of spite', which is what gets done in
  [`chibi-scheme`](https://github.com/ashinn/chibi-scheme), or most of
  [suckless](https://suckless.org/).

The first of these contains only simple logic, usually just collecting files and
throwing them at the compiler. In these cases, you can simply bundle the
necessary files as part of your project, and list them in `c-sources` in your
Cabal file. This is portable, fairly straightforward, and doesn't require any
additional help from anything or even a custom `Setup.hs`. The second variety
does more-or-less arbitrary things, without any clear interface or reason. For
these cases, it's very difficult to even determine how they should be driven in
a general sense, much less how to inform them of key facts (such as 'where you
can find your C compiler'), which makes any kind of interface we could provide
in a custom `Setup.hs` either extremely generic (making it hard to use) or
extremely specialized (making it not very useful).

Furthermore, many builds of the second kind (or even the first kind!) tend to
make use of non-POSIX `make` features; this is different to the Autotools, which
_pedantically_ avoids non-POSIX things in the makefiles it generates. This means
that even _if_ there was some 'uniformity of interface' we could rely on,
there's no guarantee that we could even drive the makefile without additional
assurances about what _kind_ of `make` it needed.

As a result, pure `make` builds won't be supported, even if other build systems
will be. Depending on what you're dealing with, either bundle everything, or
tell the maintainers to stop pretending.

## What else could this do?

Any, or all, of the following could be provided if enough people ask:

* Support for other C build systems, such as CMake or Meson.
* Building multiple packages, with possible dependencies on each other.
* (Some) additional build configuration, such as CFLAGS.
* Ability to retrieve source code from a URL via the Internet.
* Ability to retrieve `sh` or `make` on Windows via the Internet.
* Support for C++.
* Check for `busybox` if a `sh` search fails, and use that if found.

Some of these are significantly larger undertakings, so unless there's a _lot_
of demand, some of these probably won't happen.

## How do I use this?

Set up a custom `Setup.hs` file in your project, as well as a directory
containing the source of the Autotools-based library which you want to bundle.
Suppose that this library is named `catboy`, and it's placed in the directory
`cbits/catboy-unstable` in the project's main directory.

Then, put the following in your `Setup.hs`:

```haskell
module Main (main) where

import Distribution.Simple (defaultMainWithHooks, simpleUserHooks, postConf)
import Distribution.Autotools (Library (Library), buildAutotoolsLibrary)
import System.FilePath ((</>))

main = do
  let catboyLib = Library "catboy" ("cbits" </> "catboy-unstable")
  defaultMainWithHooks $ 
  simpleUserHooks {
    postConf = buildAutotoolsLibrary catboyLib sourcePath
    }
```

At build time, you need to have `sh` (that is, POSIX shell) and `make` (that is,
POSIX Make) available in the PATH. If you are on any platform _other_ than
Windows, this should already be the case; on Windows, you have several options:

* You can install GNU Make using
  [Chocolatey](https://community.chocolatey.org/packages/make).
* [Git for Windows](https://git-scm.com/download/win) comes with `sh`. You can
  also get it from [Chocolatey](https://community.chocolatey.org/packages/git).
* You can also get a POSIX shell via
  [`busybox-w32`](https://frippery.org/busybox), which you can also get from
  [Chocolatey](https://community.chocolatey.org/packages/busybox). This option
  is less ideal, as it requires symlinking `busybox sh` to `sh`.
* You can also get `make` from [MinGW](https://repo.msys2.org/mingw/mingw64).

Out of these, using Chocolatey is probably the least annoying.

## What does this run on?

Our CI currently checks Windows, Linux (on Ubuntu) and macOS. We check the
following GHC versions:

* 8.10.7
* 9.0.2
* 9.2.2

## What can I do with this?

This library is licensed under the Apache license, version 2 (SPDX code
`Apache-2.0`). For more details, please see the `LICENSE` file.
