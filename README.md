# redact

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![GitHub CI](https://github.com/ExtremaIS/redact-haskell/workflows/CI/badge.svg?branch=main)](https://github.com/ExtremaIS/redact-haskell/actions)

* [Overview](#overview)
* [Requirements](#requirements)
* [Installation](#installation)
    * [Installation From Source](#installation-from-source)
* [Usage](#usage)
* [Project](#project)
    * [Links](#links)
    * [Releases](#releases)
    * [Contribution](#contribution)
    * [License](#license)

## Overview

`redact` is a utility for hiding secret text on the terminal.

It is designed to work with Markdown syntax.  Inline code (text enclosed in
backticks) and code blocks that are fenced with backticks are hidden.  Note
that inline code that spans multiple lines is not supported, and other types
of code blocks are not hidden.

Text is hidden by displaying it using the same color for the foreground and
background.  When selecting hidden text, it remains hidden in some terminals
and becomes visible in other terminals.

Be sure to test it in your terminal before relying on it.

## Requirements

`redact` has only been tested on Linux.  Testing on Windows 10 and macOS will
be added in a future release.

## Installation

### Installation From Source

`redact` can be built from source using [Stack](https://www.haskellstack.org).
For example, you can install the latest release (to `~/.local/bin` on Linux)
as follows:

```
$ git clone https://github.com/ExtremaIS/redact-haskell.git
$ cd redact-haskell
$ stack install
```

## Usage

See the [`redact` man page](doc/redact.1.md) for usage information.

## Project

This project implements a utility program.  There are no plans to expose a
library or put the package on Hackage.

### Links

* GitHub: <https://github.com/ExtremaIS/redact-haskell>

### Releases

All releases are tagged in the `main` branch.  Release tags are signed using
the
[`security@extrema.is` GPG key](http://keys.gnupg.net/pks/lookup?op=vindex&fingerprint=on&search=0x1D484E4B4705FADF).

### Contribution

Issues and feature requests are tracked on GitHub:
<https://github.com/ExtremaIS/hr-haskell/issues>

Issues may also be submitted via email to <bugs@extrema.is>.

### License

This project is released under the
[MIT License](https://opensource.org/licenses/MIT) as specified in the
[`LICENSE`](LICENSE) file.
