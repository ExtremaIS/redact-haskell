---
title: REDACT
section: 1
hyphenate: false
...

# NAME

`redact` - hide secret text on the terminal

# SYNOPSIS

`redact` [*COLOR_OPTIONS*] [*FILE_OPTION*]
:   redact input text using the configured color

`redact` [*COLOR_OPTIONS*] `--test`
:   redact test text using the configured color

`redact` `--colors`
:   redact test text using all possible colors

# DESCRIPTION

Redact is a utility for hiding secret text on the terminal.

It is designed to work with Markdown syntax.  Inline code (text enclosed in
backticks) and code blocks that are fenced with backticks are hidden.  Note
that inline code that spans multiple lines is not supported, and other types
of code blocks are not hidden.

Text is hidden by displaying it using the same color for the foreground and
background.  When selecting hidden text, it remains hidden in some terminals
and becomes visible in other terminals.

Be sure to test it in your terminal before relying on it.

# OPTIONS

-h, \--help
:   show help and exit

\--version
:   show version and exit

-c, \--color *COLOR*
:   redacted text color

    The following colors are supported:

    * `black`
    * `red`
    * `green`
    * `yellow`
    * `blue`
    * `magenta`
    * `cyan`
    * `white`

-i, \--intensity *INTENSITY*
:   redacted text color intensity

    The following intensities are supported:

    * `dull`
    * `vivid`

-f, \--file *PATH*
:   input file (default: `STDIN`)

-l, \--lenient
:   do not exit on parse errors

    The following values are supported for the environment variable and
    configuration:

    * `true`
    * `false`

\--colors
:   redact test text using all possible colors

\--test
:   redact test text using the configured color

# CONFIGURATION

Settings priority:

1. command-line options
    * `--color`
    * `--intensity`
    * `--lenient`
2. environment variables
    * `REDACT_COLOR`=`COLOR`
    * `REDACT_INTENSITY`=`INTENSITY`
    * `REDACT_LENIENT`=`LENIENT`
3. settings file (`redact.ini`)
    * `color=COLOR`
    * `intensity=INTENSITY`
    * `lenient=LENIENT`
4. defaults
    * color: `red`
    * intensity: `vivid`
    * lenient: `false`

# EXIT CODES

0
:   no error

2
:   command-line error

# PROJECT

GitHub:
:   <https://github.com/ExtremaIS/redact-haskell>

Reporting issues:
:   GitHub: <https://github.com/ExtremaIS/redact-haskell/issues>

    Email: <bugs@extrema.is>

Copyright
:   Copyright (c) 2020-2022 Travis Cardwell

License
:   The MIT License <https://opensource.org/licenses/MIT>
