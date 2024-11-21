# MVRE - bulk file renaming using regular expression or any Perl statements

## SYNOPSIS

  perl MVRE.pm [options] replace-expr files ...

## DESCRIPTION

This program/module renames each files given in the command line,
according to the expression given in replace-expr.

Replace-expr may one of `s///` expression, `tr///` expression,
pre-defined shortcuts, or any Perl statements which will update `$_`.

# OPTIONS

## --force, -f

By default, this module will detect any duplicated target (possibly
caused by mis-specification of the replace-expr) or already-existing
files on the target, and aborts the operation if any.  By specifying
this option, this module will overwrite any existing files as
specified.

Note that this mechanism is only protection for bad specifications,
not for any race conditions.

## --test, --dry-run, -n

Instead of renaming the actual files, it only shows what will happen.

## --no-ext. -x

Exclude extension parts of the filenames (e.g. C<.txt> or C<.tar.gz>)
from replacement.  These will be stripped before applying replace-expr
and restored after that.

Some heuristics are used for determining the extension parts.  Either
believe it or check it first with `--dry-run` option.

## --no-dir. -p

Exclude directory path parts (anything before slash) from replacement.
These will be stripped Before applying replace-expr and restored after
that.
If a filename is ending with a slash, it will be silently skipped.

## APIs

This module can be either used as standalone, or with additional
pre-defined shortcuts extended by users.

As standalone, it can be used as `perl MVRE.pm ....`.

To extend, create a script snippet like below (namely, `mvre`).

    #!/usr/bin/perl
    use lib "<path to MVRE.pm>";
    use MVRE;

    ..., put additional shortcut definitions here ...

    MVRE::main();

Additional shortcut can be defined by `sub` statements or using APIs. for API details, refer the POD in the module script.

## AUTHOR/COPYRIGHT

Copyright 2015-2023 Yutaka OIWA <yutaka@oiwa.jp>.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
<http://www.apache.org/licenses/LICENSE-2.0>

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
