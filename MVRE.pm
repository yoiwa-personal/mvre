#!/usr/bin/perl
################################################################
# MVRE: bulk rename files using regular expression or any Perl statements.
################################################################

use 5.016;

use strict;

package MVRE v1.0.1;

BEGIN {
    eval { require Sub::Util; require Scalar::Util; };
    if ($@) {
	eval <<'';
	sub Sub::Util::set_subname($$) { return $_[1] }
	sub Scalar::Util::reftype($) { return ref $_[0] }

    }
}

sub _make_sub {
    # as less lexical variables as possible; before all our and my

    local $_ = eval "use strict; package main; sub { { $_[0] ;} return \$_ };";
    die "syntax error on given expression: $@" if $@;
    die "failed compilation of given expression" unless
      (ref($_) eq 'CODE' && Scalar::Util::reftype($_) eq 'CODE');
    return $_;
}

use Exporter ();

our @EXPORT = qw(dsay);
our @EXPORT_OK = qw(def_proc def_regexp cache Dumper);
our %EXPORT_TAGS =
  (
   all => [],
   apis => [qw(def_proc def_regexp cache dsay)],
   minimal_apis => [qw(dsay)],
   shortcuts => [],
   no_attributes => [],
   # predefined shortcuts are added later
  );

sub import (@) {
    my $no_attributes = grep { $_ eq ':no_attributes' } @_;
    unless ($no_attributes) {
	push @UNIVERSAL::ISA, 'MVRE::AttributeHandler';
    }
    goto &Exporter::import;
}

use Getopt::Long 'GetOptionsFromArray';
Getopt::Long::Configure qw/bundling require_order/;

our %desc;
BEGIN { %desc = (); }
our %cache = ();

our @cacheargs; # shared with main and cache
our $DEBUG = 0;

## the main processing

sub error_func (@);

sub compute_replace($$%) {
    my ($exp, $args, %opts) = @_;
    my @args = @$args;
    my @ppargs = ();
    die "compute_replace: bad argument" unless (exists $opts{noext} && exists $opts{nodir});
    my $noext = $opts{noext};
    my $nodir = $opts{nodir};
    my @ret = ();

    my $subr = _make_sub($exp);

    foreach my $t (@args) {
	local $_ = $t;
	my $pre = '';
	my $post = '';

	if ($nodir and /\//) {
	    ($pre, $_) = m@\A(.*/)([^/]*)\Z@sa;
	    next if $_ eq '';
	}
	if ($noext and /\./) {
	    ($_, $post) = m@\A(.*?)((?:\.[\-\w\d]*[a-zA-Z][\-\w\d]*(?:\.(?:gz|bz\d?))?)?(?:,v)?)\Z@sa;
	}
	die "assert failed" if "$pre$_$post" ne $t;

	push @cacheargs, $_;
	push @ppargs, [$pre, $_, $post];
    }

    # @cacheargs must be available here, before calling &$subr

    foreach my $t (@ppargs) {
	my ($pre, $post);
	($pre, $_, $post) = @$t;
	dsay (4, ":  pre=$pre _=$_ post=$post");
	my $from = $pre . $_ . $post;
	$_ = eval { &$subr($_) };
	if ($@) {
	    error_func "cannot rename \"$from\": $@";
	    next;
	} elsif ($_ eq '') {
	    error_func "cannot rename \"$from\": function resulted to nothing";
	}
	dsay (4, ":  pre=$pre _=$_ post=$post");
	my $to = $pre . $_ . $post;

	push @ret, [$from, $to];
    }
    return @ret;
}

sub main {
    my @args = @main::ARGV;

    my $force = 0;
    my $debug_no_precheck = 0;
    my $test = 0;
    my $help = 0;
    my $noext = 0;
    my $nodir = 0;

    GetOptionsFromArray(
	\@args,
	'force|f' => \$force,
	'test|dry-run|t|n' => \$test,
	'no-ext|x' => \$noext,
	'no-dir|p' => \$nodir,
	'debug|D+' => \$DEBUG,
	'debug-no-precheck' => \$debug_no_precheck, # -f only for preparation-phase
	'help|h' => \$help);

    my $exp = shift @args;

    if ($exp eq '' or $help) {
	show_help();
	exit 1;
    }

    my %table;
    my %ftable;

    *error_func = $test ? \&CORE::warn : \&CORE::die;

    my @pargs = compute_replace($exp, \@args, noext => $noext, nodir => $nodir);

    # duplicate check
    foreach my $a (@pargs) {
	my ($from, $to) = @$a;
	if (!$force && !$debug_no_precheck) {
	    error_func "cannot rename \"$from\" to \"$to\": file missing\n" unless (-e "$from");
	    if ($from ne $to) {
		# Here we just check for accidental overwriting with bad expression.
		error_func "cannot rename \"$from\": target filename \"$to\" already exists.\n" if (-e "$to");
		error_func "cannot rename \"$from\": target filename \"$to\" overwraps with \"$table{$to}\".\n" if (exists $table{$to});
	    }
	}
	$table{$to} = $from;
	$ftable{$from} = $to;
    }

    my $rename_func = $force ? \&CORE::rename : \&MVRE::RenameNoReplace::rename_noreplace;
    # On Linux (not too old ones), overwriting existing files will be
    # prevented using the dedicated system call.
    # It is impossible with pure POSIX system APIs.

    foreach my $a (@pargs) {
	my ($from, $to) = @$a;
	unless ($from eq $to) {
	    unless ($test) {
		if (!$force && !$debug_no_precheck) {
		    error_func "cannot rename \"$from\" to \"$to\": file missing (!)\n" unless (-e "$from");
		}
		unless (&$rename_func($from, $to)) {
		    die "\Q$from\E -> \Q$to\E: rename failed: $!.\n";
		}
	    }
	    print "$from -> $to\n";
	}
    }
}

sub show_help {
    my $self = $0;
    my $base = _base_path_fname($self);
    my $invoke = $self;
    if ((_path_search($base) // "") eq $self) {
	$invoke = $base;
    } elsif (-x $invoke) {
	0;
    } else {
	$invoke = "perl $self";
    }

    print STDERR <<"EOF";
$base: move (rename) files regarding to regexp

Usage: $invoke [options] 'expr' files...

   options:
       -t --test: not move but only shows the renames
       -f --force: no check for overwriting
       -h --help: show this help
       -x --no-ext: exclude extensions from processing
       -p --no-dir: exclude path components from processing

   expr is one of:
          s/from/to/
          tr/..../..../
          \$_ = ... (evaluated as perl statements, using \$_ as (in|out)put)
          a shortcut keyword

   shortcut keywords:
EOF
    foreach my $k (sort keys %desc) {
	print STDERR "      $k => $desc{$k}\n";
    }
    print STDERR "\n";
    if ($DEBUG >= 2) {
	print STDERR "    Support for No-replace rename: " , ($MVRE::RenameNoReplace::rename_noreplace_supported || "(none)"), "\n";
    }
}

sub _path_search {
    my $f = $_[0];
    return $f if $f =~ /\// and -x $f;
    return (grep { ($_ = "$_/$f"), -x $_ } (split(":", $ENV{PATH})))[0];
}

sub _base_path_fname {
    my $f = $_[0];
    my ($pre, $fname) = $f =~ m@\A(.*/)?([^/]*)\Z@;
    return $fname;
}

package MVRE::RenameNoReplace {
    # atomic system call for no-replace rename.

    sub rename_noreplace ($$);

    BEGIN {
	our $rename_noreplace_supported = undef;

	if ($^O eq 'linux') {
	    eval {
		require 'syscall.ph';
		require POSIX;
		my $SYS_renameat2 = &SYS_renameat2(); # check for existence
		my $AT_FDCWD = -100;      # linux specific value
		my $RENAME_NOREPLACE = 1; # linux specific value

		sub _rename_noreplace_linux ($$) {
		    my ($from, $to) = @_;
		    $from = $from . "";
		    $to = $to . "";
		    my $r = syscall($SYS_renameat2, $AT_FDCWD, $from, $AT_FDCWD, $to, $RENAME_NOREPLACE);
		    return $r == 0;
		}
		*rename_noreplace = \&_rename_noreplace_linux;
		$rename_noreplace_supported = "linux($SYS_renameat2)";
	    };
	} elsif ($^O eq 'MSWin32') {
	    eval {
		# not tested
		require Win32API::File;
		sub _rename_noreplace_win32 ($$) {
		    my ($from, $to) = @_;
		    return Win32API::File::MoveFileEx($from, $to, Win32API::File::MOVEFILE_REPLACE_EXISTING());
		}
		*rename_noreplace = \&_rename_noreplace_win32;
		$rename_noreplace_supported = "Win32API::File";
	    };
	}
        # TODO: BSD/MacOS (renameatx_np)

	sub _rename_noreplace_general ($$) {
	    my ($from, $to) = @_;
	    if (-e $to) {
		require Errno;
		$! = &Errno::EEXIST;
		return undef;
	    }
	    rename($from, $to);
	}
	*rename_noreplace = \&_rename_noreplace_general unless defined $rename_noreplace_supported;
    }
}

## APIs for extension writers

# dsay: show diagnostic message when --debug is given

# "undocumented" features of dsay:
# - first argument can be an integer for the debug messsage level
#    levels: 1 = user-defined/predefined procedure
#            3 = MVRE::cache internal state-keeping
#            4 = MVRE's input processing or other internal
# - message can be a code reference, which will be called only when needed.

sub dsay (@) {
    my $level = 1;
    if (@_ >= 2 and $_[0] =~ /\A\d+\z/) {
	$level = 0 + shift @_;
    }
    if ($DEBUG >= ($level // 1)) {
	if (ref($_[0]) eq 'CODE') {
	    @_ = &{$_[0]};
	}
	print STDERR join(" ", @_), "\n"
    }
}


# MVRE::Dumper: auto-loaded Data::Dumper

our $_Dumper;

sub Dumper {
    unless ($_Dumper) {
	dsay 4, "loading Data::Dumper";
	require Data::Dumper;
	$_Dumper = Data::Dumper->new([0]);
	$_Dumper->Terse(1);
	$_Dumper->Indent(0);
    }
    $_Dumper->Reset()->Values($_[0])->Dump();
}

# def_proc(glob, sub, comment)

# use as:
#   def_proc *name, sub {
#     ...code...
#   }, "comment";
#
#   Define a shortcut with name, when invoked,
#   to perform a Perl-written operations in code.
#   The "comment" is shown for --help.
#
#   The code shall update $_ according to desired operation.

# If you do not need to support --help, simply
#   sub name {
#     ...
#   }
# will work.  also, as an experimental support,
#   sub digits : Desc(comment) {
#     ...
#   }
# will also work.

sub def_proc(*&$) {
    my ($a, $proc, $help) = @_;
    my ($name, $package);

    if (ref($a) eq 'GLOB') {
	$name = *$a{NAME};
	$package = *$a{PACKAGE};
    } else {
	no strict 'refs';
	$package = caller;
	$name = $a;
	$a = \*{"${package}::${name}"};
    }
    die unless ref($a) eq 'GLOB';

    no warnings 'redefine';
    $proc = Sub::Util::set_subname("${package}::${name}", $proc);
    *$a = $proc;
    $desc{$name} = $help;
}

# def_regexp(name, expr_str)
# define a shortcut with "name", doing r/// or tr/// specified in expr_str.

sub def_regexp($$) {
    my ($name, $r) = @_;
    my $proc = _make_sub($r);
    @_ = ($name, $proc, $r);
    goto &def_proc; # to keep caller() as is
}

# cache(proc)
#
#  usage: $v (or @v) = cache { ...code... }
#
#  To be called within the code in def_proc, to implement
#  one-time computation, or preparation using all of the given file arguments.
#
#  When called for the first time,
#  all file arguments are passed as @_.
#  The code can return any value or array,
#  and that will be memorized and returned from the function "cache".
#
#  For called for second time or later, the previous result is
#  returned without re-computation.
#
#  see the code for "digits" below for an example.

sub cache(&) {
    my ($f) = @_;
    my (@caller0) = caller(0);
    my (@caller1) = caller(1);
    my $key = "$caller1[3]\@$caller0[1]:$caller0[2]=($f)";
    my @r;
    if (!defined $cache{$key}) {
	my @a = map { $_ . "" } @cacheargs;
	if (wantarray) {
	    @r = &$f(@a);
	} else {
	    @r = (scalar &$f(@a));
	}
	$cache{$key} = \@r;
	dsay(3, sub {"cache store with key = $key, value = " . Dumper([\@r])});
    } else {
	@r = @{$cache{$key}};
    	dsay(3, sub {"cache hit   with key = $key, value = " . Dumper([\@r])});
    }
    return wantarray ? @r : $r[0];
}

# attribute-based declaration for shortcut subroutines; see sub import() above for activation.

use Attribute::Handlers;
sub MVRE::AttributeHandler::Desc :ATTR(CODE,RAWDATA) {
    my ($package, $symbol, $referent, $attr, $data, $phase,
	$filename, $linenum) = @_;
    return unless ref $symbol;
    $MVRE::desc{*{$symbol}{NAME}} = $data;
}

# predefined regular expressions
{
    my %keys = (
	'lower', 'tr/A-Z/a-z/',
	'upper', 'tr/a-z/A-Z/',
	'nospecial', 's/[^\w\d\-_.\/]+/_/ga',
	'nospecial_euc', 's/[^\w\d\-_.\/\221-\376]+/_/ga',
	'nospecial_utf', 's/[^\w\d\-_.\/\200-\376]+/_/ga',
	'urlencode', 's/([^\w\d\-_.\/])/sprintf("%%%02X",ord $1)/gea',
	'urlencodeP', 's/([^\w\d\-_.\/%])/sprintf("%%%02X",ord $1)/gea',
       );

    foreach my $k (keys %keys) {
	def_regexp($k, $keys{$k});
    }
}

## default and example definitions

# digits: rename all file names with numbers to have same number of digits.
#        e.g. a1.txt and a10.txt will be renamed to a01.txt and a10.txt.
#  This is an example to use "cache" feature to inspect all filenames beforehand.

MVRE::def_proc *digits, sub {
    my $ndigits = MVRE::cache {
	my $n = 1;
	foreach my $f (@_) {
	    while($f =~ /(\d+)/g) {
		$n = length($1) if $n < length($1);
	    }
	}
	dsay "digits: number of digits: $n";
	return $n
    };
    s/(\d+)/sprintf("%0*d", $ndigits, $1)/ge;
}, '(make numbers in filenames the same length)';

# Experimentally,
#
#   sub digits : Desc((make numbers in filenames the same length)) {
#    ...
#   }
#
# will also work.

# Japanese code conversions

{
    use Encode::Guess;
    MVRE::def_proc *utf, sub {
	my $enc = guess_encoding($_, qw/euc-jp shiftjis utf8 7bit-jis/);
	if ($enc) {
	    $_ = $enc->decode($_);
	}
    }, '(convert to UTF-8 using Encode::Guess)';
};

eval {
    require NKF;
    MVRE::def_proc *mime_decode, sub {
	s/\=[\_?X]ISO-2022-JP[\_?X]B[\_?X]([0-9A-Za-z\/+]+)=*([\_?X]=)?/NKF::nkf('-mB','-Jw',"$1")/egi;
	#nkf('-Mb -e',$1)/eg;
    }, '(decode MIME B encoding)';
};

# Japanese to Latin romanization

{
    eval {
	require Text::Kakasi;
	MVRE::def_proc *kakasi, sub {
	    my $kakasi = MVRE::cache {
		Text::Kakasi->new('kakasi', '-Ha', '-Ka', '-Ja', '-Ea', '-ka', '-iutf8', '-outf8');
	    };
	    $_ = $kakasi->get($_);
	}, '(tries to translate Japanese to Ro-maji) (using library)';
	1;
    } and last;

    eval {
	my $found = 0;
	my $kakasi_path = MVRE::_path_search('kakasi');
	unless ($kakasi_path) {
	    die "kakasi not found in \$PATH";
	}
	require IPC::Open2;

	MVRE::def_proc *kakasi, sub {
	    my ($rd,$wr);
	    my $pid = IPC::Open2::open2($rd, $wr, $kakasi_path, '-Ha', '-Ka', '-Ja', '-Ea', '-ka', '-iutf8', '-outf8');
	    print $wr "$_\n";
	    close $wr;
	    $_ = readline $rd;
	    close $rd;
	    waitpid $pid, 0;
	    chomp $_;
	    die "kakasi failed" if $_ eq '';
	    return $_;
	}, "(tries to translate Japanese to Ro-maji) (using $kakasi_path)";
	1;
    };
}

(*nkf, *digits, *kakasi, *mime_decode) if 0; # no warnings 'once';

{
    my @sc = sort keys %MVRE::desc;
    push @EXPORT, @sc;
    push @{$EXPORT_TAGS{all}}, @sc;
    push @{$EXPORT_TAGS{shortcuts}}, @sc;
    push @{$EXPORT_TAGS{all}}, @{$EXPORT_TAGS{apis}};
}

## bootstrap

if ($0 eq __FILE__) {
    package main;
    MVRE->import(":DEFAULT");
    MVRE->main();
    0;
} else {
    1;
}

=head1 NAME

MVRE - bulk rename files using regular expressions or any Perl syntax.

=head1 SYNOPSIS

  perl MVRE.pm [options] replace-expr files ...

=head1 DESCRIPTION

This program/module renames each files given in the command line,
according to the expression given in replace-expr.

Replace-expr may one of C<s///> expression, C<tr///> expression,
pre-defined shortcuts, or any Perl statements which will update C<$_>.

=head1 OPTIONS

=over 8

=item B<--force, -f>

By default, this module will detect any duplicated target (possibly
caused by mis-specification of the replace-expr) or already-existing
files on the target, and aborts the operation if any.  By specifying
this option, this module will overwrite any existing files as
specified.

Note that this mechanism is only protection for bad specifications,
not for any race conditions.

=item B<--test, --dry-run, -n>

Instead of renaming the actual files, it only shows what will happen.

=item B<--no-ext. -x>

Exclude extension parts of the filenames (e.g. C<.txt> or C<.tar.gz>)
from replacement.  These will be stripped before applying replace-expr
and restored after that.

Some heuristics are used for determining the extension parts.  Either
believe it or check it first with C<--dry-run> option.

=item B<--no-dir. -p>

Exclude directory path parts (anything before slash) from replacement.
These will be stripped Before applying replace-expr and restored after
that.
If a filename is ending with a slash, it will be silently skipped.

=back

=head1 APIs

This module can be either used as standalone, or with additional
pre-defined shortcuts extended by users.

As standalone, it can be used as C<"perl MVRE.pm ....">.

To extend, create a script snippet like below (namely, C<mvre>).

    #!/usr/bin/perl
    use lib "<path to MVRE.pm>";
    use MVRE;

    ..., put additional shortcut definitions here ...

    MVRE::main();

Additional shortcut can be defined using the following APIs.

=head2 MVRE::def_regexp

C<def_regexp> will add a shortcut for simple regular expression
replacement or similar things.

It will take two string arguments as C<def_regexp("name", 'expr')>.
The name contains a shortcut name to define, and 'expr' will contain
an Perl expression for replacements.

For example, if the following is defined,

    MVRE::def_regexp('capital', 's/^(.)/\U$1\E/')

C<mvre capital file1 file2 ...> will translate the every first
character of each given files to a corresponding capital character.

=head2 MVRE::def_proc

This API is to define more complex operation than above.

It can be used in the following way:

    MVRE::def_proc *name, sub {
      ... # $_ = ...($_)
    }, "comment for --help";

The first argument, C<name>, contains a glob for a shortcut name to
define.

The second argument is a subroutine computing the rename action.  It
will receive a name of each single file as $_, one by one, and update
it accordingly.  The value shall by returned to $_, not a return value,
as similar to C<s///> or C<tr///> operator.

The third argument, a string, will be shown as a description in the
C<--help> message.

When C<--no-ext> and/or C<--no-dir> are specified,
only the relevant parts of the filenames will be passed.

=head3 pre-computation and cacheing

In some cases, the shortcut may want to access the whole list of
the given filenames to determine an action.  For example,
a predefined shortcut C<digits> will align all occurence of decimal
numbers in given files, to its longest one.

To support this, C<MVRE::cache> can be called within the user-defined
subroutine as follows:

    $v = MVRE::cache {
      ...
      return ...
    }

When this function is called for the first time for some defined
shortcut (more technically precisely, for a specific calling location
given by C<caller> Perl builtin), the block is called with all of the
filenames given in the command line passed to C<@_>.
Options C<--no-ext> and/or C<--no-dir> affects accordingly.
When the block returns an array or a value, it will be returned from
C<cache>, too.

If C<MVRE::cache> is called twice or more, it will simply return the
previous return value, not calling the block again.

See C<digits> defined in MVRE.pm as an example.

=head3 Alternatives to MVRE::def_proc

Alternatively and experimentally, an extension shortcut can also be
defined using attribute syntax as below:

    sub name : Desc(comment for --help) {
      ...
    }

Also, if support for C<--help> option is not needed,
a shortcut can be simply defined by C<sub name { ... }>.

=head1 REFERENCE

L<Homepage|https://www.github.com/yoiwa-personal/>

=head1 AUTHOR/COPYRIGHT

Copyright 2015-2023 Yutaka OIWA <yutaka@oiwa.jp>.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
L<http://www.apache.org/licenses/LICENSE-2.0>

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

=begin comment

=head1 Undocumented, experimental features

 - an replacement expression can return value, instead of updating $_.
   For example, 'return uc' works.
 - --debug-no-precheck; it will not check files for overwrite, but
   use no-replace type of system calls for actual operation if available.

=end comment

=cut
