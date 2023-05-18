#!/usr/bin/perl
################################################################
# MVRE: bulk rename files using regular expresson or any Perl syntax.
################################################################

use 5.016;

use strict;

package MVRE;

use Getopt::Long 'GetOptionsFromArray';
Getopt::Long::Configure qw/bundling require_order/;

BEGIN {
    eval { require Sub::Util; };
    if ($@) { eval 'sub Sub::Util::set_subname($$) { return @_[1] }'; }
}

our %desc;
BEGIN { %desc = (); }
our %cache = 0;

our @cacheargs; # shared with main and cache
our $DEBUG = 0;

## the main processing

sub main {
    my @args = @main::ARGV;

    my $force = 0;
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
	'help|h' => \$help);

    my $exp = shift @args;

    if ($exp eq '' or $help) {
	show_help();
	exit 1;
    }

    my %table;
    my %ftable;

    my @ppargs;
    foreach my $t (@args) {
	local $_ = $t;
	my $pre = '';
	my $post = '';

	if ($nodir and /\//) {
	    ($pre, $_) = m@\A(.*/)([^/]*)\Z@;
	    next if $_ eq '';
	}
	if ($noext and /\./) {
	    ($_, $post) = m@\A(.*?)((?:\.[\-\w\d]*[a-zA-Z][\-\w\d]*(?:\.(?:gz|bz\d?))?)?(?:,v)?)\Z@;
	}
	die "assert failed" if "$pre$_$post" ne $t;
	push @ppargs, [$pre, $_, $post];
	push @cacheargs, $_;
    }

    foreach my $a (@ppargs) {
	my ($pre, $post);
	($pre, $_, $post) = @$a;
	dsay (4, ":  pre=$pre _=$_ post=$post");
	my $from = $pre . $_ . $post;
	eval "use strict; package main; $exp;";
	die "cannot rename \"$from\": $@" if ($@);
	my $to = $pre . $_ . $post;
	if (!$force) {
	    die "cannot rename \"$from\" to \"$to\": file missing\n" unless (-e "$from");
	    if ($from ne $to) {
		# Here we just check for accidental overwriting with bad expression.
		die "cannot rename \"$from\": target filename \"$to\" already exists.\n" if (-e "$to");
		die "cannot rename \"$from\": target filename \"$to\" overwraps with \"$table{$to}\".\n" if (exists $table{$to});
	    }
	}
	$table{$to} = $from;
	$ftable{$from} = $to;
    }

    my $rename_func = $force ? \&CORE::rename : \&RenameNoReplace::rename_noreplace;
    # On Linux (not too old ones), overwriting existing files will be
    # prevented using the dedicated system call.
    # It is impossible with pure POSIX system APIs.

    foreach my $from (@args) {
	$_ = $from;
	my $to = $ftable{$from};
	unless ($from eq $to) {
	    unless ($test) {
		unless (&$rename_func($from, $to)) {
		    print STDERR "\Q$from\E -> \Q$to\E: rename failed: $!.\n";
		    exit 1;
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
    if (_path_search($base) eq $self) {
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
	print STDERR "    Support for No-replace rename: " , ($RenameNoReplace::rename_noreplace_supported || "(none)"), "\n";
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

package RenameNoReplace;
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
    *rename_noreplace = \&CORE::rename unless defined $rename_noreplace_supported;
}

## APIs for extension writers

# dsay: show diagnostic message when --debug is given

package MVRE;

sub dsay (@) {
    my $level = 1;
    if (@_ >= 2 and $_[0] =~ /\A\d+\z/) {
	$level = 0 + shift @_;
    }
    if ($DEBUG >= ($level || 1)) {
	if (ref($_[0]) eq 'CODE') {
	    @_ = &{$_[0]};
	}
	print STDERR join(" ", @_), "\n"
    }
}

# "undocumented" features:
# - first argument can be an integer for the debug messsage level
#    levels: 1 = user-defined/predefined procedure
#            3 = MVRE::cache internal state-keeping
#            4 = MVRE's input processing or other internal
# - message can be a code reference, which will be called only when needed.

# MVRE::Dumper: auto-loaded Data::Dumper

sub Dumper {
    dsay 4, "loading Data::Dumper";
    require Data::Dumper;
    no warnings 'once';
    $Data::Dumper::Indent = 0;
    $Data::Dumper::Terse = 1;
    *MVRE::Dumper = \&Data::Dumper::Dumper;
    goto &Data::Dumper::Dumper;
}

# def_regexp(name, expr_str)
# define a shortcut with "name", doing r/// or tr/// specified in expr_str.

sub def_regexp($$) {
    my ($k, $r) = @_;
    eval "package main; sub $k { $r }";
    die "while defining $k: $@" if $@;
    $desc{$k} = "$r";
}

# ** def_proc(glob, sub, comment)

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
    my $name = *$a{NAME};
    my $package = *$a{PACKAGE};
    *$a = Sub::Util::set_subname("${package}::${name}", $proc);
    $desc{$name} = $help;
}

# ** cache(proc)
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
    my $key = "$caller1[3]\@$caller0[1]:$caller0[2]";
    my @r;
    if (!defined $cache{$key}) {
	my @a = map { $_ . "" } @cacheargs;
	@r = &$f(@a);
	$cache{$key} = \@r;
	dsay(3, sub {"cache store with key = $key, value = " . Dumper(\@r)});
    } else {
	@r = @{$cache{$key}};
    	dsay(3, sub {"cache hit   with key = $key, value = " . Dumper(\@r)});
    }
    return wantarray ? @r : $r[0];
}

# experimental support for attributes;

sub main_MODIFY_CODE_ATTRIBUTES($$@) {
    my ($pkg, $ref, @attrs) = @_;
    my @result = ();
    foreach my $a (@attrs) {
	if ($a =~ /\ADesc\((.+)\)\Z/) {
	    my @subname = split("::", Sub::Util::subname($ref));
	    $desc{$subname[-1]} = $1;
	} else {
	    push @result, $a;
	}
    }
    return @result;
}

BEGIN { *main::MODIFY_CODE_ATTRIBUTES = \&main_MODIFY_CODE_ATTRIBUTES; }

# predefined regular expressions
{
    my %keys = (
	'lower', 'tr/A-Z/a-z/',
	'upper', 'tr/a-z/A-Z/',
	'nospecial', 's/[^\w\d\-_.\/]+/_/g',
	'nospecial_euc', 's/[^\w\d\-_.\/\221-\376]+/_/g',
	'nospecial_utf', 's/[^\w\d\-_.\/\200-\376]+/_/g',
	'urlencode', 's/([^\w\d\-_.\/])/sprintf("%%%02X",ord $1)/ge',
	'urlencodeP', 's/([^\w\d\-_.\/%])/sprintf("%%%02X",ord $1)/ge',
       );

    foreach my $k (keys %keys) {
	def_regexp($k, $keys{$k});
    }
}

package main;

sub dsay (@);
*dsay = \&MVRE::dsay;

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

eval {
    eval "use NKF"; die if $@;
    MVRE::def_proc *mime_decode, sub {
	s/\=[\_?X]ISO-2022-JP[\_?X]B[\_?X]([0-9A-Za-z\/+]+)=*([\_?X]=)?/nkf('-mB','-Jw',"$1")/egi;
	#nkf('-Mb -e',$1)/eg;
    }, '(decode MIME B encoding)';
};

eval {
    eval "use NKF"; die if $@;
    MVRE::def_proc *utf, sub {
	$_ = NKF::nkf('-w',$_);
    }, '(convert to UTF-8)';
};

eval {
    eval "use Jcode"; die if $@;
    MVRE::def_proc *utf, sub {
	$_ = Jcode->new($_)->utf8;
    }, '(convert to UTF-8 using Jcode.pm)';
};

# Japanese to Latin romanization

use IPC::Open2;
eval {
    my $found = 0;
    my $kakasi_path = MVRE::_path_search('kakasi');
    unless ($kakasi_path) {
	die "kakasi not found in \$PATH";
    }

    MVRE::def_proc *kakasi, sub {
        my ($rd,$wr);
        my $pid = open2($rd, $wr, $kakasi_path, '-Ha', '-Ka', '-Ja', '-Ea', '-ka', '-iutf8', '-outf8');
        print $wr "$_\n";
        close $wr;
        $_ = readline $rd;
        close $rd;
        waitpid $pid, 0;
        chomp $_;
        die "kakasi failed" if $_ eq '';
        return $_;
    }, "(tries to translate Japanese to Ro-maji) (using $kakasi_path)";
};

eval {
    eval "use Text::Kakasi"; die if $@;
    my $kakasi_module_inited = '';
    MVRE::def_proc *kakasi, sub {
	my $kakasi = MVRE::cache {
	    Text::Kakasi->new('kakasi', '-Ha', '-Ka', '-Ja', '-Ea', '-ka', '-iutf8', '-outf8');
	};
	$_ = $kakasi->get($_);
    }, '(tries to translate Japanese to Ro-maji) (using library)';
};

## bootstrap

if ($0 eq __FILE__) {
    MVRE::main();
    0;
} else {
    1;
}

(*nkf, *digits, *kakasi, *mime_decode) if 0; # no warnings 'once';


=head1 NAME

MVRE - bulk rename files using regular expressions or any Perl syntax.

=head1 SYNOPSIS

  perl MVRE.pm [options] replace-expr files ...

=head1 DESCRIPTION

This program/module renames each files given in the command line,
according to the expression given in replace-expr.

Replace-expr may one of C<s///> expression, C<tr///> expression,
pre-defined shortcuts, or any Oerl statements which will update C<$_>.

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
=cut
