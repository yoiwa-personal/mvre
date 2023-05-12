#!/usr/bin/perl
################################################################
# MVRE: bulk rename files using regular expresson or any Perl syntax.
################################################################

use 5.016;

use strict;

package MVRE;

use Getopt::Long 'GetOptionsFromArray';
Getopt::Long::Configure qw/bundling/;

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
	print STDERR <<'EOF';
mvre: move (rename) files regarding to regexp

Usage: mvre [-t] 'expr' files...

       -t --test: not move but only shows the renames
       -f --force: no check for overwriting
       -h --help: show this help

       expr is one of:
              s/from/to/
              tr/..../..../
              $_ = ... (evaluated as perl statements, using $_ as (in|out)put)
              a shortcut keyword

       shortcut keywords:
EOF

	foreach my $k (sort keys %desc) {
	    print STDERR "          $k => $desc{$k}\n";
	}
	print STDERR "\n";
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
	    ($_, $post) = m@\A(.*?)(\.[\w\d---][*[a-zA-Z][\w\d---]*(?:\.(?:gz|bz\d?))?)?\Z@;
	}
	die "assert failed" if "$pre$_$post" ne $t;
	push @ppargs, [$pre, $_, $post];
	push @cacheargs, $_;
    }

    foreach my $a (@ppargs) {
	my ($pre, $post);
	($pre, $_, $post) = @$a;
	dsay (2, ":  pre=$pre _=$_ post=$post");
	my $from = $pre . $_ . $post;
	eval "use strict; package main; $exp;";
	die "cannot rename \"$from\": $@" if ($@);
	my $to = $pre . $_ . $post;
	if (($from ne $to) and !$force) {
	    # caveat: we don't and can't care about TOCTOW: just for accidental overwriting with bad expression.
	    die "cannot rename \"$from\": target filename \"$to\" already exists.\n" if (-e "$to");
	    die "cannot rename \"$from\": target filename \"$to\" overwraps with \"$table{$to}\".\n" if (exists $table{$to});
	}
	$table{$to} = $from;
	$ftable{$from} = $to;
    }

    foreach my $from (@args) {
	$_ = $from;
	my $to = $ftable{$from};
	unless ($from eq $to) {
	    unless ($test) {
		unless (rename($from, $to)) {
		    print STDERR "\Q$from\E -> \Q$to\E: $!.\n";
		    exit 1;
		}
	    }
	    print "$from -> $to\n";
	}
    }
}


## APIs for extension writers

# dsay: show diagnostic message when --debug is given
#    levels: 1 = user-defined/predefined procedure
#            2 = MVRE's input processing
#            3 = MVRE's internal state-keeping

sub dsay (@) {
    my $level = 1;
    if (@_ >= 2 and $_[0] =~ /\A\d+\z/) {
	$level = 0 + shift @_;
    }
    print @_, "\n" if $DEBUG >= ($level || 1);
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
#   sub digits : desc(comment) {
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
    dsay(3, "cache key = $key");
    if (!defined $cache{$key}) {
	my @a = map { $_ . "" } @cacheargs;
	my @r = &$f(@a);
	$cache{$key} = \@r;
    }
    my @r = @{$cache{$key}};
    return wantarray ? @r : $r[0];
}

# experimental support for attributes;

sub main_MODIFY_CODE_ATTRIBUTES($$@) {
    my ($pkg, $ref, @attrs) = @_;
    my @result = ();
    foreach my $a (@attrs) {
	if ($a =~ /\Adesc\((.+)\)\Z/) {
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
	'nospecial', 's/[^\w\d-_.\/]+/_/g',
	'nospecial_euc', 's/[^\w\d-_.\/\221-\376]+/_/g',
	'nospecial_utf', 's/[^\w\d-_.\/\200-\376]+/_/g',
	'urlencode', 's/([^\w\d-_.\/])/sprintf("%%%02X",ord $1)/ge',
	'urlencodeP', 's/([^\w\d-_.\/%])/sprintf("%%%02X",ord $1)/ge',
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
#   sub digits : desc((make numbers in filenames the same length)) {
#    ...
#   }
#
# will also work.

# Japanese code conversions

eval {
    eval "use NKF"; die if $@;
    sub mime_decode {
	s/\=[\_?X]ISO-2022-JP[\_?X]B[\_?X]([0-9A-Za-z\/+]+)=*([\_?X]=)?/nkf('-mB','-Jw',"$1")/egi;
	#nkf('-Mb -e',$1)/eg;
    }
    $MVRE::key{'mime_decode'} = '(decode MIME B encoding)';
};

eval {
    eval "use NKF"; die if $@;
    sub utf {
	$_ = NKF::nkf('-w',$_);
    }
    $MVRE::key{'utf'} = '(convert to UTF-8)';
};

eval {
    eval "use Jcode"; die if $@;
    sub utf {
	$_ = Jcode->new($_)->utf8;
    }
    $MVRE::key{'utf'} = '(convert to UTF-8 using Jcode.pm)';
};

# Japanese to Latin romanization

use IPC::Open2;
eval {
    my $found = 0;
    my $kakasi_path = (grep { ($_ = "$_/kakasi"), -x $_ } (split(":", $ENV{PATH})))[0];
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

# just to use:
#   invoke as "perl MVRE.pm ...args..."
#
# to extend or customize:
#   use MVRE;
#   [call any number of MVRE::def_regexp or MVRE::def_proc]
#   MVRE::main();
