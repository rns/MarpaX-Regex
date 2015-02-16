use 5.010;
use strict;
use warnings;

use Test::More;

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Terse = 1;
$Data::Dumper::Deepcopy = 1;

my $string =<<"HERE";
I have some <brackets in <nested brackets> > and
<another group <nested once <nested twice> > >
and that's it.
HERE
#'

my @groups = $string =~ m/
	(                   # start of capture group 1
	<                   # match an opening angle bracket
			(?:
					[^<>]++     # one or more non angle brackets, non backtracking
						|
					(?1)        # found < or >, so recurse to capture group 1
			)*
	>                   # match a closing angle bracket
	)                   # end of capture group 1
	/xg;

$" = "\n\t";
say "Found:\n\t@groups\n";

use Marpa::R2;

my $dsl = <<DSL;

:default ::= action => [ name, values ]
lexeme default = action => [ name, value ]

string ::= '<' <string in brackets> '>'
<string in brackets> ::= <string with or without brackets>+
<string with or without brackets> ::= string | <string without brackets>

<string without brackets> ~ [^<>]+

DSL


my $g = Marpa::R2::Scanless::G->new( { source  => \$dsl } );
my $r = Marpa::R2::Scanless::R->new( {
    grammar => $g,
    trace_terminals => 0,
} );
$r->read( \("<" . $string . ">"));
say "multiple parses!" if $r->ambiguity_metric() > 1;
my $v = ${ $r->value() };
say Dumper $v;

ok(1);
done_testing();
