use 5.010;
use strict;
use warnings;

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

:default ::= action => [lhs,values]
lexeme default = action => [lhs,value]

S ::= S S
S ::= '<' S '>'
S ::= '<' '>'
S ::= non_parens

non_parens ~ non_paren*
non_paren ~ [^<>]

DSL

use Data::Dumper;
$Data::Dumper::Indent = 0;
$Data::Dumper::Terse = 1;

my $g = Marpa::R2::Scanless::G->new( { source  => \$dsl } );
my $r = Marpa::R2::Scanless::R->new( { grammar => $g } );
say $g->show_symbols;
$r->read( \$string );
my $v = ${ $r->value() };
say Dumper $v;
