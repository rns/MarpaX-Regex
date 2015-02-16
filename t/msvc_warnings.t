use 5.010;
use strict;
use warnings;

use Test::More;

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Terse = 1;
$Data::Dumper::Deepcopy = 1;

use Marpa::R2;

my $input = <<EOI;
../cpan/lib/dev/file.c(11824) : warning C4820: '__unnamed' : '3' bytes padding added after member 'c'
../cpan/lib/dev/file.c(12464) : warning C4100: 'param' : unreferenced formal parameter
file.c(12538) : warning C4127: conditional expression is constant
EOI

my $g = Marpa::R2::Scanless::G->new( {
source => \(<<'END_OF_SOURCE'),
:default ::= action => [ name, values ]
lexeme default = action => [ name, values ] latm => 1

    list ::= warning+ separator => [\n]

    warning ::= file ('(') line (')') (':' 'warning') code (':') message

    file    ~ [\w\-:/ \\\.]+    # windows and unix paths
    line    ~ int
    code    ~ 'C' int
    message ~ [\w' \-:]+ #'
    int     ~ [\d]+

:discard ~ whitespace
    whitespace ~ [ ]+

END_OF_SOURCE
} );

my $r = Marpa::R2::Scanless::R->new( { grammar => $g } );
$r->read(\$input);

say Dumper ${ $r->value() };

ok(1);
done_testing();
