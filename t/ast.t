use 5.010;
use strict;
use warnings;

use Test::More;

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Terse = 1;

use MarpaX::Regex;
use MarpaX::Regex::AST;

# following http://perldoc.perl.org/perlretut.html
# Regex BNF source, input string, scalar-context match, list-context match, desc
my $tests = [
    # Building a regexp
    [ q{
         s ::= ( nineteen | twenty | ) \d\d
         nineteen ::= '19'
         twenty   ::= '20'
      },
      'grouping, years'
    ],
    # /^[+-]?(\d+\.\d+|\d+\.|\.\d+|\d+)([eE][+-]?\d+)?$/
    [ q{
         number              ::= ^ (<optional sign>) (<f.p. mantissa> | integer) (<optional exponent>) $
         <optional sign>     ::= [+-]?
         <f.p. mantissa>     ::= digit+ '.' digit+  # mantissa of the form a.b
                               | digit+ '.'         # mantissa of the form a.
         <f.p. mantissa>     ::= '.' digit+         # mantissa of the form .b
         integer             ::= digit+             # integer of the form a
         <optional exponent> ::= ([eE][+-]?\d+)?
         digit               ::= \d
      },
      'building a regexp, unfactored form'
    ],
];

#use Carp::Always;

for my $test (@$tests){

    my ($source, $desc) = @$test;

    $source =~ s/^\s+|\s+$//g;
    diag "Regex BNF: $source";

    # must parse unambiguously unless parse error is expected
    my $rex = MarpaX::Regex->new;
    my $value = eval { $rex->parse($source) };
    ok !$@, 'Regex BNF parsed';

    my $ast = MarpaX::Regex::AST->new( $value );
#    warn $ast->dump( { Indent => 1, Deepcopy => 1 } );
    warn $ast->sprint( {
        skip => [ 'group', 'primary', 'alternative rule', 'symbol name', 'character class', 'literal', 'symbol' ],
#        max_depth => 6,
    } );
#    warn $ast->sprint( { max_depth => 8 } );
#    warn $ast->dump( { Indent => 1, Deepcopy => 1 } );
#    next;
    $ast->walk( {
        visit => sub {
            my ($ast, $context) = @_;
            local $Data::Dumper::Indent = 0;
            local $Data::Dumper::Deepcopy = 0;
            warn join ( ': ', $context->{depth}, $ast->dump( { Indent => 0 } ) );
        },
#        traversal => 'preorder',
        skip => [ 'symbol name' ],
        max_depth => 6,
    } );

} ## for my $test (@$tests) ...

done_testing();
