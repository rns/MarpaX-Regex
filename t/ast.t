use 5.010;
use strict;
use warnings;

use Test::More;
use Test::Differences;

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
q{
(19|20|)\d\d
},
'grouping, years' ],
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
q{
^([+-]?)(\d+.\d+|\d+.|.\d+|\d+)(([eE][+-]?\d+)?)$
},
'building a regexp, unfactored form' ],
# /^[+-]?(\d+\.\d+|\d+\.|\.\d+|\d+)([eE][+-]?\d+)?$/
[ q{
    number              ::= ^ (<optional sign>) (<f.p. mantissa> | integer) (<optional exponent>) $
    <optional sign>     ::= [+-]?
    <f.p. mantissa>     ::= digit+ '.' digit+  # mantissa of the form a.b
    integer             ::= digit+             # integer of the form a
    <f.p. mantissa>     ::= '.' digit+         # mantissa of the form .b
    <optional exponent> ::= ([eE][+-]?\d+)?
    <f.p. mantissa>     ::= digit+ '.'         # mantissa of the form a.
    digit               ::= \d
},
q{
^([+-]?)(\d+.\d+|.\d+|\d+.|\d+)(([eE][+-]?\d+)?)$
},
'building a regexp, unfactored form, more dispered same-lhs statements' ],

];

for my $test (@$tests){

    my ($source, $expected_regex, $desc) = @$test;

    $source =~ s/^\s+|\s+$//gms;
    $expected_regex =~ s/^\s+//gms;

    # must parse unambiguously unless parse error is expected
    my $rex = MarpaX::Regex->new;
    my $value = eval { $rex->parse($source) };
    ok !$@, 'Regex BNF parsed';

    my $ast = MarpaX::Regex::AST->new( $value );

    my $regex = $ast->distill()->substitute()->recurse()->concat();

    my $slif = $rex->compile( $value, MarpaX::Regex::TARGET_SLIF );

    diag "BNF:\n$source" unless $ENV{HARNESS_ACTIVE};
    eq_or_diff $regex, $expected_regex, "distill()->substitute()->recurse()->concat(), $desc";
    diag "regex: $regex" unless $ENV{HARNESS_ACTIVE};

} ## for my $test (@$tests) ...

done_testing();
