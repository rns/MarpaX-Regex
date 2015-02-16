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

=head2 todos:

    predicates (child, descendant, sibling)

    distill (produce new MarpaX::(Regex::)AST tree with skip => \@nodes skipped

        'skip' must affect named literals, e.g.
        collapse name/literal axis by pruning intermediate nodes up to named literal, e.g.
            ['symbol',['symbol name',['bare name','digit']]]
        prune('symbol', 'bare name') => ['symbol', 'digit' ]

        unwind recursion to arrays

    start/length -- (un)pack node to hash ref according to
        { key => index } mapping passed as an option, e.g.
        { name => 0, start => 1, length => 2, value => 3 }
            for
                [ name, start, length, value ]

    block vs. span (increasing/non-increasing depth) in sprint

    handlers

    is ast manipulation even possible without deepcopy?
        cut, paste, copy
        filter

=cut

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
 statements
   statement
     lhs
       bare name 's'
     alternatives
       #text '('
       bare name 'nineteen'
       alternation '|'
       bare name 'twenty'
       alternation '|'
       #text ')'
       character escape '\d'
       character escape '\d'
   statement
     lhs
       bare name 'nineteen'
     alternatives
       string without single quotes and metacharacters '19'
   statement
     lhs
       bare name 'twenty'
     alternatives
       string without single quotes and metacharacters '20'
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
 statements
   statement
     lhs
       bare name 'number'
     alternatives
       metacharacter '^'
       #text '('
       bracketed name '<optional sign>'
       #text ')'
       #text '('
       bracketed name '<f.p. mantissa>'
       alternation '|'
       bare name 'integer'
       #text ')'
       #text '('
       bracketed name '<optional exponent>'
       #text ')'
       metacharacter '$'
   statement
     lhs
       bracketed name '<optional sign>'
     alternatives
       #text '['
       character class characters '+-'
       #text ']'
       quantifier '?'
   statement
     lhs
       bracketed name '<f.p. mantissa>'
     alternatives
       bare name 'digit'
       quantifier '+'
       string without single quotes and metacharacters '.'
       bare name 'digit'
       quantifier '+'
       alternation '|'
       bare name 'digit'
       quantifier '+'
       string without single quotes and metacharacters '.'
   statement
     lhs
       bracketed name '<f.p. mantissa>'
     alternatives
       string without single quotes and metacharacters '.'
       bare name 'digit'
       quantifier '+'
   statement
     lhs
       bare name 'integer'
     alternatives
       bare name 'digit'
       quantifier '+'
   statement
     lhs
       bracketed name '<optional exponent>'
     alternatives
       #text '('
       #text '['
       character class characters 'eE'
       #text ']'
       #text '['
       character class characters '+-'
       #text ']'
       quantifier '?'
       character escape '\d'
       quantifier '+'
       #text ')'
       quantifier '?'
   statement
     lhs
       bare name 'digit'
     alternatives
       character escape '\d'
},
'building a regexp, unfactored form' ],
];

for my $test (@$tests){

    my ($source, $ast_str, $desc) = @$test;

    $source =~ s/^\s+|\s+$//g;
    $ast_str =~ s/^\n+//g;
    diag "Regex BNF: $source";

    # must parse unambiguously unless parse error is expected
    my $rex = MarpaX::Regex->new;
    my $value = eval { $rex->parse($source) };
    ok !$@, 'Regex BNF parsed';

    my $ast = MarpaX::Regex::AST->new( $value );

    my %node_skip_list = map { $_ => 1 } (
        'group', 'primary',
        'alternative rule',
        'symbol', 'symbol name',
        'character class', 'literal'
    );

    my $skip_ast_str = $ast->sprint( {
        skip => sub {
            my ($ast, $context) = @_;
            my ($node_id, @children) = @$ast;
            return exists $node_skip_list{ $node_id }
        }
    } );

    eq_or_diff $skip_ast_str, $ast_str, "sprint() with node skip list, $desc";
    my $distilled_ast = $ast->distill;

#    warn Dumper $distilled_ast;
    my $distilled_ast_str = $distilled_ast->sprint();
#    warn $distilled_ast_str;

    eq_or_diff $distilled_ast_str, $skip_ast_str, "distill(), $desc";

    $distilled_ast->substitute();

    warn $distilled_ast->concat();

} ## for my $test (@$tests) ...

done_testing();
