use 5.010;
use strict;
use warnings;

use Test::More;

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Terse = 1;
$Data::Dumper::Deepcopy = 1;

use Marpa::R2;

my $dsl = q{;
:default ::= action => [ name, values ]
lexeme default = action => [ name, value ] latm => 1

    expr ::= group

    # character escapes
    <character escape> ~ '\d'

    # literals
    literal ::= ( ["] ) <string without double quotes> ( ["] )
    literal ::= ( ['] ) <string without single quotes> ( ['] )
    <string without double quotes> ~ [^"]+ #"
    <string without single quotes> ~ [^']+ #'
    literal ::= <character escape>

    # character classes
    <character class> ::= '[' <character class characters> ']' quantifier
    <character class> ::= '[' <character class characters> ']'
    <character class characters> ~ <character class character>+
    <character class character> ~ [^\]] | '\[' | '\]' | '[:' | ':]'

    quantifier ::=
            '*'  | '+'  | '?'
        |   '*' '?' | '+' '?' | '?' '?'
        |   '*' '+' | '+' '+' | '?' '+'
        |   '{' <unsigned integer> '}'
        |   '{' <unsigned integer> comma '}'
        |   '{' <unsigned integer> comma <unsigned integer> '}'
        |   '{' <unsigned integer> '}' '?'
        |   '{' <unsigned integer> comma '}' '?'
        |   '{' <unsigned integer> comma <unsigned integer> '}' '?'
        |   '{' <unsigned integer> '}' '+'
        |   '{' <unsigned integer> comma '}' '+'
        |   '{' <unsigned integer> comma <unsigned integer> '}' '+'

    <unsigned integer> ~ [\d]+ # todo: enforce no [+-]
    comma ~ ','

    atom ::= literal | <character class>

    # grouping and alternation
    group ::=
            atom
        | '(' group ')' assoc => group
        || group atom
        || atom group
        || group '|'
        || '|' group
        || group '|' group

:discard ~ whitespace
    whitespace ~ [ ]+

};

# following http://perldoc.perl.org/perlretut.html
# BNF source, input string, scalar-context match, list-context match, desc
my $tests = [
    # mustn't parse
    [ q{ [] }, 'hello world', 1, [ 1 ], 'empty character class, BNF parse error expected' ],
    # must parse, but regex must not compile
    [ q{ [\x] }, 'hello world', 1, [ 1 ], 'empty character class, RE compile error expected' ],
    # must parse
    [ q{ 'hello' }, 'hello world', 1, [ 1 ], 'simple word matching' ],
    [ q{ "hello" }, 'hello world', 1, [ 1 ], 'simple word matching' ],
    [ q{ [a-z] }, 'hello world', 1, [ 1 ], 'unquantified character class' ],
    [ q{ [a-z] * }, 'hello world', 1, [ 1 ], '0 or more character class' ],
    [ q{ [a-z] * + }, 'hello world', 1, [ 1 ], 'possessive 0 or more character class' ],
    [ q{ [a-z]+ }, 'hello world', 1, [ 1 ], '1 or more character class' ],
    [ q{ [a-z]++ }, 'hello world', 1, [ 1 ], 'possessive 1 or more character class' ],
    [ q{ [a-z]? }, 'hello world', 1, [ 1 ], '0 or 1 character class' ],
    [ q{ [a-z] * ? }, 'hello world', 1, [ 1 ], 'non-greedy 0 or more character class' ],
    [ q{ [a-z] + ? }, 'hello world', 1, [ 1 ], 'non-greedy 1 or more character class' ],
    [ q{ [a-z]*? }, 'hello world', 1, [ 1 ], 'non-greedy 0 or more character class' ],
    [ q{ [a-z]+? }, 'hello world', 1, [ 1 ], 'non-greedy 1 or more character class' ],
    [ q{ [a-z]?? }, 'hello world', 1, [ 1 ], 'non-greedy 0 or 1 character class' ],
    [ q{ [a-z]{2} }, 'hello world', 1, [ 1 ], 'exactly 2 character class' ],
    [ q{ [a-z]{2,} }, 'hello world', 1, [ 1 ], '2 or more character class' ],
    [ q{ [a-z]{2,3} }, 'hello world', 1, [ 1 ], '2 or 3 character class' ],
    [ q{ [a-z]{2}? }, 'hello world', 1, [ 1 ], 'non-greedy 2 or more character class' ],
    [ q{ [a-z]{2,}? }, 'hello world', 1, [ 1 ], 'non-greedy 2 or more character class' ],
    [ q{ [a-z] { 2 , 3 } ? }, 'hello world', 1, [ 1 ], 'non-greedy 2 or 3 character class' ],
    [ q{ [a-z]{2}+ }, 'hello world', 1, [ 1 ], 'non-backtracking (possessive) exactly 2 character class' ],
    [ q{ [a-z]{2,}+ }, 'hello world', 1, [ 1 ], 'non-backtracking 2 or more character class' ],
    [ q{ [^a-z] { 2 , 3 } + }, 'hello world', '', [], 'non-backtracking 2 or 3 character class' ],
    [ q{ 'lit[a-z]ral' }, 'literal', 1, [ 1 ], 'embedded in literal character class' ],
    # alternation
    [ q{ 'cat' | 'dog' | 'bird' }, "cats and dogs", 1, [ 1 ], 'alternation: match literal the first alternative' ],
    [ q{ 'dog' | "cat" | 'bird' }, "cats and dogs", 1, [ 1 ], 'alternation: match literal earlier in the string' ],
    [ q{ 'c' | 'ca' | 'cat' | "cats" }, "cats", 1, [ 1 ], 'alternation, match character class at the first string position' ],
    [ q{ "cats" | 'cat' | 'ca' | 'c' }, "cats", 1, [ 1 ], 'alternation, match character class at the first string position' ],
    [ q{ [cat]{3} | [dog]+ | [^bird]* }, "cats and dogs", 1, [ 1 ], 'alternation, match character classes' ],
    # grouping
    [ q{ ( 'a' | 'b')  }, "ab", 1, [ 'a' ], 'grouping' ],
    [ q{ ( 'a' | 'b' ) 'b' }, "bb", 1, [ 'b' ], 'grouping' ],
    [ q{ [b] ( 'a' | 'b' ) }, "bb", 1, [ 'b' ], 'grouping' ],
    [ q{ ( 'ac' | 'b' ) 'b' }, [ 'acb', "bb" ], [ 1, 1 ], [ 'ac', 'b' ], 'grouping' ],
    # matches 'ac' at start of string or 'bc' anywhere
    # todo: test error, e.g. unbalanced parens: ( ('^a'|'b')'c'
    [ q{ ('^a'|'b')'c' }, [ 'ac', "bc" ], [ 1, 1 ], [ 'a', 'b' ], 'grouping' ],
    # matches 'ad', 'bd', or 'cd'
    [ q{ ('a'|[bc])'d' }, [ 'ad', 'bd', 'cd' ], [ 1, 1, 1 ], [ 'a', 'b', 'c' ], 'grouping with character class' ],

    [ q{ 'house' ( 'cat' | ) }, [ 'housecat', 'house' ], [ 1, 1 ], [ 'cat', '' ], 'grouping, empty alternative' ],
    [ q{ 'house' ( 'cat' ( 's' |)|) },
        [ 'housecats',      'housecat',    'house' ],
        [ 1,                1,             1 ],
        [ [ 'cats', 's' ],  [ 'cat', '' ], [ '', undef ] ],
        'grouping, nested' ],
     # match years 19xx, 20xx, or the Y2K problem, xx
    [ q{ ('19'|'20'|)\d\d },
        [ '1901', '2001', '20' ],
        [ 1,      1,      1 ],
        [ '19', '20', '' ],
        'grouping, years' ],

];

=pod grouping

    "20" =~ /(19|20|)\d\d/;  # matches the null alternative '()\d\d',
                             # because '20\d\d' can't match
=cut

my $slg = Marpa::R2::Scanless::G->new( { source  => \$dsl } );

sub translate{
    my ($ast) = @_;
    state $depth++;
    my $s;
    my $indent = "  " x ($depth - 1);
    if (ref $ast){
        my ($node_id, @children) = @$ast;
        if (0){
#        if ($node_id eq 'alternation'){
#            warn Dumper $node_id, \@children;
#            $s .= join '|', map { translate( $_ ) } @children
        }
        else{
            $s .= join '', map { translate( $_ ) } @children;
        }
    }
    else{
        $s .= $ast;
    }
    $depth--;
    return $s;
}

TESTS:
for my $test (@$tests){

    my ($source, $input, $expected_scalar, $expected_list, $desc) = @$test;

    $input = [ $input ] unless ref $input eq "ARRAY";
    $expected_scalar = [ $expected_scalar ] unless ref $expected_scalar eq "ARRAY";
    $expected_list = [ $expected_list ] unless ref $expected_list eq "ARRAY";

    my $must_parse   = $desc !~ /BNF parse error expected/;
    my $must_compile = $desc !~ /RE compile error expected/;

    $source =~ s/^\s+|\s+$//g;
    diag "BNF: $source";

    # must parse unambiguously unless parse error is expected
    my $ast = eval { ${ $slg->parse(\$source) } };
    if ($must_parse){
        ok !$@, 'BNF parsed';
    }
    else{
        ok $@, 'BNF not parsed, as expected';
    }

    # reparse with trace_terminals and progress unless parse error expected
    if ($@ and $must_parse){
        my $slr = Marpa::R2::Scanless::R->new( {
            grammar => $slg,
            trace_terminals => 1,
        } );
        eval { $slr->read(\$source) } || warn "$@\nProgress report is:\n" . $slr->show_progress;
        if ( $slr->ambiguity_metric() >= 2 ){
            diag "BNF parse is ambiguous:\n", $slr->ambiguous();
            # count or list parses here?
        }
        else{
            my $value_ref = $slr->value();
            diag "Parse failed, but there is this value:\n", Dumper ${ $value_ref } if defined $value_ref;
        }
    }

    SKIP: {

        skip "BNF source parse error", 3 unless defined $ast and $must_parse;

#        warn Dumper $ast;
        my $re = translate( $ast );
        diag "RE: /$re/";

        my $re_compiles;
        { no warnings; $re_compiles = eval { qr/$re/ } };
        ok !$@, "$desc: compile";
        SKIP: {
            skip "RE doesn't compile", @$input - 1 unless $re_compiles and $must_compile;

            for my $i (0 .. @$input - 1){
                my $in = $input->[$i];
                my $exp_list = $expected_list->[$i];
                my $exp_scalar = $expected_scalar->[$i];

                diag "input $i: $in";

                my $got = $in =~ /$re/x;
                is $got, $exp_scalar, "$desc: scalar-context match";

                my @got = $in =~ /$re/x;
                # compare to empty array if no match in list context
                $exp_list = [] if not defined $exp_list;
                # compare to single-item array if matched in list context
                $exp_list = [ $exp_list ] unless ref $exp_list;
                is_deeply \@got, $exp_list, "$desc: list-context match";
            } ## for $input ...
        }
    }

} ## for my $test (@$tests) ...

done_testing();
