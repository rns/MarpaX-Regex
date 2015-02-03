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

    statements ::= statement+

    # character escapes
    <character escape> ~ '\d' | '\w'

    # literals
    literal ::= ( ["] ) <string without double quotes> ( ["] )
    literal ::= ( ['] ) <string without single quotes> ( ['] )
    <string without double quotes> ~ [^\^\$\."]+ #"
    <string without single quotes> ~ [^\^\$\.']+ #'

    # character classes
    <character class> ::= '[' <character class characters> ']' quantifier
    <character class> ::= '[' <character class characters> ']'
    <character class> ::= <character escape>
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
    boolean ~ [01]

    symbol ::= <symbol name> # adapted from metag.bnf
    <symbol name> ::= <bare name>
    <symbol name> ::= <bracketed name>
    <bare name> ~ [^0-9'"\[\]\\\*\+\{\}\?\(\)\|\^\$\.\s] <word chars> #'
    <word chars> ~ [\w]*
    <bracketed name> ~ '<' <bracketed name string> '>'
    <bracketed name string> ~ [\s\.\w]+

    atom ::= literal | <character class> | symbol | metacharacter
    metacharacter ~ '^' | '$' | '.'

    # grouping and alternation
    group ::=
            atom
        | '(' group ')' assoc => group
#        || group atom
#        || atom group
#        || '|' group
#        || group '|'
        || group '|' group
        || group group

    # statements
    statement ::= <empty rule> | <alternative rule> | <quantified rule>

    <empty rule> ::= lhs (<op declare bnf>)
    <alternative rule> ::= lhs (<op declare bnf>) alternatives
    <quantified rule> ::= lhs (<op declare bnf>) <single symbol> quantifier <adverb list>
    <quantified rule> ::= lhs (<op declare bnf>) <single symbol> quantifier

    alternatives ::= group

    <adverb list> ::= <adverb list items>
    <adverb list items> ::= <adverb item>*
    <adverb item> ::= <separator specification> | <proper specification>

    <separator specification> ::= ('separator' '=>') <single symbol>
    <proper specification> ::= ('proper' '=>') boolean

    lhs ::= <symbol name>
    <single symbol> ::= symbol

    <op declare bnf> ~ '::='

:discard ~ whitespace
    whitespace ~ [\s]+

:discard ~ <hash comment>
    <hash comment> ~ <terminated hash comment> | <unterminated final hash comment>
    <terminated hash comment> ~ '#' <hash comment body> <vertical space char>
    <unterminated final hash comment> ~ '#' <hash comment body>
    <hash comment body> ~ <hash comment char>*
    <vertical space char> ~ [\x{A}\x{B}\x{C}\x{D}\x{2028}\x{2029}]
    <hash comment char> ~ [^\x{A}\x{B}\x{C}\x{D}\x{2028}\x{2029}]

};

# following http://perldoc.perl.org/perlretut.html
# BNF source, input string, scalar-context match, list-context match, desc
my $tests = [
    # mustn't parse
    [ q{ s ::= [] }, 'hello world', 1, [ 1 ], 'empty character class, BNF parse error expected' ],
    # must parse, but regex must not compile
    [ q{ s ::= [\x] }, 'hello world', 1, [ 1 ], 'empty character class, RE compile error expected' ],
    # must parse
    [ q{ s ::= 'hello' }, 'hello world', 1, [ 1 ], 'simple word matching' ],
    [ q{ s ::= "hello" }, 'hello world', 1, [ 1 ], 'simple word matching' ],
    [ q{ s ::= [a-z] }, 'hello world', 1, [ 1 ], 'unquantified character class' ],
    [ q{ s ::= [a-z] * }, 'hello world', 1, [ 1 ], '0 or more character class' ],
    [ q{ s ::= [a-z] * + }, 'hello world', 1, [ 1 ], 'possessive 0 or more character class' ],
    [ q{ s ::= [a-z]+ }, 'hello world', 1, [ 1 ], '1 or more character class' ],
    [ q{ s ::= [a-z]++ }, 'hello world', 1, [ 1 ], 'possessive 1 or more character class' ],
    [ q{ s ::= [a-z]? }, 'hello world', 1, [ 1 ], '0 or 1 character class' ],
    [ q{ s ::= [a-z] * ? }, 'hello world', 1, [ 1 ], 'non-greedy 0 or more character class' ],
    [ q{ s ::= [a-z] + ? }, 'hello world', 1, [ 1 ], 'non-greedy 1 or more character class' ],
    [ q{ s ::= [a-z]*? }, 'hello world', 1, [ 1 ], 'non-greedy 0 or more character class' ],
    [ q{ s ::= [a-z]+? }, 'hello world', 1, [ 1 ], 'non-greedy 1 or more character class' ],
    [ q{ s ::= [a-z]?? }, 'hello world', 1, [ 1 ], 'non-greedy 0 or 1 character class' ],
    [ q{ s ::= [a-z]{2} }, 'hello world', 1, [ 1 ], 'exactly 2 character class' ],
    [ q{ s ::= [a-z]{2,} }, 'hello world', 1, [ 1 ], '2 or more character class' ],
    [ q{ s ::= [a-z]{2,3} }, 'hello world', 1, [ 1 ], '2 or 3 character class' ],
    [ q{ s ::= [a-z]{2}? }, 'hello world', 1, [ 1 ], 'non-greedy 2 or more character class' ],
    [ q{ s ::= [a-z]{2,}? }, 'hello world', 1, [ 1 ], 'non-greedy 2 or more character class' ],
    [ q{ s ::= [a-z] { 2 , 3 } ? }, 'hello world', 1, [ 1 ], 'non-greedy 2 or 3 character class' ],
    [ q{ s ::= [a-z]{2}+ }, 'hello world', 1, [ 1 ], 'non-backtracking (possessive) exactly 2 character class' ],
    [ q{ s ::= [a-z]{2,}+ }, 'hello world', 1, [ 1 ], 'non-backtracking 2 or more character class' ],
    [ q{ s ::= [^a-z] { 2 , 3 } + }, 'hello world', '', [], 'non-backtracking 2 or 3 character class' ],
    [ q{ s ::= 'lit[a-z]ral' }, 'literal', 1, [ 1 ], 'embedded in literal character class' ],
    # alternation
    [ q{ s ::= 'cat' | 'dog' | 'bird' }, "cats and dogs", 1, [ 1 ], 'alternation: match literal the first alternative' ],
    [ q{ s ::= 'dog' | "cat" | 'bird' }, "cats and dogs", 1, [ 1 ], 'alternation: match literal earlier in the string' ],
    [ q{ s ::= 'c' | 'ca' | 'cat' | "cats" }, "cats", 1, [ 1 ], 'alternation, match character class at the first string position' ],
    [ q{ s ::= "cats" | 'cat' | 'ca' | 'c' }, "cats", 1, [ 1 ], 'alternation, match character class at the first string position' ],
    [ q{ s ::= [cat]{3} | [dog]+ | [^bird]* }, "cats and dogs", 1, [ 1 ], 'alternation, match character classes' ],
    # grouping
    [ q{ s ::= ( 'a' | 'b')  }, "ab", 1, [ 'a' ], 'grouping' ],
    [ q{ s ::= ( 'a' | 'b' ) 'b' }, "bb", 1, [ 'b' ], 'grouping' ],
    [ q{ s ::= [b] ( 'a' | 'b' ) }, "bb", 1, [ 'b' ], 'grouping' ],
    [ q{ s ::= ( 'ac' | 'b' ) 'b' }, [ 'acb', "bb" ], [ 1, 1 ], [ 'ac', 'b' ], 'grouping' ],
    # matches 'ac' at start of string or 'bc' anywhere
    # todo: test error, e.g. unbalanced parens: ( ('^a'|'b')'c'
    [ q{ s ::= ( ^ 'a' | 'b' ) 'c' }, [ 'ac', "bc" ], [ 1, 1 ], [ 'a', 'b' ], 'grouping' ],
    # matches 'ad', 'bd', or 'cd'
    [ q{ s ::= ('a'|[bc])'d' }, [ 'ad', 'bd', 'cd' ], [ 1, 1, 1 ], [ 'a', 'b', 'c' ], 'grouping with character class' ],

    [ q{ s ::= 'house' ( 'cat' | ) }, [ 'housecat', 'house' ], [ 1, 1 ], [ 'cat', '' ], 'grouping, empty alternative' ],
    [ q{ s ::= 'house' ( 'cat' ( 's' |)|) },
        [ 'housecats',      'housecat',    'house' ],
        [ 1,                1,             1 ],
        [ [ 'cats', 's' ],  [ 'cat', '' ], [ '', undef ] ],
        'grouping, nested' ],
     # match years 19xx, 20xx, or the Y2K problem, xx
    [ q{ s ::= ( '19' | '20' | ) \d \d },
        [ '1901', '2001', '20' ],
        [ 1,      1,      1 ],
        [ '19', '20', '' ],
        'grouping, years' ],
    # Building a regexp
    [ q{ s ::= ( nineteen | twenty | ) \d\d
         nineteen ::= '19'
         twenty   ::= '20' },
        [ '1901', '2001', '20' ],
        [ 1,      1,      1 ],
        [ '19', '20', '' ],
        'grouping, years' ],
    [ q{
         s ::= ^(<optional sign>)(<f.p. mantissa> | integer)(<optional exponent>)$

         <optional sign> ::= [+-]?

         <f.p. mantissa> ::= \d+\.\d+  # mantissa of the form a.b
         <f.p. mantissa> ::= \d+\.     # mantissa of the form a.
         <f.p. mantissa> ::= \.\d+     # mantissa of the form .b

         integer         ::= \d+       # integer of the form a

         <optional exponent> ::= ([eE][+-]?\d+)?

        }, '1.3', 1, '1.3', 'building a regexp, unfactored form' ]

];

=pod

    //;

    /^[+-]?(\d+\.\d+|\d+\.|\.\d+|\d+)([eE][+-]?\d+)?$/;  # Ta da!

=cut

my $slg = Marpa::R2::Scanless::G->new( { source  => \$dsl } );

sub translate{
    my ($ast) = @_;
    state $depth++;
    my $s;
    my $indent = "  " x ($depth - 1);
    if (ref $ast){
        my ($node_id, @children) = @$ast;
        if ($node_id eq 'statement'){
            warn Dumper $node_id, \@children;
            my $lhs = $children[0]->[1]->[1]->[1]->[1];
            warn "lhs: ", Dumper $lhs;
            warn "alternatives: ", Dumper $children[0]->[2];
            $s .= "(?#$lhs)" . '(?:' . join('', map { translate( $_ ) } $children[0]->[2] ) . ')';
        }
#        elseif ($node_id eq 'alternatives'){
#
#        }
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
        { no warnings; $re_compiles = eval { qr/$re/x } };
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
