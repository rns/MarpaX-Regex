use 5.010;
use strict;
use warnings;

use Test::More;

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Terse = 1;
$Data::Dumper::Deepcopy = 1;

use MarpaX::Regex;

# following http://perldoc.perl.org/perlretut.html
# Regex BNF source, input string, scalar-context match, list-context match, desc
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

# the below 2 tests show the difference between empty capturing group
# supported as is or via empty rule
    [ q{ s ::= 'house' ( 'cat' | ) }, [ 'housecat', 'house' ], [ 1, 1 ], [ 'cat', '' ],
        'grouping, empty alternative as null regex' ],
#    [ q{ s ::= 'house' ( 'cat' | <empty> )
#         <empty> ::=
#        }, [ 'housecat', 'house' ], [ 1, 1 ], [ 'cat', '' ],
#        'grouping, empty alternative by empty rule' ],
#
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
        }, '1.3', 1, '1.3', 'building a regexp, unfactored form' ],
];

=pod more tests

    [ q{
         number              ::= ^ (<optional sign>) (<f.p. mantissa> | integer) <optional exponent> $
         <optional sign>     ::= [+-]?
         <f.p. mantissa>     ::= integer '.' integer # mantissa of the form a.b
                               | digit+ '.'          # mantissa of the form a.
         <f.p. mantissa>     ::= '.' digit+          # mantissa of the form .b
         integer             ::= integer             # integer of the form a
         <optional exponent> ::= ([eE][+-]?\d+)?
         digit               ::= \d
        }, '1.3', 1, '1.3', 'building a regexp, partially integer-factored form' ],
    [ q{
         number              ::= ^ <optional sign> (<f.p. mantissa> | integer) <optional exponent> $
         <optional sign>     ::= [+-]?
         <f.p. mantissa>     ::= integer '.' integer  # mantissa of the form a.b
                               | integer '.'          # mantissa of the form a.
         <f.p. mantissa>     ::= '.' integer          # mantissa of the form .b
         integer             ::= integer              # integer of the form a
         <optional exponent> ::= ( [eE][+-]? integer )?
         integer             ::= digit+
         digit               ::= \d
        }, '1.3', 1, '1.3', 'building a regexp, fully integer-factored form' ],
    # possible todo: feature: infer a more compact form below from the ast
    # possible todo: feature: assemble empty rules to form ()? groups
    #    /^[+-]?\ *(\d+(\.\d*)?|\.\d+)([eE][+-]?\d+)?$/;


/^
            [+-]?\ *      # first, match an optional sign
            (             # then match integers or f.p. mantissas:
                    \d+       # start out with a ...
                    (
                            \.\d* # mantissa of the form a.b or a.
                    )?       # ? takes care of integers of the form a
                  |\.\d+     # mantissa of the form .b
            )
            ([eE][+-]?\d+)?  # finally, optionally match an exponent
      $/x;

or written in the compact form,

    /^[+-]?\ *(\d+(\.\d*)?|\.\d+)([eE][+-]?\d+)?$/;

=cut

sub ast_find{
    my ($ast, $code ) = @_;
    my $found = [];
    sub find{
        my ($node, $code, $found) = @_;
        if (ref $node){
            my ($node_id, @children) = @$node;
            if ( $code->($node) ){
                local $Data::Dumper::Indent = 0;
                push @$found, $node;
            }
            find ($_, $code, $found) for @children;
        }
    }
    find($ast, $code, $found);
    return $found;
}

sub terminals{
    my ($ast) = @_;
    return ast_find(
        $ast,
        sub {
            my ($node_id, @children) = @{ $_[0] };
            if ($node_id eq 'statement'){
                for my $child (@children){
                    my $symbols = ast_find($child, sub { $_[0]->[0] eq 'symbol' } );
                    return 0 if @$symbols;
                }
                return 1;
            }
        }
    );
}

sub primaries{
    my ($ast) = @_;

    local $Data::Dumper::Indent = 0;

    return ast_find( $ast, sub {
        my ($node_id, @children) = @{ $_[0] };
        return 0 unless defined $node_id;
#        warn "# group \$children[0]:\n", Dumper $children[0];
        return 0 unless $_[0]->[0] eq 'group';
#        return 0 unless ref $children[0] eq "ARRAY";
#        return 0 unless ref $children[0]->[1] eq "ARRAY";
#        return 1 if $children[0]->[1] eq 'primary';
        return 0 unless ref $_[0]->[1] eq "ARRAY";
        return 1 if $_[0]->[1]->[0] eq 'primary';
        return 0;
    } );
}

sub inaccessible{
    my ($ast, $symbol) = @_;
    my $found = ast_find( $ast, sub {
#        warn $_[0]->[0];
        return 0 unless $_[0]->[0] eq 'symbol name';
        return $_[0]->[1]->[1] eq $symbol;
    } );
#    warn "$symbol: ", @$found;
    return @$found == 1;
}

sub delete_statement_by_lhs{
    my ($ast, $lhs) = @_;

    local $Data::Dumper::Indent = 0;
    my ($node_id, @children) = @{ $ast };
    # delete_child(sub{})
    # find index
    my $ix = undef;
    for my $i (0..@children-1){
        my $statement = $children[$i];
#        warn "# statement: ", Dumper $statement;
        my $lhs_i = $statement->[1]->[1]->[1]->[1]->[1];
#        warn Dumper $i, $lhs_i;
        $ix = $i if $lhs eq $lhs_i;
    }
#    warn "# statements: ", Dumper $ast;
#    warn "deleting statement ", Dumper $ast->[$ix+1];
    splice( @{ $ast }, $ix + 1, 1 ) if defined $ix;
}

sub substitute {
    my ($ast) = @_;

    local $Data::Dumper::Indent = 0;

SUBSTITUTE:
    for my $subst_iter (0..10){

        my $terminals = terminals($ast);
        last SUBSTITUTE unless @$terminals;
        my $substitutes = {};
        for my $t (@$terminals){
            warn "# terminal:\n", Dumper($t);
            my ($node_id, @children) = @$t;
            my $lhs = $children[0]->[1]->[1]->[1]->[1];
            # get terminal's alternatives group
            my $group = $children[0]->[2]->[1];
            # TODO: merge terminals with the same lhs
            # to a group joined with '|'
            push @{ $substitutes->{$lhs} }, [ $t, $group ];
        }
    #    warn "# substitutes:\n", Dumper $substitutes;

        my $primaries = primaries($ast);
        for my $p (@$primaries){
#            warn "# primary:\n", Dumper($p);
            next unless ref $p->[1]->[1] eq "ARRAY";
            next unless $p->[1]->[1]->[0] eq "symbol";
    #        warn Dumper $p->[1]->[1]->[1]->[1]->[1];

            my $symbol_name = $p->[1]->[1]->[1]->[1]->[1];
            next unless exists $substitutes->{$symbol_name};
            warn "\n# substitute $symbol_name group\n", Dumper($p);
            warn "before substitution:", Dumper $ast;

            for my $subst (@{ $substitutes->{$symbol_name} } ){
                my ($statement, $group) = @$subst;
                warn "with this group:\n  ", Dumper $group;
#                warn "and set up new terminal statement for the next cycle:\n  ",
#                    Dumper $statement,
#                    "if there are references to it from other rules (unlike <optional sign>, which should be deleted)";
                # this doesn't work for digit group
                $p->[1]->[1] = $group;
            }
    #        local $Data::Dumper::Indent = 1;
            warn "after substitution:", Dumper $ast;
            my $inaccessible = inaccessible($ast, $symbol_name);
            if ($inaccessible) {
                warn "$symbol_name is inaccessible, deleting";
                delete_statement_by_lhs($ast, $symbol_name);
            }
        }
    } ## for
    warn "after all substitutions:", Dumper $ast;
}

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
    my $rex = MarpaX::Regex->new;
    my $ast = eval { $rex->parse($source) };
    if ($must_parse){
        ok !$@, 'BNF parsed';
    }
    else{
        ok $@, 'BNF not parsed, as expected';
    }

    # reparse with trace_terminals and progress unless parse error expected
    if ($@ and $must_parse){
        diag $rex->parse_debug($source);
    }

    SKIP: {

        skip "BNF source parse error", 3 unless defined $ast and $must_parse;

#        $ast = eval(Dumper( $ast )); # we need data, not refs
        substitute( $ast );
        my $re = $rex->translate( $ast );
        diag "RE: /$re/";

        my $re_compiles;
        { no warnings; $re_compiles = eval { qr/$re/x } };
        ok !$@, "$desc: compile";

        SKIP: {

            skip "RE doesn't compile", @$input - 1 unless $must_compile and $re_compiles;

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
