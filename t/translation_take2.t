use 5.010;
use strict;
use warnings;

use utf8;

use Test::More;

binmode Test::More->builder->output, ":utf8";
binmode Test::More->builder->failure_output, ":utf8";

binmode STDOUT, ":utf8";
binmode STDERR, ":utf8";

use Marpa::R2;

use Data::Dumper;
$Data::Dumper::Indent = 0;
$Data::Dumper::Terse = 1;
$Data::Dumper::Deepcopy = 1;

use YAML;

=pod test layout

    source grammars, their inputs and ASTs
    translation grammars = { 'source' => 'target' => grammar }
    iterate over source grammars
    if there is source->target in the translation grammars
    translate and test the result against source input

=cut

=pod
    2 + 3                         -- infix
    (+ 2 3)                       -- prefix
    (2 3 +)                       -- postfix
    bipush 2                      -- JVM
    bipush 3
    iadd
    the sum of 2 and 3            -- English
    -- http://www.cse.chalmers.se/edu/year/2012/course/DAT150/lectures/proglang-02.html

    Expression ::=
    Number
        | '(' Expression ')' assoc => group
       || Expression '**' Expression assoc => right
       || Expression '*' Expression
        | Expression '/' Expression
       || Expression '+' Expression
        | Expression '-' Expression

    -- https://metacpan.org/pod/distribution/Marpa-R2/pod/Scanless/DSL.pod

    A synchronous context free grammar for time normalization
    -- http://aclweb.org/anthology//D/D13/D13-1078.pdf
    -- https://github.com/bethard/timenorm

    So I excel sheet I have string like below:

    If ((Myvalue.xyz == 1) Or (Frame_1.signal_1 == 1)) Then a = 1
    Else a = 0;

    This I have to convert into:

    a = (((Myvalue.xyz == 1) || (Frame_1.signal_1 == 1))?1:0)
    -- http://longanswers.blogspot.de/2013/06/transforming-syntax.html

    SLIF <-> BNF
        <non-terminal>
        terminal
        'lexeme'
        "lexeme"

    SLIF <-> EBNF

    EBNF <-> BNF

    Marpa <-> Perl Regexes

=cut

#
# grammars to be translated, translations grammars
#
my $grammar_prolog = q{
    :default ::= action => [ name, values]
    lexeme default = action => [ name, values] latm => 1
};

my $grammar_epilog = q{
    :discard ~ whitespace
    whitespace ~ [\s+]
};

my $tests = {
    # nested quotes grammar based on http://marvin.cs.uidaho.edu/Teaching/CS445/grammar.html
    # typewriter double quotes (to be translated to curly (“...”) quotes)
    'well-formed typewriter double quotes' => [
        [
            '"these are "words in typewriter double quotes" and then some"',
            '"these are "words in "nested typewriter double" quotes" and then some"',
            '"these are "words in "nested "and even more nested" typewriter double" quotes" and then some"'
        ],
        q{
            S       ::= '"' quoted '"'
            quoted  ::= item | quoted item
            item    ::= S | unquoted

            unquoted ~ [^"]+ # "
        },
        [
            ['S','"',['quoted',['quoted',['quoted',['item',['unquoted','these are ']]],['item',['S','"',['quoted',['item',['unquoted','words in typewriter double quotes']]],'"']]],['item',['unquoted',' and then some']]],'"'],
            ['S','"',['quoted',['quoted',['quoted',['item',['unquoted','these are ']]],['item',['S','"',['quoted',['quoted',['quoted',['item',['unquoted','words in ']]],['item',['S','"',['quoted',['item',['unquoted','nested typewriter double']]],'"']]],['item',['unquoted',' quotes']]],'"']]],['item',['unquoted',' and then some']]],'"'],
            ['S','"',['quoted',['quoted',['quoted',['item',['unquoted','these are ']]],['item',['S','"',['quoted',['quoted',['quoted',['item',['unquoted','words in ']]],['item',['S','"',['quoted',['quoted',['quoted',['item',['unquoted','nested ']]],['item',['S','"',['quoted',['item',['unquoted','and even more nested']]],'"']]],['item',['unquoted',' typewriter double']]],'"']]],['item',['unquoted',' quotes']]],'"']]],['item',['unquoted',' and then some']]],'"']
        ]
    ],
    # curly double quotes
    'well-formed curly double quotes' => [
        [
            '“these are “words in curly double quotes” and then some”',
            '“these are “words in “nested curly double” quotes” and then some”',
            '“these are “words in “nested “and even more nested” curly double” quotes” and then some”'
        ],
        q{
            S       ::= '“' quoted '”'
            quoted  ::= item | quoted item
            item    ::= S | unquoted

            unquoted ~ [^“”]+
        },
        [
            ['S','“',['quoted',['quoted',['quoted',['item',['unquoted','these are ']]],['item',['S','“',['quoted',['item',['unquoted','words in curly double quotes']]],'”']]],['item',['unquoted',' and then some']]],'”'],
            ['S','“',['quoted',['quoted',['quoted',['item',['unquoted','these are ']]],['item',['S','“',['quoted',['quoted',['quoted',['item',['unquoted','words in ']]],['item',['S','“',['quoted',['item',['unquoted','nested curly double']]],'”']]],['item',['unquoted',' quotes']]],'”']]],['item',['unquoted',' and then some']]],'”'],
            ['S','“',['quoted',['quoted',['quoted',['item',['unquoted','these are ']]],['item',['S','“',['quoted',['quoted',['quoted',['item',['unquoted','words in ']]],['item',['S','“',['quoted',['quoted',['quoted',['item',['unquoted','nested ']]],['item',['S','“',['quoted',['item',['unquoted','and even more nested']]],'”']]],['item',['unquoted',' curly double']]],'”']]],['item',['unquoted',' quotes']]],'”']]],['item',['unquoted',' and then some']]],'”']
        ]
    ],
    #    2 + 3                         -- infix
    infix => [
        [ '2 + 3', '2 * 3' ],
        q{
            e   ::= int plus int name => 'add' |
                    int star int name => 'mul'
            int  ~ [\d]
            plus ~ '+'
            star ~ '*'
        },
        [
            ['e/add','(',['int','2'],['int','3'],['plus','+'],')'],
            ['e/mul','(',['int','2'],['int','3'],['star','*'],')']
        ]
    ],
    #    (+ 2 3)                       -- prefix
    prefix => [
        [ '(+ 2 3)', '(* 2 3)' ],
        q{
            e   ::= '(' plus int int ')' name => 'add' |
                    '(' star int int ')' name => 'mul'
            int  ~  [\d]
            plus ~ '+'
            star ~ '*'
        },
        [
            ['e/add','(',['plus','+'],['int','2'],['int','3'],')'],
            ['e/mul','(',['star','*'],['int','2'],['int','3'],')']
        ]
    ],
    #    (2 3 +)                       -- postfix
    postfix => [
        [ '(2 3 +)', '(2 3 *)' ],
        q{
            e   ::= '(' int int plus ')' name => 'add' |
                    '(' int int star ')' name => 'mul'
            int  ~ [\d]
            plus ~ '+'
            star ~ '*'
        },
        [
            ['e/add','(',['int','2'],['int','3'],['plus','+'],')'],
            ['e/mul','(',['int','2'],['int','3'],['star','*'],')']
        ]
    ],
    #    bipush 2                      -- JVM
    #    bipush 3
    #    iadd
    JVM => [
        [
            q{
                bipush 2
                bipush 3
                iadd
            },
            q{
                bipush 2
                bipush 3
                imul
            },
        ],
        q{
            e    ::= push push add name => 'add' |
                     push push mul name => 'mul'
            push ::= 'bipush' int
            int  ~ [\d]+
            add  ~ 'iadd'
            mul  ~ 'imul'
        },
        [
            ['e/add',['push','bipush',['int','2']],['push','bipush',['int','3']],['add','iadd']],
            ['e/mul',['push','bipush',['int','2']],['push','bipush',['int','3']],['mul','imul']]
        ]
    ],
    #    the sum of 2 and 3            -- English
    English => [
        [ 'the sum of 2 and 3', 'the product of 2 and 3' ],
        q{
            e ::= 'the' op 'of' int 'and' int
            op ~ 'sum' name => 'add' | 'product' name => 'mul'
            int  ~ [\d]
        },
        [
            ['e','the',['op','sum'],'of',['int','2'],'and',['int','3']],
            ['e','the',['op','product'],'of',['int','2'],'and',['int','3']]
        ]
    ],
};

=pod Given 2 BNFs, describe transformations from BNF1 AST to BNF2 AST

    rearrangement
         e/add ::= 1 = 2            # indices
         e/mul ::= 2 = 1
         e/add ::= 1, 2 = 2, 1
         # node_id lhs[index] in children array
         e/add ::= source_lhs[1], source_lhs[2] = source_lhs[2], source_lhs[1]
    substitution
         e/mul ::= source_lhs = target_lhs
         e/add ::= 0..3 = target_lhs
         e/mul ::= 0..1 = target_lhs1, target_lhs2
    insertion/removal of terminals
         e/add ::= 0, 3 += '(', ')' # insert '(' at index 0, ')' at index 3
         e/add ::= 0, 3 -= '(', ')' # remove '(' at index 0, ')' at index 3
    insertion/removal of non-terminals
         e/add ::= 0..1 += lhs1, lhs2
         e/add ::= 0..1 -= lhs1, <lhs2 I really need here>
            # <> are just symbols markers to allow spaces like SLIF
    # rule-to-rule correspondence
    # no rule-to-rule correspondence, some transfer rules are needed
         the above operations are on the children of a parent node

=cut

my $transducers = {
    infix => {
        postfix => q{
            e/add ::=   1, 2 = 2, 1
            e/mul ::=   1, 2 = 2, 1
            e/add ::=   0, 3 += '(', ')'
            e/mul ::=   0, 3 -= '(', ')'
        }
    }
};

# todo: add prolog and epilog to all grammars
#
for my $source (sort keys %$tests){
    my ($source_inputs, $source_grammar, $trees) = @{ $tests->{ $source } };

    # translate if there is translation grammar
    for my $target ( keys %$tests ){ # target name

        next if $source eq $target; # translation into itself is trivial :)

        my $tg = $transducers->{$source}->{$target};
        next unless defined $tg; # skip non-existing tables

        diag "$source -> $target";
        # prepare transducer
        # parse transducer grammar
        $tg = $grammar_prolog . $tg . $grammar_epilog;
        my $sltg = Marpa::R2::Scanless::G->new( { source  => \$tg } );
        my $sltr = Marpa::R2::Scanless::R->new( {
            grammar => $sltg,
            ranking_method => 'high_rule_only',
            trace_terminals => 0,
        } );

        # now, parsing source inputs with $sltg should produce target's ASTs
        for my $i (0 .. @$source_inputs - 1){
            # parse source input with translation grammar
            eval { $sltr->read(\$si) } || warn "$@\nProgress report is:\n" . $sltr->show_progress;
            # transform ast with transducer grammar
            # test
        }
    }
}

done_testing();
