use Test::More;

use MarpaX::Regex;

my $bnf = MarpaX::Regex->new(<<BNF);

# comment
start ::= seq | alt | <lhs with spaces> | ( <hidden empty rule lhs> )

seq  ::= item+ separator => [ ] proper => 1 # comment
item ::= '"item"' | "'item'" | [item1]

seq1  ::= item* separator => [ ] proper => 1 # comment
item1 ::= '"item1"' | "'item1'" | [item1]

alt ::= s11 s12 | s21 s22 

<lhs with spaces> ::= <rhs with spaces> | <another rhs with spaces>

<rhs with spaces> ::=
<hidden empty rule lhs> ::=

BNF

warn $bnf->fmt();
