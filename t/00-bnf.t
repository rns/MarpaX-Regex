use Test::More;

use MarpaX::Regex;

warn 1;

my $bnf = MarpaX::Regex->new(<<'BNF'); # qingle quotes are important for escapes or use q{}  

# comment
start ::= seq | <another seq> | alt | <lhs with spaces> | ( <hidden empty rule lhs> )

seq ::= seq_item+ separator => [ ] proper => 1 # comment
item ::= '"item"' | "'item'" | [item1]

<another seq> ::= item1* separator => [,] proper => 0 # comment
item1 ::= '"item1"' | "'item1'" | [item1 [:alpha:] a-zA-z ^A-Z [:digit:] \p{XPosixAlpha} \[ \]]

alt ::= s11 s12 | s21 s22 

<lhs with spaces> ::= <rhs symbol with spaces> | <another rhs symbol with spaces>

<rhs symbol with spaces> ::= [item1 [:alpha:] [:digit:] \p{XPosixAlpha} \[ \]]+

<hidden empty rule lhs> ::=

cc ::= [ ]
cc ::= [a-z]
cc ::= [a-z]*
cc ::= [a-z]+
cc ::= [a-z]?
cc ::= [a-z]*?
cc ::= [a-z]+?
cc ::= [a-z]??
cc ::= [a-z]{2}
cc ::= [a-z]{2,}
cc ::= [a-z]{2,3}
cc ::= [a-z]{2}?
cc ::= [a-z]{2,}?
cc ::= [a-z]{2,3}?

BNF

warn 1;

warn $bnf->fmt();
