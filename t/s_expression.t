# http://pastebin.com/ndrHTZJB
use strict;
use warnings;

use Test::More;

use MarpaX::Regex;

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Terse = 1;
$Data::Dumper::Deepcopy = 1;

my $expr = "(print (add 3 (add (min 9 3) 4)))";
my $ast = [
  [
    [
      {
        'token' => 'print'
      },
      [
        {
          'token' => 'add'
        },
        {
          'token' => '3'
        },
        [
          {
            'token' => 'add'
          },
          [
            {
              'token' => 'min'
            },
            {
              'token' => '9'
            },
            {
              'token' => '3'
            }
          ],
          {
            'token' => '4'
          }
        ]
      ]
    ]
  ]
];

my $stack = [];

sub op {
  push @$stack, [];
}

sub clos {
  my $t = pop @$stack;
  my $n = pop @$stack;
  push @$n, $t;
  push @$stack, $n;
}

sub st {
    my $x = shift;
    my $t = pop @$stack;
    push @$t, {token => $x};
    push @$stack, $t;
}

my $re = q {
    (?(DEFINE)
        (?<WS> \s*)
        (?<l> \( (?{&op()}) )
        (?<r> \) (?{&clos()}))
        (?<token>(?&WS)
            ([a-zA-Z0-9]+)
            (?&WS) (?{&st($+)}))
        (?<expr> (?&l)(?&WS) (?: (?&expr)+  | (?&token)+ )+ (?&WS) (?&r) )

    )
    (?<tks> (?&expr))

};

use re 'eval';
like $expr, qr/$re/six, "s-expression regex match";
is_deeply $stack, $ast, "s-expression regex value";

my $BNFish = q {

    <WS>    ::= \s*
    <l>     ::= '\(' '(?{&op()})'
    <r>     ::= '\)' '(?{&clos()})'
    <token> ::= <WS> (<name>) <WS> '(?{&st($+)})'
    <name>  ::= [a-zA-Z0-9]+
    # (?:<token>)+ because "(?{&st($+)})+ matches null string many times in regex"
    # warning is issued otherwise
    <expr>  ::= <l> <WS> (?: <expr>+  | (?:<token>)+ )+ <WS> <r>

};

my $regex = MarpaX::Regex->new($BNFish);
#diag $regex;
$stack = [];
like $expr, qr/$regex/six, "s-expression BNFish match";
is_deeply $stack, $ast, "s-expression BNFish value";

done_testing();
