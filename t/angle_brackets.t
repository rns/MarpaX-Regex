use 5.010;
use strict;
use warnings;

use Test::More;

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Terse = 1;
$Data::Dumper::Deepcopy = 1;

my $string =<<"HERE";
I have some <brackets in <nested brackets> > and
<another group <nested once <nested twice> > >
and that's it.
HERE
#'

my @groups = $string =~ m/
	(                   # start of capture group 1
	<                   # match an opening angle bracket
			(?:
					[^<>]++     # one or more non angle brackets, non backtracking
						|
					(?1)        # found < or >, so recurse to capture group 1
			)*
	>                   # match a closing angle bracket
	)                   # end of capture group 1
	/xg;

# bnf:
my $BNFish = q{
    <balanced brackets> ::=
        (
            '<'
                (?:
                    <non angle brackets>
                        |
                    <balanced brackets>
                )*
            '>'
        )

    <non angle brackets> ::= [^<>]++ # non-backtracking
};

=pod
# regex:

    # pretty:
        (?<balanced_brackets>
            <
                (?:
                    [^<>]++
                        |
                    (?&balanced_brackets)
                )*
            >
        )

    # compact:

        (?<balanced_brackets><(?:[^<>]++|(?&balanced_brackets))*>)


Wrap alternatives in

[ '#text', '(?<' . $lhs> ')' } ]
[ ]
[ '#text', ')' ]

Replace
[ 'bare name', ]

bracketed name '<balanced brackets>'
[ '#text', '(?&' . 'balanced_brackets' . ')' ]

$ast->child(ast->IX_BEFORE_FIRST, $child);
$ast->child(ast->IX_AFTER_LAST, $child);
$ast->child(sub{ $_->[0] eq 'bracketed name' }, $child);
my ($child, $ix) = $ast->child( sub{ $_->[0] eq 'bracketed name' } );
my (undef, $ix) = $ast->child( sub{ $_->[0] eq 'bracketed name' } );

    named capture groups
         If you prefer to name your groups, you can use (?&name) to recurse into that group.

    # cannot skip group nodes -- they are needed to pretty print and comment

=cut

$" = "\n\t";
say "Found:\n\t@groups\n";

use MarpaX::Regex;
use MarpaX::Regex::AST;

# must parse unambiguously unless parse error is expected
my $rex = MarpaX::Regex->new;
my $value = eval { $rex->parse($BNFish) };
ok !$@, 'Regex BNF parsed';

my $ast = MarpaX::Regex::AST->new($value);
my $regex = $ast->distill->substitute->recurse->concat;
diag $regex;

done_testing();
