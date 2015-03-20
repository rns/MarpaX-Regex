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

my @expected_groups = $string =~ m/
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

my $expected_regex = '(?<balanced_brackets><(?:[^<>]++|(?&balanced_brackets))*>)';

use MarpaX::Regex;
use MarpaX::Regex::AST;

# must parse unambiguously unless parse error is expected
my $rex = MarpaX::Regex->new;
my $value = eval { $rex->parse($BNFish) };
ok !$@, 'Regex BNF parsed';

my $ast = MarpaX::Regex::AST->new($value);
my $regex = $ast->distill->substitute->recurse->concat;
chomp $regex;
is $regex, $expected_regex, "angle brackets regex translate";

my @groups = $string =~ m/$regex/xg;
is_deeply \@groups, \@expected_groups, "angle brackets regex match";

=pod
    my $pp = qr/^(\W* (?: (\w) (?1) \g{-1} | \w? ) \W*)$/ix;
    for $s ( "saippuakauppias", "A man, a plan, a canal: Panama!" ){
        print "'$s' is a palindrome\n" if $s =~ /$pp/;
    }
=cut

done_testing();
