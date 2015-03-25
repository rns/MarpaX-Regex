use 5.010;
use strict;
use warnings;

use Test::More;

use MarpaX::Regex;
use MarpaX::Regex::AST;

#
# Recursive Pattern #1,  "paste method", http://www.rexegg.com/regex-recursion.html
#

my $string = 'aaa111bbb222';

my $re = '\w{3}\d{3}(?R)?';
like $string, qr/$re/, '"paste method", recursive pattern 1, regex';

my $BNFish_re = q{ <recursive pattern 1> ::= \w{3}\d{3} <recursive pattern 1> ? };
my $regex = MarpaX::Regex->new($BNFish_re);
like $string, qr/$regex/x, '"paste method", recursive pattern 1, BNFish';

#
# Recursive Pattern #2,  "paste method", http://www.rexegg.com/regex-recursion.html
#

my @strings = ( "abc", "abcabc" );

$re = 'abc(?:$|(?R))';
for $string (@strings){
    like $string, qr/$re/, '"paste method", recursive pattern 2, regex';
}
unlike "abc123", qr/$re/, '"paste method", recursive pattern 2, regex';

$BNFish_re = q{ <recursive pattern 2> ::= 'abc'(?:$|<recursive pattern 2>) };
$regex = MarpaX::Regex->new($BNFish_re);
for $string (@strings){
    like $string, qr/$regex/x, '"paste method", recursive pattern 2, BNFish';
}
unlike "abc123", qr/$regex/x, '"paste method", recursive pattern 2, BNFish';

# more recursion tests
#   http://www.rexegg.com/regex-trick-line-numbers.html#recursion
#   http://www.rexegg.com/regex-quantifier-capture.html#recursion

#
# balanced angle brackets (perlretut)
#
$string =<<"HERE";
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

# must parse unambiguously unless parse error is expected
my $rex = MarpaX::Regex->new;
my $value = eval { $rex->parse($BNFish) };
ok !$@, 'angle brackets Regex BNF parsed';

my $ast = MarpaX::Regex::AST->new($value);
$regex = $ast->distill->substitute->recurse->concat;
chomp $regex;
is $regex, $expected_regex, "angle brackets regex translate";

my @groups = $string =~ m/$regex/xg;
is_deeply \@groups, \@expected_groups, "angle brackets regex match";

#
# palindromes (perlretut)
#

my @palindromes = ( "saippuakauppias", "A man, a plan, a canal: Panama!" );

my $pp = qr/
    (?<palindrome>
        [\W]*
        (?:
            ([\w]) (?&palindrome) \1 | [\w]?
        )
        [\W]*
    )
    /ix;

for my $s ( @palindromes ){
    like $s, qr/$pp/ix, , "'$s' is a palindrome (regex)";
}

my $BNFish_pp = q{
    palindrome      ::= <to be ignored>
                            (?:
                              # (char) palindrome '\1' | char? also works
                                (char) palindrome [\1] | char?
                            )
                        <to be ignored>
    <to be ignored> ::= [\W]*
    char            ::= [\w]
};

$regex = MarpaX::Regex->new($BNFish_pp);

for my $s ( @palindromes ){
    like $s, qr/$regex/ix, "'$s' is a palindrome (BNFish)";
}

done_testing();
