use 5.010;
use strict;
use warnings;

use Test::More;

use MarpaX::Regex;

my $input = <<EOI;
../cpan/lib/dev/file.c(11824) : warning C4820: '__unnamed' : '3' bytes padding added after member 'c'
../cpan/lib/dev/file.c(12464) : warning C4100: 'param' : unreferenced formal parameter
file.c(12538) : warning C4127: conditional expression is constant
EOI

my $source = q{

    warning ::= (file) '\(' (line) '\)' ' : warning ' (code) ': ' (message) [\n]

    file    ::= [\w\-:\/ \\\.]+  # windows and unix paths
    line    ::= int
    code    ::= 'C' int
    message ::= [\w\' :\-]+ #'   # [: and :] form part of POSIX character classes
    int     ::= [\d]+

};

my $expected = [
    [ '../cpan/lib/dev/file.c', '11824', 'C4820', "'__unnamed' : '3' bytes padding added after member 'c'" ],
    [ '../cpan/lib/dev/file.c', '12464', 'C4100', "'param' : unreferenced formal parameter" ],
    [ 'file.c', '12538', 'C4127', 'conditional expression is constant' ]
];

MarpaX::Regex->new($source);

# must parse unambiguously unless parse error is expected
my $regex = MarpaX::Regex->new($source);
chomp $regex; # no extended patterns this time
diag $regex unless $ENV{HARNESS_ACTIVE};

my $ln = 0;
while ($input =~ m/$regex/gc){
    is_deeply $expected->[$ln++], [ $1, $2, $3, $4 ], "warning $ln match";
}

done_testing();
