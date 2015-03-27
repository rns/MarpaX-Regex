# source: http://www.codeproject.com/Articles/297056/Most-Important-Regular-Expression-for-parsing-HTML

use strict;
use warnings;

use Test::More;

use MarpaX::Regex;

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Terse = 1;
$Data::Dumper::Deepcopy = 1;

my $html = <<END;
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2 Final//EN">
<HTML>
<HEAD>
<TITLE></TITLE>
<META NAME="Generator" CONTENT="TextPad 4.6">
<META NAME="Author" CONTENT="?">
<META NAME="Keywords" CONTENT="?">
<META NAME="Description" CONTENT="?">
</HEAD>

<BODY BGCOLOR="#FFFFFF" TEXT="#000000" LINK="#FF0000" VLINK="#800000" ALINK="#FF00FF" BACKGROUND="?">

</BODY>
</HTML>
END

# /<(\w+)((?:\s+\w+(?:\s*=\s*(?:(?:"[^"]*")|(?:'[^']*')|[^>\s]+))?)*)\s*(\/?)>/

my $re = q{
<
    (\w+)
    (
        (?:
            \s+\w+
            (?:
                \s*=\s*
                (?:
                    (?:"[^"]*") #"
                    |
                    (?:'[^']*') #'
                    |
                    [^>\s]+
                )
            )?
        )*
    )
    \s*
    (\/?)
>
};

use re 'eval';
like $html, qr/$re/x, "html regex match";

my $BNFish = q {

    html ::=
        '<'
            (<tag name>)
            (
                (?:
                    \s+\w+
                    (?:
                        \s* '=' \s*
                        (?:
                            attributes
                        )
                    )?
                )*
            )
            \s*
            (<empty element maybe>)
        '>'

    <tag name>                      ::= \w+

    attributes                      ::= (?:
                                            <double quote>
                                            <0 or more non-double quotes>
                                            <double quote>
                                        )
                                        |
                                        (?:
                                            <single quote>
                                            <0 or more non-single quotes>
                                            <single quote>
                                        )
                                        |
                                        <only 1 right-angled bracket>

    <double quote>                  ::= '"'
    <0 or more non-double quotes>   ::= [^"]* #"
    <single quote>                  ::= "'"
    <0 or more non-single quotes>   ::= [^']* #'
    <only 1 right-angled bracket>   ::= [^>\s]+

    <empty element maybe>           ::= [/]?
};

my $regex = MarpaX::Regex->new($BNFish);
like $html, qr/$regex/six, "html BNFish match";

done_testing();
