use 5.010;
use strict;
use warnings;

use Test::More;

use MarpaX::Regex;
use MarpaX::Regex::AST;

# Gruber's URL regex

my $gruber_url_re = q{
# Commented multi-line version:
\b
(                           # Capture 1: entire matched URL
  (?:
    https?:             # URL protocol and colon
    (?:
      /{1,3}                        # 1-3 forward slashes
      |                             #   or
      [a-z0-9%]                     # Single letter or digit or '%'
                                    # (Trying not to match e.g. "URI::Escape")
    )
    |                           #   or
                                # looks like domain name followed by a slash:
    [a-z0-9.\-]+[.]
    (?:com|net|org|edu|gov|mil|aero|asia|biz|cat|coop|info|int|jobs|mobi|museum|name|post|pro|tel|travel|xxx|ac|ad|ae|af|ag|ai|al|am|an|ao|aq|ar|as|at|au|aw|ax|az|ba|bb|bd|be|bf|bg|bh|bi|bj|bm|bn|bo|br|bs|bt|bv|bw|by|bz|ca|cc|cd|cf|cg|ch|ci|ck|cl|cm|cn|co|cr|cs|cu|cv|cx|cy|cz|dd|de|dj|dk|dm|do|dz|ec|ee|eg|eh|er|es|et|eu|fi|fj|fk|fm|fo|fr|ga|gb|gd|ge|gf|gg|gh|gi|gl|gm|gn|gp|gq|gr|gs|gt|gu|gw|gy|hk|hm|hn|hr|ht|hu|id|ie|il|im|in|io|iq|ir|is|it|je|jm|jo|jp|ke|kg|kh|ki|km|kn|kp|kr|kw|ky|kz|la|lb|lc|li|lk|lr|ls|lt|lu|lv|ly|ma|mc|md|me|mg|mh|mk|ml|mm|mn|mo|mp|mq|mr|ms|mt|mu|mv|mw|mx|my|mz|na|nc|ne|nf|ng|ni|nl|no|np|nr|nu|nz|om|pa|pe|pf|pg|ph|pk|pl|pm|pn|pr|ps|pt|pw|py|qa|re|ro|rs|ru|rw|sa|sb|sc|sd|se|sg|sh|si|sj| Ja|sk|sl|sm|sn|so|sr|ss|st|su|sv|sx|sy|sz|tc|td|tf|tg|th|tj|tk|tl|tm|tn|to|tp|tr|tt|tv|tw|tz|ua|ug|uk|us|uy|uz|va|vc|ve|vg|vi|vn|vu|wf|ws|ye|yt|yu|za|zm|zw)
    /
  )
  (?:                           # One or more:
    [^\s()<>{}\[\]]+                        # Run of non-space, non-()<>{}[]
    |                               #   or
    \([^\s()]*?\([^\s()]+\)[^\s()]*?\)  # balanced parens, one level deep: (…(…)…)
    |
    \([^\s]+?\)                         # balanced parens, non-recursive: (…)
  )+
  (?:                           # End with:
    \([^\s()]*?\([^\s()]+\)[^\s()]*?\)  # balanced parens, one level deep: (…(…)…)
    |
    \([^\s]+?\)                         # balanced parens, non-recursive: (…)
    |                                   #   or
    [^\s`!()\[\]{};:'".,<>?«»“”‘’]      # not a space or one of these punct chars
  )
  |                 # OR, the following to match naked domains:
  (?:
    (?<!@)          # not preceded by a @, avoid matching foo@_gmail.com_
    [a-z0-9]+
    (?:[.\-][a-z0-9]+)*
    [.]
    (?:com|net|org|edu|gov|mil|aero|asia|biz|cat|coop|info|int|jobs|mobi|museum|name|post|pro|tel|travel|xxx|ac|ad|ae|af|ag|ai|al|am|an|ao|aq|ar|as|at|au|aw|ax|az|ba|bb|bd|be|bf|bg|bh|bi|bj|bm|bn|bo|br|bs|bt|bv|bw|by|bz|ca|cc|cd|cf|cg|ch|ci|ck|cl|cm|cn|co|cr|cs|cu|cv|cx|cy|cz|dd|de|dj|dk|dm|do|dz|ec|ee|eg|eh|er|es|et|eu|fi|fj|fk|fm|fo|fr|ga|gb|gd|ge|gf|gg|gh|gi|gl|gm|gn|gp|gq|gr|gs|gt|gu|gw|gy|hk|hm|hn|hr|ht|hu|id|ie|il|im|in|io|iq|ir|is|it|je|jm|jo|jp|ke|kg|kh|ki|km|kn|kp|kr|kw|ky|kz|la|lb|lc|li|lk|lr|ls|lt|lu|lv|ly|ma|mc|md|me|mg|mh|mk|ml|mm|mn|mo|mp|mq|mr|ms|mt|mu|mv|mw|mx|my|mz|na|nc|ne|nf|ng|ni|nl|no|np|nr|nu|nz|om|pa|pe|pf|pg|ph|pk|pl|pm|pn|pr|ps|pt|pw|py|qa|re|ro|rs|ru|rw|sa|sb|sc|sd|se|sg|sh|si|sj| Ja|sk|sl|sm|sn|so|sr|ss|st|su|sv|sx|sy|sz|tc|td|tf|tg|th|tj|tk|tl|tm|tn|to|tp|tr|tt|tv|tw|tz|ua|ug|uk|us|uy|uz|va|vc|ve|vg|vi|vn|vu|wf|ws|ye|yt|yu|za|zm|zw)
    \b
    [/]?
    (?!@)           # not succeeded by a @, avoid matching "foo.na" in "foo.na@example.com"
  )
)

};
# '

like 'https://mail.google.com', qr/$gruber_url_re/ix, "Gruber's URL pattern (regex)";

my $gruber_url_BNFish = q{
# Commented multi-line version:

    url ::=
        <word boundary>
        (                                       # Capture 1: entire matched URL
          (?:
            <URL protocol and colon>
            (?:
              <1-3 forward slashes>
              |
              <Single letter or digit or %>     # (Trying not to match e.g. "URI::Escape")
            )
            |
            <domain name followed by a slash>
          )
          (?:                                 # One or more:
            <run of non-space, non-()\<\>{}[]>  # angle brackets need escaping in a bracketed name
            |
            <balanced parens, one level deep>   # (…(…)…)
            |
            <balanced parens, non-recursive>    # (…)
          )+
          (?:                                   # End with:
            <balanced parens, one level deep>
            |
            <balanced parens, non-recursive>
            |
            <not a space or one of punct chars>
          )
          |                                     # OR, the following to match naked domains:
          (?:
            # assertions can be passed through as literals
            ('?<!@')                            # not preceded by a @, avoid matching foo@_gmail.com_
            <1 or more lower case chars and digits>
            (?:[.\-]<1 or more lower case chars and digits>)*
            [.]
            <tld>
            <word boundary>
            <0 or 1 forward slash>
            ('?!@')                             # not succeeded by a @, avoid matching
                                                # "foo.na" in "foo.na@example.com"
          )
        )
        #'

    <URL protocol and colon>            ::= (?: 'https' ) ? ':'
    <1-3 forward slashes>               ::= [/]{1,3}
    <Single letter or digit or %>       ::= [a-z0-9%]
    <balanced parens, one level deep>   ::= '\(' [^\s()]*? '\(' [^\s()]+ '\)' [^\s()]*? '\)'  # : (…(…)…)
    <balanced parens, non-recursive>    ::= '\(' [^\s]+? '\)' # (…)
    <domain name followed by a slash>   ::= [a-z0-9.\-]+[.] <tld> '/'
    <run of non-space, non-()\<\>{}[]>  ::= [^\s()<>{}\[\]]+
    <not a space or one of punct chars> ::= [^\s`!()\[\]{};:'".,<>?«»“”‘’] #'
    <word boundary>                     ::= \b
    <0 or 1 forward slash>              ::= [/]?
    <1 or more lower case chars and digits>  ::= [a-z0-9]+
    <tld> ::= (?:
        'com' | 'net' | 'org' | 'edu' | 'gov' | 'mil' | 'aero' | 'asia' | 'biz' | 'cat' |
        'coop' | 'info' | 'int' | 'jobs' | 'mobi' | 'museum' | 'name' | 'post' | 'pro' |
        'tel' | 'travel' | 'xxx'  | 'ac' | 'ad' | 'ae' | 'af' | 'ag' | 'ai' |
        'al' | 'am' | 'an' | 'ao' | 'aq' | 'ar' | 'as' | 'at' | 'au' | 'aw' | 'ax' |
        'az' | 'ba' | 'bb' | 'bd' | 'be' | 'bf' | 'bg' | 'bh' | 'bi' | 'bj' | 'bm' |
        'bn' | 'bo' | 'br' | 'bs' | 'bt' | 'bv' | 'bw' | 'by' | 'bz' | 'ca' | 'cc' |
        'cd' | 'cf' | 'cg' | 'ch' | 'ci' | 'ck' | 'cl' | 'cm' | 'cn' | 'co' | 'cr' |
        'cs' | 'cu' | 'cv' | 'cx' | 'cy' | 'cz' | 'dd' | 'de' | 'dj' | 'dk' | 'dm' |
        'do' | 'dz' | 'ec' | 'ee' | 'eg' | 'eh' | 'er' | 'es' | 'et' | 'eu' | 'fi' |
        'fj' | 'fk' | 'fm' | 'fo' | 'fr' | 'ga' | 'gb' | 'gd' | 'ge' | 'gf' | 'gg' |
        'gh' | 'gi' | 'gl' | 'gm' | 'gn' | 'gp' | 'gq' | 'gr' | 'gs' | 'gt' | 'gu' |
        'gw' | 'gy' | 'hk' | 'hm' | 'hn' | 'hr' | 'ht' | 'hu' | 'id' | 'ie' | 'il' |
        'im' | 'in' | 'io' | 'iq' | 'ir' | 'is' | 'it' | 'je' | 'jm' | 'jo' | 'jp' |
        'ke' | 'kg' | 'kh' | 'ki' | 'km' | 'kn' | 'kp' | 'kr' | 'kw' | 'ky' | 'kz' |
        'la' | 'lb' | 'lc' | 'li' | 'lk' | 'lr' | 'ls' | 'lt' | 'lu' | 'lv' | 'ly' |
        'ma' | 'mc' | 'md' | 'me' | 'mg' | 'mh' | 'mk' | 'ml' | 'mm' | 'mn' | 'mo' |
        'mp' | 'mq' | 'mr' | 'ms' | 'mt' | 'mu' | 'mv' | 'mw' | 'mx' | 'my' | 'mz' |
        'na' | 'nc' | 'ne' | 'nf' | 'ng' | 'ni' | 'nl' | 'no' | 'np' | 'nr' | 'nu' |
        'nz' | 'om' | 'pa' | 'pe' | 'pf' | 'pg' | 'ph' | 'pk' | 'pl' | 'pm' | 'pn' |
        'pr' | 'ps' | 'pt' | 'pw' | 'py' | 'qa' | 're' | 'ro' | 'rs' | 'ru' | 'rw' |
        'sa' | 'sb' | 'sc' | 'sd' | 'se' | 'sg' | 'sh' | 'si' | 'sj' | 'Ja' | 'sk' |
        'sl' | 'sm' | 'sn' | 'so' | 'sr' | 'ss' | 'st' | 'su' | 'sv' | 'sx' | 'sy' |
        'sz' | 'tc' | 'td' | 'tf' | 'tg' | 'th' | 'tj' | 'tk' | 'tl' | 'tm' | 'tn' |
        'to' | 'tp' | 'tr' | 'tt' | 'tv' | 'tw' | 'tz' | 'ua' | 'ug' | 'uk' | 'us' |
        'uy' | 'uz' | 'va' | 'vc' | 've' | 'vg' | 'vi' | 'vn' | 'vu' | 'wf' | 'ws' |
        'ye' | 'yt' | 'yu' | 'za' | 'zm' | 'zw'
    )

};

# silence Deep recursion ... warning
my $DOWARN = 0; BEGIN { $SIG{'__WARN__'} = sub { warn $_[0] if $DOWARN } }

my $url = 'https://mail.google.aero';
my $regex = MarpaX::Regex->new($gruber_url_BNFish);
like $url, qr/$regex/ix, "Gruber's URL pattern (BNFish)";

$url =~ qr/$regex/ix;
is $&, $url, "Gruber's URL pattern (BNFish) capture";

unlike 'foo.na@example.com', qr/$regex/ix, "foo.na in foo.na\@example.com isn't matched";

done_testing();
