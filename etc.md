MarpaX-Regex
============

quicker and easier writing
    
improve build- and read-ability
  define names for metacharacters and character escapes
  take your regex and modularize it
    needs full regex parser
    
compile to Marpa::R2 for long strings and/or hard regexes
  external lexing
  lexemes
    literals, charclasses and symbolless groups
  expand the brackets by rewriting the grammar to EBNF -> BNF
    to prioritized rules to handle associativity and precedence?
    MarpaX::Regex grammar as a template

Syntax
------    

# symbols in RE groups, empty groups, optional <> for symbols with spaces
  s ::= 'house' ( 'cat' |)      
  s ::= 'house' ( 'cat' ( 's' |) |)
  
Priorities
----------

  symbol expansion
  explicit non-capturing parens (?: ...)

=====the=above=are=done=====
  
  recursion

  escaped metacharacters in literals and charclasses
    like <character class character>

  coerce symbol names to REs
    Currently NAME is restricted to simple identifiers only.  In other words, it must match "/^[_A-Za-z][_A-Za-z0-9]*\z/" or its Unicode extension (see utf8), though it is not extended by the locale (see perllocale).

  pretty-printing and commenting

  assertions

  more regex-ish syntax  
    bracketed symbols <>
    bareword literals
    PEG uses quoted literals and bareword symbols

  names for character escapes, metacharacters and other line noise
      <start of line>   ::= ^
      <end of line>     ::= $
      <word character>  ::= \w
      <digit>           ::= \d  
  
  abstract distill() 
    parent/child pairs

= ast

  id()   
  child(index_or_predicate)
    first_child()
    last_child()
  children()
  append_child(MarpaX::Regex::AST)
  remove_child(index_or_predicate)
  replace_child(index_or_predicate, MarpaX::Regex::AST)
   
use cases
  Building regexp as a rewriting system
    BNF 
      msvc_warnings.t
      Marpa::R2 synopsis
    RE  
      json parser by Randal
      angle brackets
      gruber url regexp
      christiansen html parser
      Regexp::Common
      Regexp::Grammars
      Parse::RecDescent
    recursion
    composability
      include + namespaces
  RE Converter -- give a regex, have it presented in a nice syntax and start working with it
      - MarpaX::Languages::Regexp::AST
  Grammar inference with Sequitur
    
RE Features (Support or not)
----------------------------

http://perldoc.perl.org/perlretut.html

Part 1: The basics
  Simple word matching
  Using character classes
  Matching this or that
  Grouping things and hierarchical matching
  Extracting matches
  
  Backreferences
  Relative backreferences
  Named backreferences
  
  Alternative capture group numbering
  Position information
  Non-capturing groupings
  Matching repetitions
  Possessive quantifiers
  Building a regexp
  -----------------
    specifying the task in detail,
    breaking down the problem into smaller parts,
    translating the small parts into regexps,
    combining the regexps,
    and optimizing the final combined regexp.

  Using regular expressions in Perl
  
Part 2: Power tools
  More on characters, strings, and character classes
  Compiling and saving regular expressions
  Composing regular expressions at runtime
  Embedding comments and modifiers in a regular expression
  Looking ahead and looking behind
  Using independent subexpressions to prevent backtracking
  Conditional expressions
  Defining named patterns
  Recursive patterns
  A bit of magic: executing Perl code in a regular expression
  Backtracking control verbs
  Pragmas and debugging

Synopsis
--------
    
    use 5.010;
    use MarpaX::Regex;
    my $re = MarpaX::Regex->new(<<GRAMMAR);
    ...
    GRAMMAR
    my $s = $re->as_string;
    my $input = 'what needs to be matched';
    $input =~ m/$re/x; # extended syntax is required
    # $1 etc. captures work as expected
    $input =~ s/$re/substitution/x;
        
Implementation Details
----------------------

- 5.010 regular expressions
- return regexp as a string
- return regexp compiled with /x

BNF Primer
----------

BNF -- a rewriting system
-- Jeffrey Kegler on 
[Marpa IRC Channel](http://irclog.perlgeek.de/marpa/2014-01-15#i_8120641}

a stone is a stein is a rock is a boulder is a pebble
-- Ernest Hemingway, For Whom the Bell Tolls

RE escapes and their names
--------------------------

    escape      reserved bareword
    ------      -----------------
    \w+         word
    \d+         digits

    \w          word-char
    \W          non-word-char

    \d          digit
    \D          non-digit

     \s        [3]  whitespace
     \S        [3]  non-whitespace
     \d        [3]  decimal digit
     \D        [3]  non-digit
     \pP       [3]  property{P}, property{Prop} for longer names
     \PP       [3]  Match non-P
     \X        [4]  Match Unicode "eXtended grapheme cluster"
     \C             Match a single C-language char (octet) even if that is
                      part of a larger UTF-8 character.  Thus it breaks up
                      characters into their UTF-8 bytes, so you may end up
                      with malformed pieces of UTF-8.  Unsupported in
                      lookbehind.
     \1        [5]  Backreference to a specific capture group or buffer.
                      '1' may actually be any positive integer.
     \g1       [5]  Backreference to a specific or previous group,
     \g{-1}    [5]  The number may be negative indicating a relative
                      previous group and may optionally be wrapped in
                      curly brackets for safer parsing.
     \g{name}  [5]  Named backreference
     \k<name>  [5]  Named backreference
     \K        [6]  Keep the stuff left of the \K, do not include it in $&
     \N        [7]  Any character but \n.  Not affected by /s modifier
     \v        [3]  Vertical whitespace
     \V        [3]  Not vertical whitespace
     \h        [3]  Horizontal whitespace
     \H        [3]  Not horizontal whitespace
     \R        [4]  Linebreak

    \t          tab                   (HT, TAB)
    \n          newline               (LF, NL)
    \r          return                (CR)
    \f          formfeed             (FF)
    \a          alarm (bell)          (BEL)
    \e          escape (think troff)  (ESC)
    \cK         control char          (example: VT)
    \x{}, \x00  character whose ordinal is the given hexadecimal number
    \N{name}    named Unicode character or character sequence
    \N{U+263D}  Unicode character     (example: FIRST QUARTER MOON)
    \o{}, \000  character whose ordinal is the given octal number
    \l          lowercase next char (think vi)
    \u          uppercase next char (think vi)
    \L          lowercase till \E (think vi)
    \U          uppercase till \E (think vi)
    \Q          quote (disable) pattern metacharacters till \E
    \E          end either case modification or quoted section, think vi

    zero-width assertions
    \b  word boundary
    \B  except at a word boundary
    \A  only at beginning of string
    \Z  only at end of string, or before newline at the end
    \z  only at end of string
    \G  only at pos() (e.g. at the end-of-match position
        of prior m//g)

Regexp::Common
--------------
    
    convert 
    
    * strings with balanced parenthesized delimiters.
    * comments of various languages (43 languages currently).
    * delimited strings.
    * palindromes.
      my $pp = qr/^(\W* (?: (\w) (?1) \g{-1} | \w? ) \W*)$/ix;
      for $s ( "saippuakauppias", "A man, a plan, a canal: Panama!" ){
          print "'$s' is a palindrome\n" if $s =~ /$pp/;
      }
    * lists.
    * IPv4 addresses and MAC addresses.
    * numbers (integers and reals).
    * profanity.
    * leading and trailing whitespace.
    * zip codes.        

    superset

    * email addresses 
    * HTML/XML tags
    * more numerical matchers,
    * mail headers (including multiline ones),
    * more URLS
    * telephone numbers of various countries
    * currency (universal 3 letter format, Latin-1, currency names)
    * dates
    * binary formats (e.g. UUencoded, MIMEd)
    -- http://www.regular-expressions.info/examples.html
    -- http://www.regular-expressions.info/examplesprogrammer.html

* quotes
    
    - "simple, well-defined, and self-contained"   
    -- http://blogs.perl.org/users/jeffrey_kegler/2013/05/the-design-of-four.html
    
    - ... Regex compiler -- a compiler from some nice BNF-ish format, to Perl regular expressions. 
    -- https://groups.google.com/d/msg/marpa-parser/2TZn5nolyzk/u5rCDIZ0gKoJ

    - So the basic idea of the SLIF is BNF -- a rewriting system with an alphabet of terminals, and rules which are composed of symbols. 

        Add a special start symbol, and you're ready to rock 'n roll!

        -- Jeffrey Kegler, [Marpa IRC Channel](http://irclog.perlgeek.de/marpa/2014-01-15#i_8120641}

        In a context-free grammar every rule has one LHS symbol, and zero or more RHS symbols.

Syntax Options
--------------

https://github.com/jddurand/MarpaX-Languages-ECMAScript-AST/blob/master/lib/MarpaX/Languages/ECMAScript/AST/Grammar/ECMAScript_262_5/Pattern.pm

# bareword literals, <> required for symbols
  <s> ::= house ( cat | <none> )
  <none> ::= # empty rule

# RE in BNF
  <regex>       ::= <term> '|' <regex> |  <term>
  <term>        ::= { <factor> } # one or more
  <factor>      ::= <base> { '*' }
  <base>        ::= <char> |  '\\' <char> |  '(' <regex> ')'  
# BNF in BNF
  <syntax>      ::= <rule> | <rule> <syntax>
  <rule>        ::= "<" <rule-name> ">" "::=" <expression>
  <expression>  ::= <list> | <list> "|" <expression>
  <list>        ::= <term> | <term> <list>
  <term>        ::= <literal> | "<" <rule-name> ">"
  <literal>     ::= '"' <text> '"' | "'" <text> "'"
  <text>
  <rule-name>
# EBNF in EBNF
  letter = "A" | "B" | "C" | "D" | "E" | "F" | "G"
         | "H" | "I" | "J" | "K" | "L" | "M" | "N"
         | "O" | "P" | "Q" | "R" | "S" | "T" | "U"
         | "V" | "W" | "X" | "Y" | "Z" ;
  digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
  symbol = "[" | "]" | "{" | "}" | "(" | ")" | "<" | ">"
         | "'" | '"' | "=" | "|" | "." | "," | ";" ;
  character = letter | digit | symbol | "_" ;

  identifier = letter , { letter | digit | "_" } ;
  terminal = "'" , character , { character } , "'" 
           | '"' , character , { character } , '"' ;

  lhs = identifier ;
  rhs = identifier
       | terminal
       | "[" , rhs , "]"
       | "{" , rhs , "}"
       | "(" , rhs , ")"
       | rhs , "|" , rhs
       | rhs , "," , rhs ;

  rule = lhs , "=" , rhs , ";" ;
  grammar = { rule } ;

References
----------

    There are a number of projects that would, I think, be quite popular and 
    useful but which I simply don't have the cycles to consider doing 
    myself.  One is a Regex compiler -- a compiler from some nice BNF-ish 
    format, to Perl regular expressions.  I'd think this could be very 
    popular -- it would be very much in the comfort zone of some programmers 
    who otherwise would not consider using Marpa. 

    To be specific, this is another specialized Marpa-to-Perl compiler.  The 
    compiler would write a Perl regex, and the Perl regex would be what 
    actually runs.  The value added by Marpa would be that more complex 
    regexes could be more quickly and easily written, and the output regex 
    could be nicely pretty-printed and commented. 

    Sometimes not understood is that one thing regular expressions *cannot* 
    parse is the representation of a regular expression.  Regular 
    expressions are defined recursively, but they do not themselves deal 
    with recursion. 

    One way to think of this project is as a Marpa super-superset of 
    Regexp::Common, whose functionality could be incorporated.  A related 
    effort within Perl was the DEFINE predicate for sub-patterns, but DEFINE 
    had horrific syntax and AFAIK was little or never used. 

    -- Jeffrey Kegler, https://groups.google.com/d/msg/marpa-parser/2TZn5nolyzk/u5rCDIZ0gKoJ

    It would be awesome to have more targets, for example JavaScript. So 
    one would be able to write a language (protocol) in BNF form and parse 
    it in JS on client side and with perl or other language on server 
    side. It's for sure interesting task and chalenging one.

    -- Ruslan Zakirov, https://groups.google.com/d/msg/marpa-parser/2TZn5nolyzk/kqq5NM2zkIQJ

    + https://metacpan.org/pod/Regexp::Grammars
        - must do what they can do

    + https://metacpan.org/pod/Regexp::Common
        - "Marpa super-superset of Regexp::Common, whose functionality could be incorporated"

    http://www.regular-expressions.info/
    http://stackoverflow.com/questions/1435411/what-is-the-bnf-for-a-regex-in-order-to-write-a-full-or-partial-parser

    http://www.codinghorror.com/blog/2008/06/regular-expressions-now-you-have-two-problems.html

    http://stackoverflow.com/questions/13816439/left-linear-and-right-linear-grammars/13945932#13945932
    http://stackoverflow.com/questions/10510700/how-can-i-transform-this-backus-naur-form-expression-into-a-regex-net?rq=1

Liberal Regex Pattern for URLs
https://gist.github.com/gruber/8891611

# Single-line version:
(?i)\b((?:https?:(?:/{1,3}|[a-z0-9%])|[a-z0-9.\-]+[.](?:com|net|org|edu|gov|mil|aero|asia|biz|cat|coop|info|int|jobs|mobi|museum|name|post|pro|tel|travel|xxx|ac|ad|ae|af|ag|ai|al|am|an|ao|aq|ar|as|at|au|aw|ax|az|ba|bb|bd|be|bf|bg|bh|bi|bj|bm|bn|bo|br|bs|bt|bv|bw|by|bz|ca|cc|cd|cf|cg|ch|ci|ck|cl|cm|cn|co|cr|cs|cu|cv|cx|cy|cz|dd|de|dj|dk|dm|do|dz|ec|ee|eg|eh|er|es|et|eu|fi|fj|fk|fm|fo|fr|ga|gb|gd|ge|gf|gg|gh|gi|gl|gm|gn|gp|gq|gr|gs|gt|gu|gw|gy|hk|hm|hn|hr|ht|hu|id|ie|il|im|in|io|iq|ir|is|it|je|jm|jo|jp|ke|kg|kh|ki|km|kn|kp|kr|kw|ky|kz|la|lb|lc|li|lk|lr|ls|lt|lu|lv|ly|ma|mc|md|me|mg|mh|mk|ml|mm|mn|mo|mp|mq|mr|ms|mt|mu|mv|mw|mx|my|mz|na|nc|ne|nf|ng|ni|nl|no|np|nr|nu|nz|om|pa|pe|pf|pg|ph|pk|pl|pm|pn|pr|ps|pt|pw|py|qa|re|ro|rs|ru|rw|sa|sb|sc|sd|se|sg|sh|si|sj|Ja|sk|sl|sm|sn|so|sr|ss|st|su|sv|sx|sy|sz|tc|td|tf|tg|th|tj|tk|tl|tm|tn|to|tp|tr|tt|tv|tw|tz|ua|ug|uk|us|uy|uz|va|vc|ve|vg|vi|vn|vu|wf|ws|ye|yt|yu|za|zm|zw)/)(?:[^\s()<>{}\[\]]+|\([^\s()]*?\([^\s()]+\)[^\s()]*?\)|\([^\s]+?\))+(?:\([^\s()]*?\([^\s()]+\)[^\s()]*?\)|\([^\s]+?\)|[^\s`!()\[\]{};:'".,<>?«»“”‘’])|(?:(?<!@)[a-z0-9]+(?:[.\-][a-z0-9]+)*[.](?:com|net|org|edu|gov|mil|aero|asia|biz|cat|coop|info|int|jobs|mobi|museum|name|post|pro|tel|travel|xxx|ac|ad|ae|af|ag|ai|al|am|an|ao|aq|ar|as|at|au|aw|ax|az|ba|bb|bd|be|bf|bg|bh|bi|bj|bm|bn|bo|br|bs|bt|bv|bw|by|bz|ca|cc|cd|cf|cg|ch|ci|ck|cl|cm|cn|co|cr|cs|cu|cv|cx|cy|cz|dd|de|dj|dk|dm|do|dz|ec|ee|eg|eh|er|es|et|eu|fi|fj|fk|fm|fo|fr|ga|gb|gd|ge|gf|gg|gh|gi|gl|gm|gn|gp|gq|gr|gs|gt|gu|gw|gy|hk|hm|hn|hr|ht|hu|id|ie|il|im|in|io|iq|ir|is|it|je|jm|jo|jp|ke|kg|kh|ki|km|kn|kp|kr|kw|ky|kz|la|lb|lc|li|lk|lr|ls|lt|lu|lv|ly|ma|mc|md|me|mg|mh|mk|ml|mm|mn|mo|mp|mq|mr|ms|mt|mu|mv|mw|mx|my|mz|na|nc|ne|nf|ng|ni|nl|no|np|nr|nu|nz|om|pa|pe|pf|pg|ph|pk|pl|pm|pn|pr|ps|pt|pw|py|qa|re|ro|rs|ru|rw|sa|sb|sc|sd|se|sg|sh|si|sj|Ja|sk|sl|sm|sn|so|sr|ss|st|su|sv|sx|sy|sz|tc|td|tf|tg|th|tj|tk|tl|tm|tn|to|tp|tr|tt|tv|tw|tz|ua|ug|uk|us|uy|uz|va|vc|ve|vg|vi|vn|vu|wf|ws|ye|yt|yu|za|zm|zw)\b/?(?!@)))


# Commented multi-line version:

(?xi)
\b
(                           # Capture 1: entire matched URL
  (?:
    https?:             # URL protocol and colon
    (?:
      /{1,3}                        # 1-3 slashes
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
    /?
    (?!@)           # not succeeded by a @, avoid matching "foo.na" in "foo.na@example.com"
  )
)

christiansen html parser

http://stackoverflow.com/questions/4231382/regular-expression-pattern-not-matching-anywhere-in-string/4234491#4234491

= regex/bnf/ebnf equivalence
  http://stackoverflow.com/questions/29124221/are-the-folowing-bnf-ebnf-regular-expressions-and-lex-syntax-correct

  regular expression

  [A-E]([A-E0-9_]*[$]?[A-E0-9_]*)
  BNF

  <S>::= <letter><Rest>
  <Rest>::=<rest1><symbol>|<rest1><symbol><rest1>|<symbol><rest1>|<rest1>|<symbol>
  <rest1>::=<character><rest1>|<rest1><character>|<character>
  <character>::=A|B|C|D|E|1|2|3|4|_
  <letter>::=A|B|C|D|E
  <symbol>::=$
  EBNF

  S=letter{character}[symbol]{character}
  letter=|"A"|"B"|"C"|"D"|"E"|
  character=|"1"|"2"|"3"|"4"|"A"|"B"|"C"|"D"|"E"|"_"
  symbol="$"
  lex

  ^[A-E][A-E0-3_]*[$]?[A-E0-3_]*$
