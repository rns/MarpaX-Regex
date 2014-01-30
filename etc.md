Buzzwords
---------

- Practical general BNF parsing meets Perl regular expressions.
- Regexp::Grammars and Parse::RecDescent without the line noise
- Regexp::Grammars done with BNF
- BNF symbols and rules from expressions of literals and character classes
- structure throught -- in input, grammar, and output
    - find structure in input
    - describe structure in BNF
    - get structured results from output

Synopsis
--------
    
    use 5.010;
    
    my $input; # absolutely need one :)
    
    my $match = MarpaX::Regex->new($match_grammar);
    $input =~ m/$match/;

    my ( $match, $subst ) = MarpaX::Regex->new($match_grammar, $subst_grammar);
    $input =~ s/$match/$substitutition/x;
        
    my ( $match, $subst ) = MarpaX::Regex->new(<<MATCH_GRAMMAR, <<SUBST_GRAMMAR);
    ...
    MATCH_GRAMMAR
    ...
    SUBST_GRAMMAR

    $input =~ s/$match/$subst/x;
    
Implementation Details
----------------------

- MarpaX-Regex-Compiler
- 5.010 regular expressions
- return string, not regex
- require extended syntax

- catch undefined symbols -- warning: symbol <symbol> undefined, assuming ''

- grammar source needs to be a proper string, not mere implementation of string interface (no tied vars or handles)

- symbols names /^[_A-Za-z][_A-Za-z0-9]*\z/ or 'use utf8;', no locale extensions, per perlre:
        Currently NAME is restricted to simple identifiers only.  In other words, it must match "/^[_A-Za-z][_A-Za-z0-9]*\z/" or its Unicode extension (see utf8), though it isn't extended by the locale (see perllocale).
        -- perlre

- capturing groups
    - named (Perl) or numbered is named aren't supported (e.g. JS)

?- m//, s///, =~ operator overloading and %/ a-la Regexp::Grammars
?- AST -- hierarchy of results via %/ hash or otherwise?
    - %+, %-, @+, @- store spans and matches of captre groups

Syntax
------
    
    SLIF
        - L0
        - precedence
        - adverb lists, except sequence rules (separator and proper)
        + double quited strings
        + unused symbols warning
        + undefined symbols warning
        TODO: extend character classes
        
    * BNF: a rewriting system with an alphabet of terminals, and rules which are composed of symbols, the first rule is the start rule
        
    * symbols -- barewords
        
        lhs ::= rhs1 rhs2 
        
    + | alternation 

lexing

    + literals, charclasses, line start, line end (left wall/right wall?) SOL EOL

    + BNF to Regex translation patterns
        
        * test: text with balanced quotes -- angle_brackets.t
        
        - the rules' values are named capture groups + quantifier
            - named capture group -- (?<name>...) or (?'name'...). 
            null rule and recursion -> ()? * + quantifiers
            sequences are recursion
            literals will be quoted (quotemeta), so don't quote metachars in the source 
            
            BNF
                - symbols
                - rules
                - recursion (LHS occurrence in the same rule's RHS)
                - terminal/non-terminal (LHS occurrence in another rule's RHS

            Regex

                - named capture groups

                        (?<name>...) or (?'name'...)

                - quantifiers (also non-backtracking (+) and non-greedy (?))

                        + ? * {n} {n,m}

                - backreferences

                        \1        [5]  Backreference to a specific capture group or buffer.
                                      '1' may actually be any positive integer.
                        \g1       [5]  Backreference to a specific or previous group,
                        \g{-1}    [5]  The number may be negative indicating a relative
                                      previous group and may optionally be wrapped in
                                      curly brackets for safer parsing.
                        \g{name}  [5]  Named backreference
                        \k<name>  [5]  Named backreference
                
                - charclasses are passed through as is
                
                - comments "(?#text)"
                
          rule to regex
          
            [ lhs, [ [ s11 s12 ], [ s21 s22 ], [ s31 s32 s33 ] ] ]
            
            BNF -> Regex -- named capture groups + quantifiers and backreferences

            rules -> named capture groups 
            recursion -> quantifiers
            occurrence of LHS symbol on RHS of another rule -> backreference
            
            text via sequitur
            
            lhs ::= s11 s12 | s21 s22 | s31 s32 s33 | 
            
            lhs ::= s11 s12
            lhs ::= s21 s22
            lhs ::= s31 s32 s33 
            lhs ::= 
            
            (?<lhs> (s11 s12)|(s21 s22)|(s31 s32 s33) )?
            
            seq ::= item
            seq ::= seq item            
            seq ::= 

            (?<seq> (?<item>))
            (?<seq> (?<item>)+)
            (?<seq> (?<item>)*)

        - special syntax for sequences from Marpa
        
            seq ::= item* separator => [\n]
            seq ::= item | separator item

            (?<seq> (?<item> | ()))
            
        - recursive rules are backreferences -- \g{name} 

        + negation (-) is negative lookahead assertion
            - 
        + parens are hiding

        ? test that %+ hash is filled correctly
        ? extended syntax

    + literals    
            
            'la la " fa fa' "he he ' ha ha" -- no escaping with \
            
    + ^ $, SOL, newline, line-start, line-end 
    
    + character classes with modifiers
        
        - escapes and their names
                        
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
             \K        [6]  Keep the stuff left of the \K, don't include it in $&
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

        - negation: lookahead negative - and - (), 
        
            s1 - s2 
            s1 - ( s2 s3 s4 ... )

    + quantifiers -- greedy and not
        
        - ?, *, +, {n,m}
        - +?, *?, ??, {n,m}?

    + replacement
        
        - also as BNF?
        
    - use cases
    
        - showcase -- to be refined -- https://gist.github.com/rns/8625302
        - balanced text
            quote instring quote -- sl_json.t
            '[' instring ']'
        - Famous regexes
            Randal Schwartz JSON regex
            XML regex
            ...
            other examples with named rules, captures and assertions
        
    - converter -- give a regex, have it presented in a nice syntax and start working with it
        - MarpaX::Languages::Regexp::AST
    - grammar inference with Sequitur
        rules' and symbols' names
            R_name_me1...
            LHS_name_me2...
            S_name_me2...
                same names in same contexts to facilitate find/replace
        - hosted service
            - input your text and/or regex, get a skeleton grammar
    
    - Pattern lib
        - named patterns
        - named pattern closures
        - convert Regexp::Common
            
            * strings with balanced parenthesized delimiters.
            * comments of various languages (43 languages currently).
            * delimited strings.
            * palindromes.
            * lists.
            * IPv4 addresses and MAC addresses.
            * numbers (integers and reals).
            * profanity.
            * leading and trailing whitespace.
            * zip codes.        
            
        - superset Regexp::Common            
        
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

        
    - metag.bnf -- grammar to parse grammars

    http://blogs.perl.org/users/jeffrey_kegler/2013/05/the-design-of-four.html
    
        - possibilities
            ?+ (??{ code }) -- road to inline actions
            ?+ support regexes?
                - but how to know if its a regex
            ?+ positive lookahead assertions + and + () 
                - ambiguous syntax
                - s1 +( s2 s3 s4 ... ) -- lookahead positive
                - s1 +s2 -- lookahead positive
                - s1 +( s2 s3 s4 ... ) -- lookahead positive
            ?+ (modifier: lhs ::= @rhs) for symbols rules
            ?+ actions?
            ?+ structural regular expressions?
                    split on terminals -- structural REs
                split (
                    $regex, $input, sub { 
                        split $regex, $input 
                    } 
                )

* quotes
    
    - "simple, well-defined, and self-contained"   
    -- http://blogs.perl.org/users/jeffrey_kegler/2013/05/the-design-of-four.html
    
    - ... Regex compiler -- a compiler from some nice BNF-ish format, to Perl regular expressions. 
    -- https://groups.google.com/d/msg/marpa-parser/2TZn5nolyzk/u5rCDIZ0gKoJ

    - So the basic idea of the SLIF is BNF -- a rewriting system with an alphabet of terminals, and rules which are composed of symbols. 

        In a context-free grammar every rule has one LHS symbol, and zero or more RHS symbols.

        Add a special start symbol, and you're ready to rock 'n roll!

        -- Jeffrey Kegler, [Marpa IRC Channel](http://irclog.perlgeek.de/marpa/2014-01-15#i_8120641}

    Note that a startup can be the ECMAScript pattern specification - ECMAScript implements a very low subset of perl regexp pattern, but it shows quite well how such a grammar should be structured. -- http://www.ecma-international.org/ecma-262/5.1/#sec-15.10.1

    Then you might like my implementation [1] - the only subtility in this simple grammar is that I use these user-defined character classes. 

        [1] https://github.com/jddurand/MarpaX-Languages-ECMAScript-AST/blob/master/lib/MarpaX/Languages/ECMAScript/AST/Grammar/ECMAScript_262_5/Lexical/RegularExpressionLiteral.pm

        [2] https://github.com/jddurand/MarpaX-Languages-ECMAScript-AST/blob/master/lib/MarpaX/Languages/ECMAScript/AST/Grammar/CharacterClasses.pm

* references
    + origin

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

    - http://www.regular-expressions.info/

    http://stackoverflow.com/questions/1435411/what-is-the-bnf-for-a-regex-in-order-to-write-a-full-or-partial-parser

    http://www.codinghorror.com/blog/2008/06/regular-expressions-now-you-have-two-problems.html

    http://stackoverflow.com/questions/13816439/left-linear-and-right-linear-grammars/13945932#13945932
    
    * txt2regex

        # the RegEx show

        start to match
            on the line beginning
            in any part of the line

        followed by"
            any character
            a specific character
            a literal string
            an allowed characters list
            a forbidden characters list
            a special combination
            a POSIX combination (locale aware)
            a ready RegEx (not implemented)
            anything

        how many times (repetition)"
            one
            zero or one (optional)
            zero or more
            one or more
            exactly N
            up to N
            at least N

        # COMBO
            uppercase letters
            lowercase letters
            numbers
            underscore
            space
            TAB

        # TODO put all posix components?
        letters
        lowercase letters
        uppercase letters
        numbers
        letters and numbers
        hexadecimal numbers
        whitespaces (space and TAB)
        graphic chars (not-whitespace)

        # title (line 2-3)
        or
        open group
        close group

    regex BNF

    <RE>     ::=     <union> | <simple-RE>
    <union>  ::=    <RE> "|" <simple-RE>
    <simple-RE>  ::=     <concatenation> | <basic-RE>
    <concatenation>  ::=    <simple-RE> <basic-RE>
    <basic-RE>   ::=     <star> | <plus> | <elementary-RE>
    <star>   ::=    <elementary-RE> "*"
    <plus>   ::=    <elementary-RE> "+"
    <elementary-RE>  ::=     <group> | <any> | <eos> | <char> | <set>
    <group>  ::=    "(" <RE> ")"
    <any>    ::=    "."
    <eos>    ::=    "$"
    <char>   ::=    any non metacharacter | "\" metacharacter
    <set>    ::=     <positive-set> | <negative-set>
    <positive-set>   ::=    "[" <set-items> "]"
    <negative-set>   ::=    "[^" <set-items> "]"
    <set-items>  ::=    <set-item> | <set-item> <set-items>
    <set-items>  ::=    <range> | <char>
    <range>  ::=    <char> "-" <char>

    http://www.cs.sfu.ca/~cameron/Teaching/384/99-3/regexp-plg.html

    expression = term
                 term | expression
    term = factor
           factor term

    factor = atom
             atom metacharacter

    atom = character
           .
           ( expression )
           [ characterclass ]
           [ ^ characterclass ]
           { min }
           { min ,  }
           { min , max }
    characterclass = characterrange
                     characterrange characterclass

    characterrange = begincharacter
                     begincharacter - endcharacter

    begincharacter = character
    endcharacter = character

    character =
                anycharacterexceptmetacharacters

                \ anycharacterexceptspecialcharacters

    metacharacter = ?
                    * {=0 or more, greedy}
                    *? {=0 or more, non-greedy}
                    + {=1 or more, greedy}
                    +? {=1 or more, non-greedy}
                    ^ {=begin of line character}
                    $ {=end of line character}
                    $` {=the characters to the left of the match}
                    $' {=the characters to the right of the match}
                    $& {=the characters that are matched}
                    \t {=tab character}
                    \n {=newline character}
                    \r {=carriage return character}
                    \f {=form feed character}
                    \cX {=control character CTRL-X}
                    \N {=the characters in Nth tag (if on match side)}
                    $N{=the characters in Nth tag (if not on match side)}
                    \NNN {=octal code for character NNN}
                    \b {=match a 'word' boundary}
                    \B {=match not a 'word' boundary}
                    \d {=a digit, [0-9]}
                    \D {=not a digit, [^0-9]}
                    \s {=whitespace, [ \t\n\r\f]}
                    \S {=not a whitespace, [^ \t\n\r\f]}
                    \w {='word' character, [a-zA-Z0-9_]}
                    \W {=not a 'word' character, [^a-zA-Z0-9_]}
                    \Q {=put a quote (de-meta) on characters, until \E}
                    \U {=change characters to uppercase, until \E}
                    \L {=change characters to uppercase, until \E}

    min = integer
    max = integer
    integer = digit
              digit integer

    anycharacter = ! " # $ % & ' ( ) * + , - . / :
                   ; < = > ? @ [ \ ] ^ _ ` { | } ~
                   0 1 2 3 4 5 6 7 8 9
                   A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
                   a b c d e f g h i j k l m n o p q r s t u v w x y z

    http://web.archive.org/web/20090129224504/http://faqts.com/knowledge_base/view.phtml/aid/25718/fid/200               
