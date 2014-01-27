MarpaX-Regex-Verbal
===================

- MarpaX::Regex
    
    - simple, well-defined, and self-contained
    - Practical general BNF parsing meets Perl regular expressions.
    - BNF at heart, extended with regex constructs, which fit BNF syntax
    - Regexp::Grammars and Parse::RecDescent without the line noise
    - Regexp::Grammars done with BNF
        
Synopsis
--------

    my $r = MarpaX::Regex->new($find);
    my $r = MarpaX::Regex->new($find, $replace);

    my $result = $input =~ $r;
    my @result = $input =~ $r;
    my %result = $input =~ $r;

    $input =~ m/$r/;
    $input =~ s/$r/replacement/;    

Highlights
----------
    
    * BNF: a rewriting system with an alphabet of terminals, and rules which are composed of symbols, the first rule is the start rule
        
    * symbols -- barewords
        
        lhs ::= rhs1 rhs2 
        
    + | alternation 
    
= here we can start matching -- symbols are pure syntax
    
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

    + ( ... ) groups, names captures, negation
        
        - groups 

            (s1 s2 s3)?                 maybe(s1 s2 s3)
            (s1 s2 s3)*                 anything(s1 s2 s3)
            (s1 s2 s3)+                 something(s1 s2 s3)

        - named captures
            
            (name: s1 s2 s3) -- capture to name
            
        - negation: lookahead negative - and - (), 
        
            s1 - s2 
            s1 - ( s2 s3 s4 ... )

    + quantifiers -- greedy and not
        
        - ?, *, +, {n,m}
        - +?, *?, ??, {n,m}?

    + backreferences

        - in match pattern -- recursion
        - $name - named capture in replacement pattern
        no \1 ancd $1 -- line noise

    + replacement
        
    + operator overloading

        
        $input ~= m//;
        $input ~= s///;

        combinators
            r1 + r2 -- r1 and r2
            r1 - r2 -- r1 and not r2
            r1 | r2 -- r1 or r2
    
    - utf8 as the internal encoding

        - all input must be converted from the source encoding to utf8
        - all output must be converted from utf8 to source encoding
    
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
        
    - metag.bnf -- grammar to parse grammars

    http://blogs.perl.org/users/jeffrey_kegler/2013/05/the-design-of-four.html
    
        - possibilities
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
                split (
                    $regex, $input, sub { 
                        split $regex, $input 
                    } 
                )

* quotes
    
    perfect is "simple, well-defined, and self-contained" -- http://blogs.perl.org/users/jeffrey_kegler/2013/05/the-design-of-four.html
    One is a Regex compiler -- a compiler from some nice BNF-ish format, to Perl regular expressions. -- https://groups.google.com/d/msg/marpa-parser/2TZn5nolyzk/u5rCDIZ0gKoJ

    - So the basic idea of the SLIF is BNF -- a rewriting system with an alphabet of terminals, and rules which are composed of symbols. 

        In a context-free grammar every rule has one LHS symbol, and zero or more RHS symbols.

        Add a special start symbol, and you're ready to rock 'n roll!

        -- Jeffrey Kegler, [Marpa IRC Channel](http://irclog.perlgeek.de/marpa/2014-01-15#i_8120641}

    Note that a startup can be the ECMAScript pattern specification - ECMAScript implements a very low subset of perl regexp pattern, but it shows quite well how such a grammar should be structured. -- http://www.ecma-international.org/ecma-262/5.1/#sec-15.10.1

    Then you might like my implementation [1] - the only subtility in this simple grammar is that I use these user-defined character classes. 

        [1] https://github.com/jddurand/MarpaX-Languages-ECMAScript-AST/blob/master/lib/MarpaX/Languages/ECMAScript/AST/Grammar/ECMAScript_262_5/Lexical/RegularExpressionLiteral.pm

        [2] https://github.com/jddurand/MarpaX-Languages-ECMAScript-AST/blob/master/lib/MarpaX/Languages/ECMAScript/AST/Grammar/CharacterClasses.pm

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
