package MarpaX::Regex;

use 5.010;
use strict;
use warnings;

use Marpa::R2;

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Terse = 1;
$Data::Dumper::Deepcopy = 1;

use MarpaX::Regex::AST;

my $dsl = q{;
:default ::= action => [ name, values ]
lexeme default = action => [ name, values ] latm => 1

    statements ::= statement+

    # bottom to top: char, literal, charclass, symbol, grouping/alternation, statement
    metacharacter       ~ '^' | '$' | '.' | [\\\\]
    # todo: add other character escapes
    <character escape>  ~
    # (non)digits, alphanumerics, whitespaces,
    '\d' | '\D' | '\w' | '\W' | '\s' | '\S' |
    # recursions
    '\1' |
    # zero-width assertions
    '\b' | '\B' | '\A' | '\z' | '\Z'

    # spaces are allowed between ? and +, hence G1 rule
    quantifier ::= '?' | '*' | '+'
                 | '?' <quantifier modifier>
                 | '*' <quantifier modifier>
                 | '+' <quantifier modifier>
                 | '{' uint '}'
                 | '{' uint comma '}'
                 | '{' uint comma uint '}'
    <quantifier modifier> ::= '?' | '+'

    uint    ~ [\d]+
    comma   ~ ','

    literal ::= ( ["] ) <string without double quotes> ( ["] )
    literal ::= ( ['] ) <string without single quotes> ( ['] )
    <string without double quotes> ~ [^"]+ #"
    <string without single quotes> ~ [^']+ #'

    <character class> ::= '[' <character class characters> ']'
    <character class characters> ~ <character class character>+
    <character class character> ~ [^\]] | '\[' | '\]' # | '[:' | ':]'

    # these will be used in named captures so
    # the rules for regexp NAMEs must be obeyed
    symbol ::= <symbol name>
    <symbol name> ::= <bare name>
    <symbol name> ::= <bracketed name>
    <bare name> ~ [\w]+
    <bracketed name> ~ '<' <bracketed name string> '>'
    <bracketed name string> ~ <bracketed name character>+
    <bracketed name character> ~ [^>] | '\>' | '\<'

    # grouping and alternation
    primary ::= literal
              | <character class>
              | <character class> quantifier
              | symbol
              | symbol quantifier
              | <character escape>
              | <character escape> quantifier
              | metacharacter
# uncomment the below 2 lines to allow empty groups (null regex)
              | alternation
    alternation         ~ '|'

    group ::= primary
            | '(?:' group ')' quantifier assoc => group
            | '(?:' group ')' assoc => group
            | '(' group ')' quantifier assoc => group
            | '(' group ')' assoc => group
           || group group       # and
# comment out the below line to allow empty groups (null regex)
#           || group '|' group   # or

    # rules
    statement           ::= <empty rule> | <alternative rule>
    <empty rule>        ::= lhs (<op declare bnf>)
    <alternative rule>  ::= lhs (<op declare bnf>) alternatives
    lhs                 ::= <symbol name>
    alternatives        ::= group
    <op declare bnf> ~ '::='

:discard ~ whitespace
    whitespace ~ [\s]+

:discard ~ <hash comment>
    <hash comment> ~ <terminated hash comment> | <unterminated final hash comment>
    <terminated hash comment> ~ '#' <hash comment body> <vertical space char>
    <unterminated final hash comment> ~ '#' <hash comment body>
    <hash comment body> ~ <hash comment char>*
    <vertical space char> ~ [\x{A}\x{B}\x{C}\x{D}\x{2028}\x{2029}]
    <hash comment char> ~ [^\x{A}\x{B}\x{C}\x{D}\x{2028}\x{2029}]

};

sub new {
    my ($class, $source) = @_;

    my $self = {};
    my $slg = Marpa::R2::Scanless::G->new( { source => \$dsl } );
    $self->{slg} = $slg;

    bless $self, $class;

    if ( defined $source ){
        my $ast = eval { $self->parse($source) };
        if ($@){
            warn "# Parse failure:\n" . $self->parse_debug($source);
            return;
        }
        return $self->translate( $ast );
    }

    return $self;
}

sub parse{
    my ($self, $source) = @_;

    return ${ $self->{slg}->parse( \$source ) };
}

=head2 translate pseudocode

    sanity check
        merge statements with the same lhs, like
            lhs ::= rhs1
            lhs ::= rhs2
        to a group under the lhs
            lhs ::= rhs1 '|' rhs2
        by joing the groups with [ 'group', [ 'primary', [ 'alternation', '|' ] ] ]

    until there is no symbols to replace
        find terminals (rules without symbols)
        replace all occurrences terminal symbols in non-terminals with the contents of terminals
    if there are symbols, but there is no terminals to replace them, warn
    concatenate ast

    terminal rules      -- RHS has no symbols
    non-terminals rules -- RHS has at least one symbol
        -- enforce those rules in the grammar?

=cut

sub translate{
    my ($self, $ast) = @_;
    $ast = MarpaX::Regex::AST->new($ast);
    return $ast->distill->substitute->recurse->concat();
}

sub parse_debug{
    my ($self, $source) = @_;

    my $slr = Marpa::R2::Scanless::R->new( {
        grammar => $self->{slg},
        trace_terminals => 1,
    } );
    eval { $slr->read(\$source) } || warn "$@\nProgress report is:\n" . $slr->show_progress;

    if ( $slr->ambiguity_metric() >= 2 ){
        return "BNF parse is ambiguous:\n" . $slr->ambiguous();
        # count or list parses here?
    }
    else{
        my $value_ref = $slr->value();
        return "Parse failed, but there is this value:\n" . Dumper ${ $value_ref } if defined $value_ref;
    }
}

1;
