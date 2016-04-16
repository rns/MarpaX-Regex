package MarpaX::Regex;

use 5.010;
use strict;
use warnings;

use Marpa::R2;

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Terse = 1;
$Data::Dumper::Deepcopy = 1;

use Carp;

use MarpaX::Regex::AST;

my $dsl = q{;
:default ::= action => [ name, values ]
lexeme default = action => [ name, values ] latm => 1

    statements ::= statement+

    # bottom to top: char, literal, charclass, symbol, grouping/alternation, statement
    metacharacter       ~ '^' | '$' | '.' | [\\\\]
    # todo: unicode -- \p{}
    # todo: add other character escapes
    # todo: split into classes, e.g assertions and shortcuts
    <character escape>  ~
    # (non)digits, alphanumerics, whitespaces,
    '\d' | '\D' | '\w' | '\W' | '\s' | '\S' |
    # recursions -- todo: include in regex grammar for pretty-printing
#    '\1' |
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
              | <character class> quantifier name => 'quantified character class'
              | symbol
              | symbol quantifier name => 'quantified symbol'
              | <character escape>
              | <character escape> quantifier name => 'quantified character escape'
              | metacharacter
# uncomment the below 2 lines to allow empty groups (null regex)
              | alternation
    alternation         ~ '|'

    group ::= primary
            | '(?:' group ')' quantifier assoc => group name => 'quantified non-capturing group'
            | '(?:' group ')' assoc => group  name => 'non-capturing group'
            | '(' group ')' quantifier assoc => group name => 'quantified capturing group'
            | '(' group ')' assoc => group name => 'capturing group'
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

use constant TARGET_PERL_REGEX => 'perl_regex';
use constant TARGET_MARPA_NAIF    => 'marpa_naif';

sub new {
    my ($class, $source, $target) = @_;

    $target //= TARGET_PERL_REGEX;

    my $self = {};
    my $slg = Marpa::R2::Scanless::G->new( { source => \$dsl } );
    $self->{slg} = $slg;

    bless $self, $class;

    if ( defined $source ){
        my $ast = eval { $self->parse_BNFish($source) };
        if ($@){
            warn "# Parse failure:\n" . $self->parse_debug($source);
            return;
        }
        return $self->compile( $ast, $target );
    }

    return $self;
}

# parses BNFish regex source
sub parse_BNFish{
    my ($self, $source) = @_;

    return ${ $self->{slg}->parse( \$source ) };
}

sub compile{
    my ($self, $ast, $target) = @_;

    $target //= TARGET_PERL_REGEX;

    if ($target eq TARGET_PERL_REGEX){
        $ast = MarpaX::Regex::AST->new($ast);
        return $ast->distill->substitute->recurse->concat();
    }
    elsif ($target eq TARGET_MARPA_NAIF){
        $ast = MarpaX::Regex::AST->new($ast);
        return $ast->naif_grammar();
    }
    else{
        croak "Compile target must be " . TARGET_MARPA_NAIF . " or " . TARGET_PERL_REGEX .
            " not $target";
    }
}

sub parse{
    my ($self, $naif_grammar, $input) = @_;
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
