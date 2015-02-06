package MarpaX::Regex;

use 5.010;
use strict;
use warnings;

use Marpa::R2;
use Data::Dumper;
$Data::Dumper::Indent = 0;
$Data::Dumper::Terse = 1;

my $dsl = q{;
:default ::= action => [ name, values ]
lexeme default = action => [ name, values ] latm => 1

    statements ::= statement+

    # bottom to top: char, literal, charclass, symbol, grouping/alternation, statement
    metacharacter       ~ '^' | '$' | '.' | [\\\\]
    <character escape>  ~ '\d' | '\w'

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

    literal ::= ( ["] ) <string without double quotes and metacharacters> ( ["] )
    literal ::= ( ['] ) <string without single quotes and metacharacters> ( ['] )
    <string without double quotes and metacharacters> ~ [^\^\$"]+ #"
    <string without single quotes and metacharacters> ~ [^\^\$']+ #'

    <character class> ::= '[' <character class characters> ']'
    <character class characters> ~ <character class character>+
    <character class character> ~ [^\]] | '\[' | '\]' | '[:' | ':]'

    # these will be used in named captures so
    # the rules for regexp NAMEs must be obeyed
    symbol ::= <symbol name>
    <symbol name> ::= <bare name>
    <symbol name> ::= <bracketed name>
    <bare name> ~ [\w]+
    <bracketed name> ~ '<' <bracketed name string> '>'
    <bracketed name string> ~ [\s\.\w]+

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
    my $class = shift;

    my $self = {};

    my $slg = Marpa::R2::Scanless::G->new( { source => \$dsl } );
    $self->{slg} = $slg;

    bless $self, $class;
}

sub parse{
    my ($self, $source) = @_;

    return ${ $self->{slg}->parse( \$source ) };
}

sub translate{
    my ($self, $ast) = @_;
    state $depth++;
    my $s;
    my $indent = "  " x ($depth - 1);
    if (ref $ast){
        my ($node_id, @children) = @$ast;
        if ($node_id eq 'statement'){
#            warn Dumper $node_id, \@children;
            my $lhs = $children[0]->[1]->[1]->[1]->[1];
#            warn "lhs: ", Dumper $lhs;
#            warn "# $lhs alternatives:\n", Dumper $children[0]->[2]->[1];
            $s .= "(?#$lhs)" . "(?:" . join('', map { $self->translate( $_ ) } $children[0]->[2] ) . ")";
        }
        else{
            $s .= join '', map { $self->translate( $_ ) } @children;
        }
    }
    else{
        $s .= $ast;
    }
    $depth--;
    return $s;
}

sub parse_debug{
    my ($self, $source) = @_;

    my $slr = Marpa::R2::Scanless::R->new( {
        grammar => $self->{slg},
        trace_terminals => 1,
    } );
    eval { $slr->read(\$source) } || warn "$@\nProgress report is:\n" . $slr->show_progress;
    return $slr;
}

sub walk_ast {
    my ( $ast, $callback ) = @_;

#    warn "walk_ast: ", Dumper $ast;

    if (ref $ast){
        my ($start, $length, $id, @nodes) = @$ast;
        $callback->($id, @nodes);
        return map { walk_ast($_, $callback) } @nodes;
    }
    else{
        $callback->($ast);
    }
}

1;
