package MarpaX::Regex;

use 5.010;
use strict;
use warnings;

use Marpa::R2;

# try jdd's optimization
#=pod
BEGIN {
  #
  # Marpa internal optimisation: we do not want the closures to be rechecked every time
  # we call $r->value(). This is a static information, although determined at run-time
  # the first time $r->value() is called on a recognizer.
  #
  no warnings 'redefine';

  sub Marpa::R2::Recognizer::registrations {
    my $recce = shift;
    if (@_) {
      my $hash = shift;
      if (! defined($hash) ||
          ref($hash) ne 'HASH' ||
          grep {! exists($hash->{$_})} qw/
                                           NULL_VALUES
                                           REGISTRATIONS
                                           CLOSURE_BY_SYMBOL_ID
                                           CLOSURE_BY_RULE_ID
                                           RESOLVE_PACKAGE
                                           RESOLVE_PACKAGE_SOURCE
                                           PER_PARSE_CONSTRUCTOR
                                         /) {
        Marpa::R2::exception(
                             "Attempt to reuse registrations failed:\n",
                             "  Registration data is not a hash containing all necessary keys:\n",
                             "  Got : " . ((ref($hash) eq 'HASH') ? join(', ', sort keys %{$hash}) : '') . "\n",
                             "  Want: CLOSURE_BY_RULE_ID, CLOSURE_BY_SYMBOL_ID, NULL_VALUES, PER_PARSE_CONSTRUCTOR, REGISTRATIONS, RESOLVE_PACKAGE, RESOLVE_PACKAGE_SOURCE\n"
                            );
      }
      $recce->[Marpa::R2::Internal::Recognizer::NULL_VALUES] = $hash->{NULL_VALUES};
      $recce->[Marpa::R2::Internal::Recognizer::REGISTRATIONS] = $hash->{REGISTRATIONS};
      $recce->[Marpa::R2::Internal::Recognizer::CLOSURE_BY_SYMBOL_ID] = $hash->{CLOSURE_BY_SYMBOL_ID};
      $recce->[Marpa::R2::Internal::Recognizer::CLOSURE_BY_RULE_ID] = $hash->{CLOSURE_BY_RULE_ID};
      $recce->[Marpa::R2::Internal::Recognizer::RESOLVE_PACKAGE] = $hash->{RESOLVE_PACKAGE};
      $recce->[Marpa::R2::Internal::Recognizer::RESOLVE_PACKAGE_SOURCE] = $hash->{RESOLVE_PACKAGE_SOURCE};
      $recce->[Marpa::R2::Internal::Recognizer::PER_PARSE_CONSTRUCTOR] = $hash->{PER_PARSE_CONSTRUCTOR};
    }
    return {
            NULL_VALUES => $recce->[Marpa::R2::Internal::Recognizer::NULL_VALUES],
            REGISTRATIONS => $recce->[Marpa::R2::Internal::Recognizer::REGISTRATIONS],
            CLOSURE_BY_SYMBOL_ID => $recce->[Marpa::R2::Internal::Recognizer::CLOSURE_BY_SYMBOL_ID],
            CLOSURE_BY_RULE_ID => $recce->[Marpa::R2::Internal::Recognizer::CLOSURE_BY_RULE_ID],
            RESOLVE_PACKAGE => $recce->[Marpa::R2::Internal::Recognizer::RESOLVE_PACKAGE],
            RESOLVE_PACKAGE_SOURCE => $recce->[Marpa::R2::Internal::Recognizer::RESOLVE_PACKAGE_SOURCE],
            PER_PARSE_CONSTRUCTOR => $recce->[Marpa::R2::Internal::Recognizer::PER_PARSE_CONSTRUCTOR]
           };
  } ## end sub registrations

  sub Marpa::R2::Scanless::R::registrations {
    my $slr = shift;
    my $thick_g1_recce =
      $slr->[Marpa::R2::Internal::Scanless::R::THICK_G1_RECCE];
    return $thick_g1_recce->registrations(@_);
  } ## end sub Marpa::R2::Scanless::R::registrations

};
#=cut

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
        return $self->compile( $ast );
    }

    return $self;
}

sub parse{
    my ($self, $source) = @_;

    return ${ $self->{slg}->parse( \$source ) };
}

sub compile{
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
