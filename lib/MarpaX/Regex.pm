package MarpaX::Regex;

use 5.010;
use strict;
use warnings;

use Marpa::R2;
use Data::Dumper;
$Data::Dumper::Indent = 0;
$Data::Dumper::Terse = 1;

sub new {
    my $class = shift;
    my $source = shift;

    my $bnfg = Marpa::R2::Scanless::G->new( { source => \do { local $/; <DATA> } } );
    my $bnfr = Marpa::R2::Scanless::R->new( { grammar => $bnfg } );
    $bnfr->read(\$source);

    warn $bnfg->show_symbols(0, 'G1');

    my $ast = ${ $bnfr->value() };
    
    my $compiler = {};
    $compiler->{bnfg} = $bnfg;
    $compiler->{bnfr} = $bnfr;
    $compiler->{ast}  = $ast;
    bless $compiler, $class;
}

sub compile{
    my $compiler = shift;
    my $ast = $compiler->{ast};
}

sub fmt{
    my $compiler = shift;
    my $ast = $compiler->{ast};

    my $fmt = '';
    walk_ast( $ast, sub {
        my ($id, @nodes) = @_;
        if (@nodes){
            $id = $compiler->{bnfg}->symbol_display_form($id);
            warn "fmt: $id: ", Dumper \@nodes;
        }
        else{ # $id is literal
            warn "fmt: literal: '$id'";
        }
    } );
    return $fmt;
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

__DATA__
# This is a modification of metag.bnf file from Jeffrey Kegler's 
# Marpa::R2 -- https://github.com/jeffreykegler/Marpa--R2
# Here is the copyright notice from that file:

# Copyright 2014 Jeffrey Kegler
# This file is part of Marpa::R2.  Marpa::R2 is free software: you can
# redistribute it and/or modify it under the terms of the GNU Lesser
# General Public License as published by the Free Software Foundation,
# either version 3 of the License, or (at your option) any later version.
#
# Marpa::R2 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser
# General Public License along with Marpa::R2.  If not, see
# http://www.gnu.org/licenses/.

:default ::= action => [start,length,lhs,values]
lexeme default = action => [start,length,lhs,value]

statements ::= statement+
statement ::= <empty rule> | <alternative rule> | <quantified rule>

<alternative rule> ::= lhs <op declare bnf> alternatives
<empty rule> ::= lhs <op declare bnf>
<quantified rule> ::= lhs <op declare bnf> <single symbol> quantifier <adverb list>
<quantified rule> ::= lhs <op declare bnf> <single symbol> quantifier

alternatives ::= alternative+ separator => <op alternate bnf> proper => 1
alternative ::= rhs

<adverb list> ::= <adverb list items>
<adverb list items> ::= <adverb item>*
<adverb item> ::= <separator specification> | <proper specification>

<separator specification> ::= ('separator' '=>') <single symbol>
<proper specification> ::= ('proper' '=>') boolean

lhs ::= <symbol name>
rhs ::= <rhs primary>+

<rhs primary> ::= <single symbol>
<rhs primary> ::= <single quoted string> | <double quoted string>
<rhs primary> ::= <character class>
<rhs primary> ::= <parenthesized rhs primary list>
<parenthesized rhs primary list> ::= ('(') <rhs primary list> (')')
<rhs primary list> ::= <rhs primary>+

<single symbol> ::= symbol | <character class>
symbol ::= <symbol name>
<symbol name> ::= <bare name>
<symbol name> ::= <bracketed name>

:discard ~ whitespace
whitespace ~ [\s]+

# allow comments
:discard ~ <hash comment>
<hash comment> ~ <terminated hash comment> | <unterminated final hash comment>
<terminated hash comment> ~ '#' <hash comment body> <vertical space char>
<unterminated final hash comment> ~ '#' <hash comment body>
<hash comment body> ~ <hash comment char>*
<vertical space char> ~ [\x{A}\x{B}\x{C}\x{D}\x{2028}\x{2029}]
<hash comment char> ~ [^\x{A}\x{B}\x{C}\x{D}\x{2028}\x{2029}]

<op declare bnf> ~ '::='
<op alternate bnf> ~ '|'
quantifier ::= 
        '*'  | '+'  | '?' 
    |   '*?' | '+?' | '??' 
    |   '{' <unsigned integer> '}' 
    |   '{' <unsigned integer> comma '}' 
    |   '{' <unsigned integer> comma <unsigned integer> '}'
    |   '{' <unsigned integer> '}?' 
    |   '{' <unsigned integer> comma '}?' 
    |   '{' <unsigned integer> comma <unsigned integer> '}?'
    
boolean ~ [01]
<unsigned integer> ~ [\d]+
comma ~ ','

<bare name> ~ [\w]+
<bracketed name> ~ '<' <bracketed name string> '>'
<bracketed name string> ~ [\s\w]+

# In single/double quotes strings and character classes
# no escaping or internal newlines, and disallow empty string
<single quoted string> ~ ['] <string without single quote or vertical space> [']
<string without single quote or vertical space> ~ [^''\x{0A}\x{0B}\x{0C}\x{0D}\x{0085}\x{2028}\x{2029}]+

<double quoted string> ~ ["] <string without double quote or vertical space> ["]
<string without double quote or vertical space> ~ [^""\x{0A}\x{0B}\x{0C}\x{0D}\x{0085}\x{2028}\x{2029}]+

# now we just pass through everything that is between '[' ']'
# ?todo: change <character class characters> to <cc elements> 
# with Perl regex charclass elements per perlre
# as in metag.bnf
<character class> ::= '[' <character class characters> ']'
:lexeme ~ <character class characters> forgiving => 1
<character class characters> ~ <character class character>+
<character class character> ~ [^\]] | '\[' | '\]' | '[:' | ':]'
