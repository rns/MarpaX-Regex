use 5.010;
use strict;
use warnings;

use Test::More;

=pod translation use case
    2 + 3                         -- infix
    
    (+ 2 3)                       -- prefix
  
    (2 3 +)                       -- postfix
  
    bipush 2                      -- JVM 
    bipush 3 
    iadd 
  
    the sum of 2 and 3            -- English
    -- http://www.cse.chalmers.se/edu/year/2012/course/DAT150/lectures/proglang-02.html
    
=cut

=pod Notes

    A BNF is translatable if every parse tree node can have a unique id. If any rule has more than one rhs alternative, then a unique node id is 'lhs/name' otherwise the unique node id is the lhs of the rule.

=cut

use Marpa::R2;

use Data::Dumper;
$Data::Dumper::Indent = 0;
$Data::Dumper::Terse = 1;

my $tests = [
    #    2 + 3                         -- infix
    [   
        'infix',
        [ '2 + 3', '2 * 3' ],
        q{
            e   ::= int plus int name => 'add' | 
                    int star int name => 'mul'
            int  ~ [\d]   
            plus ~ '+'
            star ~ '*'
        }
    ],
    #    (+ 2 3)                       -- prefix
    [   
        'prefix',
        [ '(+ 2 3)', '(* 2 3)' ],
        q{
            e   ::= '(' plus int int ')' name => 'add' | 
                    '(' star int int ')' name => 'mul'
            int  ~  [\d]   
            plus ~ '+'
            star ~ '*'
        }
    ],
    #    (2 3 +)                       -- postfix
    [   
        'postfix',
        [ '(2 3 +)', '(2 3 *)' ],
        q{
            e   ::= '(' int int plus ')' name => 'add' | 
                    '(' int int star ')' name => 'mul'
            int  ~ [\d]   
            plus ~ '+'
            star ~ '*'
        }
    ],
    #    bipush 2                      -- JVM 
    #    bipush 3 
    #    iadd 
    [   
        'JVM',
        [
            q{
                bipush 2
                bipush 3
                iadd 
            },
            q{
                bipush 2
                bipush 3
                imul 
            },
        ],
        q{
            e    ::= push push add name => 'add' | 
                     push push mul name => 'mul'
            push ::= 'bipush' int
            int  ~ [\d]
            add  ~ 'iadd'
            mul  ~ 'imul'
        }
    ],
    #    the sum of 2 and 3            -- English
    [   
        'English',
        [ 'the sum of 2 and 3', 'the product of 2 and 3' ],
        q{
            e ::= 'the' op 'of' int 'and' int
            op ~ 'sum' name => 'add' | 'product' name => 'mul'
            int  ~ [\d]
        }
    ],
];


my $grammar_prolog = q{
    :default ::= action => [symbol, name, values]
    lexeme default = action => [symbol, name, values] latm => 1
};

my $grammar_epilog = q{
    :discard ~ whitespace
    whitespace ~ [\s+]
};

#
# each node on the newline indented in 2-space increments
# literals in single quotes ''
# 
sub ast_show{
    my ($ast) = @_;
    state $depth++;
    my $s;
    my $indent = "  " x ($depth - 1);
    if (ref $ast){
        my ($node_id, @children) = @$ast;
        if (@children == 1 and not ref $children[0]){
            $s .= $indent . "$node_id '$children[0]'" . "\n";
        }
        else{
            $s .= $indent . "$node_id\n";
            $s .= join '', map { ast_show( $_ ) } @children;
        }
    }
    else{
        $s .= $indent . "'$ast'"  . "\n";
    }
    $depth--;
    return $s;
}

sub ast_show_compact{
    my ($ast) = @_;
    state $depth++;
    my $s;
    if (ref $ast){
        my ($node_id, @children) = @$ast;
        if (@children == 1 and not ref $children[0]){
            $s .= '(' . "$node_id '$children[0]'" . ')';
        }
        else{
            $s .= "($node_id ";
            $s .= join(' ', map { ast_show_compact( $_ ) } @children) . ')';
        }
    }
    else{
        $s .= "'$ast'";
    }
    $depth--;
    return $s;
}

#
# generate string $s from $ast
#
sub ast_derive{
    my ($ast) = @_;
    my $s;
    state $depth++;
    my $indent = "  " x ($depth - 1);
    if (ref $ast){
        my ($lhs, @nodes) = @$ast;
        if (@nodes == 1 and not ref $nodes[0]){ # lexeme
            $s .= $nodes[0];
        }
        else{
            $s .= join ' ', map { ast_derive( $_ ) } @nodes;
        }
    }
    else{ # scalar rule value
        $s .= $ast;
    }
    $depth--;
    return $s;
}

#
# convert nodes' symbol, name tuples to id's
#
sub ast_symbol_name_to_id {
    my ($ast) = @_;
    if (ref $ast){
        my ($name, $symbol, @nodes) = @$ast;
        if ($name eq $symbol){
            $ast->[0] = $name;
        }
        else{
            $ast->[0] = "$name/$symbol";
        }
        splice @$ast, 1, 1;
        map { ast_symbol_name_to_id ( $_ ) } @nodes;
    }
}

sub parse{
    my ($slr, $input) = @_;
    $slr->read( \$input );
    my $ast = ${ $slr->value() };
    ast_symbol_name_to_id($ast);
    return $ast;
}

for my $test (@$tests){
    my ($name, $inputs, $grammar_source) = @$test;
    warn "#\n# $name\n#";
#    warn $grammar_source;
    $grammar_source = $grammar_prolog . $grammar_source . $grammar_epilog;
    my $g = Marpa::R2::Scanless::G->new( { source  => \$grammar_source } );
    for my $input (@$inputs){

        # parse input string
        my $r = Marpa::R2::Scanless::R->new( { grammar => $g } );
        warn "# input:\n", $input;
        my $ast = parse( $r, $input );
        warn "# Dumper'ed ast\n",  Dumper $ast;
        warn "# ast\n", ast_show( $ast );
        warn "# compact ast\n", ast_show_compact( $ast );
        
        # generate string from ast (reproduce input)
        my $s = ast_derive( $ast );
        $r = Marpa::R2::Scanless::R->new( { grammar => $g } );
        my $s_ast = parse( $r, $input );
        
        is_deeply($s_ast, $ast, "ast from generated string");
    }
}

done_testing();
