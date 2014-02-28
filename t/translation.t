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

    Unique IDs of Parse Tree Nodes

        If any rule has more than one rhs alternative, then a unique node id is 'lhs/name' otherwise the unique node id is the lhs of the rule.

    ast_show
        add structual elements like in st_timeflies.t that need a new line after them
        
=cut

use Marpa::R2;

use Data::Dumper;
$Data::Dumper::Indent = 0;
$Data::Dumper::Terse = 1;

use YAML;

my $tests = {
    #    2 + 3                         -- infix
    infix => [   
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
    prefix => [   
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
    postfix => [   
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
    JVM => [   
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
    English => [   
        [ 'the sum of 2 and 3', 'the product of 2 and 3' ],
        q{
            e ::= 'the' op 'of' int 'and' int
            op ~ 'sum' name => 'add' | 'product' name => 'mul'
            int  ~ [\d]
        }
    ],
};


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
            $s .= '[' . "'$node_id','$children[0]'" . ']';
        }
        else{
            $s .= "['$node_id',";
            $s .= join(',', map { ast_show_compact( $_ ) } @children) . ']';
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

#
# id/id/1 => literal
#
sub ast_to_hash{
    my ($ast) = @_;
    my $h = {};
    do_ast_to_hash( $ast, [], $h );
    return $h;
}

sub do_ast_to_hash {
    my ($ast, $k, $h) = @_;
    if (ref $ast){
        my ($node_id, @children) = @$ast;
        push @$k, $node_id;
        if (@children == 1 and not ref $children[0]){
            $h->{ join '/', @$k } = $children[0];
        }
        else{
            my $i;
            map { push @$k, $i++; do_ast_to_hash( $_, $k, $h ); pop @$k } @children;
        }
        pop @$k;
        
    }
    else{
#        warn join('/', @$k), ' => ', $ast;
        $h->{ join '/', @$k } = $ast;
    }
}

sub parse{
    my ($slr, $input) = @_;
    $slr->read( \$input );
    my $ast = ${ $slr->value() };
    ast_symbol_name_to_id($ast);
    return $ast;
}

=pod
2 + 3       ['e/add',['int','2'],['plus','+'],['int','3']]
e/add/0/int: 2
e/add/1/plus: +
e/add/2/int: 3

(+ 2 3)     ['e/add','(',['plus','+'],['int','2'],['int','3'],')']

(+ 2 3)     ['e/add','(',['plus','+'],['int','2'],['int','3'],')']
2 * 3       ['e/mul',['int','2'],['star','*'],['int','3']]
(* 2 3)     ['e/mul','(',['star','*'],['int','2'],['int','3'],')']

=cut

my $tt = {
    infix => {
        prefix => {
            'e/add' => ['e/add','(',['plus',undef],['int',undef],['int',undef],')'],
            'e/mul' => ['e/mul','(',['star',undef],['int',undef],['int',undef],')'],
            'e/add/1/plus' => 'e/add/1/plus',
            'e/add/2/int' => 'e/add/0/int',
            'e/add/3/int' => 'e/add/2/int',
            'e/mul/1/star' => 'e/mul/1/star',
            'e/mul/2/int' => 'e/mul/0/int',
            'e/mul/3/int' => 'e/mul/2/int',
        },
        postfix => {
            'e/add' => ['e/add','(',['int',undef],['int',undef],['plus',undef],')'],
            'e/mul' => ['e/mul','(',['int',undef],['int',undef],['star',undef],')'],
            'e/add/1/int' => 'e/add/0/int',
            'e/add/2/int' => 'e/add/2/int',
            'e/add/3/plus' => 'e/add/1/plus',
            'e/mul/1/int' => 'e/mul/0/int',
            'e/mul/2/int' => 'e/mul/2/int',
            'e/mul/3/star' => 'e/mul/1/star', 
        },
        English => {
            'e/add' => ['e','the',['op','sum'],'of',['int',undef],'and',['int',undef]],
            'e/mul' => ['e','the',['op','product'],'of',['int',undef],'and',['int',undef]],
            'e/3/int' => [ 'e/add/0/int', 'e/mul/0/int' ],
            'e/5/int' => [ 'e/add/2/int', 'e/mul/2/int' ],
        }
    },
};

sub do_ast_translate{
    my ($ast, $t, $h, $k) = @_;
#    warn Dump \@_;
    
    if (ref $ast){
        my ($node_id, @children) = @$ast;
        push @$k, $node_id;
        if (@children == 1 and not ref $children[0] and not defined $children[0]){
#            warn "key: '", join('/', @$k), "'";
            my $s_keys = $t->{ join('/', @$k) };
            $s_keys = [ $s_keys ] unless ref $s_keys eq "ARRAY";
#            warn Dump $s_keys;
            for my $s_key (@$s_keys){
                my $tv = $h->{ $s_key };    # target value
                next unless defined $tv;    # skip if 
                $ast->[1] = $tv;
            }
            
#            warn join('/', @$k), ' => ', $ast->[1];
        }
        else{
            my $i;
            map { push @$k, $i++; do_ast_translate( $_, $t, $h, $k ); pop @$k } @children;
        }
        pop @$k;
    }
    else{
#        warn join('/', @$k), ' => ', $ast;
    }
}

sub ast_translate{
    my ($ast, $t) = @_;

    my $h = ast_to_hash( $ast );
#   warn Dump $h;
    use Storable qw(dclone);
    my $t_ast = dclone( $t->{ $ast->[0] } );
    do_ast_translate( $t_ast, $t, $h, [] );
    return $t_ast;
}

for my $name (keys %$tests){
    my ($inputs, $grammar_source) = @{ $tests->{ $name } };
    warn "#\n# $name\n#";
#    warn $grammar_source;
    $grammar_source = $grammar_prolog . $grammar_source . $grammar_epilog;
    my $g = Marpa::R2::Scanless::G->new( { source  => \$grammar_source } );
    for my $input (@$inputs){

        # parse input string
        my $r = Marpa::R2::Scanless::R->new( { grammar => $g } );
        warn "# input:\n", $input;
        my $ast = parse( $r, $input );
        warn ast_show_compact( $ast );        
        warn Dump ast_to_hash( $ast );        
        # generate string from ast (reproduce input)
        my $s = ast_derive( $ast );
        $r = Marpa::R2::Scanless::R->new( { grammar => $g } );
        my $s_ast = parse( $r, $input );
        
        is_deeply($s_ast, $ast, "ast from derived string");
        
        # translate into other if there is the translation table
        for my $name_to ( keys %$tests ){
            next if $name eq $name_to;
#            warn $name, ' => ', $name_to;
            my $t = $tt->{$name}->{$name_to};
            next unless defined $t;
#            warn Dump $t;
            my $t_ast = ast_translate( $ast, $t );
            warn "# source ast\n", ast_show_compact( $ast );
            warn "# target ast\n", ast_show_compact( $t_ast );
            my $ts = ast_derive( $t_ast );
#            warn $ts;
            my $t_grammar_source = $grammar_prolog . $tests->{ $name_to }->[ 1 ] . $grammar_epilog;
#            warn $t_grammar_source;
            my $tg = Marpa::R2::Scanless::G->new( { source  => \$t_grammar_source } );
            my $tr = Marpa::R2::Scanless::R->new( { grammar => $tg } );
            my $parsed_ts_ast = parse( $tr, $ts );
            is_deeply($parsed_ts_ast, $t_ast, "$name -> $name_to: ast from string derived from translated ast");
        }
    }
}

done_testing();
