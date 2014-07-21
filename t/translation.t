use 5.010;
use strict;
use warnings;

use Test::More;

=pod Use Cases

    2 + 3                         -- infix
    (+ 2 3)                       -- prefix
    (2 3 +)                       -- postfix
    bipush 2                      -- JVM 
    bipush 3 
    iadd 
    the sum of 2 and 3            -- English
    -- http://www.cse.chalmers.se/edu/year/2012/course/DAT150/lectures/proglang-02.html
    
    Expression ::=
    Number
        | '(' Expression ')' assoc => group
       || Expression '**' Expression assoc => right
       || Expression '*' Expression
        | Expression '/' Expression
       || Expression '+' Expression
        | Expression '-' Expression
    
    -- https://metacpan.org/pod/distribution/Marpa-R2/pod/Scanless/DSL.pod
    
    A synchronous context free grammar for time normalization
    -- http://aclweb.org/anthology//D/D13/D13-1078.pdf
    -- https://github.com/bethard/timenorm
    
    Marpa <-> Perl Regexes
    
=cut

=pod Notes

    Unique IDs of Parse Tree Nodes

        If any rule has more than one rhs alternative, then a unique node id is 'lhs/name' otherwise the unique node id is the lhs of the rule.

    ast_show
        add structual elements like in st_timeflies.t that need a new line after them
    todo: 
      allow passing structural elements which need a new line after them
      left/right space normalization
      perhaps by postprocessing callbacks   
        
=cut

use Marpa::R2;

use Data::Dumper;
$Data::Dumper::Indent = 0;
$Data::Dumper::Terse = 1;

use YAML;

my $tests = {
    # typewriter double quotes (to be translated to curly (“...”) quotes) 
    'well-formed typewriter double quotes' => [
        [ 
            'these are "words in typewriter double quotes" and then some', 
            'these are "words in "nested typewriter double" quotes" and then some',  
            'these are "words in "nested "and even mode nested" typewriter double" quotes" and then some' ],
        q{
            S ::= S S       name => 'pair' |
                  '"' S '"' name => 'quoted' |
                  '"' '"'   name => 'empty-quoted' |
                  non_quotes name => 'non-quoted'

            non_quotes ~ non_quote*
            non_quote ~ [^"] #"
        },
        [
            ['S','"',['S',['non_quotes',' words in typewriter double quotes']],'"'],
            ['S','"',['S',['S',['S',['non_quotes',' words in ']],['S','"',['S',['non_quotes','nested angle']],'"']],['S',['non_quotes',' quotes ']]],'"'],
            ['S/quoted','"',['S/pair',['S/pair',['S/non-quoted',['non_quotes',' words in ']],['S/quoted','"',['S/pair',['S/pair',['S/non-quoted',['non_quotes','nested ']],['S/quoted','"',['S/non-quoted',['non_quotes','and even mode nested']],'"']],['S/non-quoted',['non_quotes',' angle']]],'"']],['S/non-quoted',['non_quotes',' quotes ']]],'"']
        ]
    ], 
    # curly double quotes
    'well-formed curly double quotes' => [
        [ 
            'these are “words in curly double quotes” and then some', 
            'these are “words in “nested curly double” quotes” and then some',  
            'these are “words in “nested “and even mode nested” curly double” quotes” and then some' ],
        q{
            S ::= S S       name => 'pair' |
                  '“' S '”' name => 'quoted' |
                  '“' '”'   name => 'empty-quoted' |
                  non_quotes name => 'non-quoted'

            non_quotes ~ non_quote*
            non_quote ~ [^“”] #"
        },
        [
            ['S','"',['S',['non_quotes',' words in curly double quotes']],'"'],
            ['S','"',['S',['S',['S',['non_quotes',' words in ']],['S','"',['S',['non_quotes','nested angle']],'"']],['S',['non_quotes',' quotes ']]],'"'],
            ['S/quoted','"',['S/pair',['S/pair',['S/non-quoted',['non_quotes',' words in ']],['S/quoted','"',['S/pair',['S/pair',['S/non-quoted',['non_quotes','nested ']],['S/quoted','"',['S/non-quoted',['non_quotes','and even mode nested']],'"']],['S/non-quoted',['non_quotes',' angle']]],'"']],['S/non-quoted',['non_quotes',' quotes ']]],'"']
        ]
    ], 
    #    2 + 3                         -- infix
    infix => [   
        [ '2 + 3', '2 * 3' ],
        q{
            e   ::= int plus int name => 'add' | 
                    int star int name => 'mul'
            int  ~ [\d]   
            plus ~ '+'
            star ~ '*'
        },
        [ 
            ['e/add','(',['int','2'],['int','3'],['plus','+'],')'],
            ['e/mul','(',['int','2'],['int','3'],['star','*'],')']
        ]
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
        },
        [
            ['e/add','(',['plus','+'],['int','2'],['int','3'],')'],
            ['e/mul','(',['star','*'],['int','2'],['int','3'],')']
        ]
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
        },
        [
            ['e/add','(',['int','2'],['int','3'],['plus','+'],')'],
            ['e/mul','(',['int','2'],['int','3'],['star','*'],')']
        ]
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
        },
        [
            ['e/add',['push','bipush',['int','2']],['push','bipush',['int','3']],['add','iadd']],
            ['e/mul',['push','bipush',['int','2']],['push','bipush',['int','3']],['mul','imul']]
        ]
    ],
    #    the sum of 2 and 3            -- English
    English => [   
        [ 'the sum of 2 and 3', 'the product of 2 and 3' ],
        q{
            e ::= 'the' op 'of' int 'and' int
            op ~ 'sum' name => 'add' | 'product' name => 'mul'
            int  ~ [\d]
        },
        [
            ['e','the',['op','sum'],'of',['int','2'],'and',['int','3']],
            ['e','the',['op','product'],'of',['int','2'],'and',['int','3']]
        ]
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

#
# ast in one line
#
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
        if (index($name, '/') >= 0){
            # do nothing -- the id has already been set to $ast->[0]
            # which is really a ref so it'll propagate
        }
        elsif ($name eq $symbol){ # $name and $symbol are the same -- lexeme
            $ast->[0] = $name;
        }
        else{ # $name and $symbol are different: RHS alternative
            $ast->[0] = "$name/$symbol";
        }
        splice @$ast, 1, 1;
        map { ast_symbol_name_to_id ( $_ ) } @nodes;
    }
}

#
# turn a hash produced by ast_to_hash() back to an ast
#
sub hash_to_ast{
    my ($hash) = @_;
    
#    warn "hash_to_ast:";
#    warn Dump $hash;

    my $ast = [ ];
    
    for my $path (sort keys %$hash){

#        warn "\npath:  ", $path, ": '$hash->{ $path }'";

        my @path = split q{/(\d+)/?}, $path;
#        warn "split: ", join ' ', @path;
        
        # a path can end in either named or unnamed terminal
        # that must become the value of the last node (array item at index $i)
        my $value;
        if ( @path % 2 == 0 ){
            $value = $hash->{ $path };
        }
        else{
            $value = [ pop @path, $hash->{ $path } ];
        }
#        warn "value: ", Dump $value;

#        warn "Odd number of items in path: ", scalar(@path) unless @path % 2 == 0;
        
        my $node = $ast;
        for ( my $ix = 0; $ix <= $#path; $ix += 2 ) {
            my $name = $path[$ix];      # node's name
            my $i    = $path[$ix + 1];  # index in the array of node's values 
#            warn "    $name @ $i";
#            warn "    $ix, $#path: ", Dump $node;
            $node->[ 0 ] = $name;
            if ($ix == $#path - 1){ # last node 
                $node->[ $i + 1 ] = $value;
            }
            else{ # next node
                unless ( defined $node->[ $i + 1 ] ){ # avoid re-initializing the node
                    $node->[ $i + 1] = [];
                }
                $node = $node->[ $i + 1 ];
            }
        }
    }
#    warn Dump $ast;
#    warn "# re-created from hash: ";
#    warn ast_show( $ast );
#    warn ast_show_compact( $ast );
    return $ast;
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

#
# read $input with SLIF recognizer $slr
# get parse tree (ast)
# convert [ symbol, name ] pairs to ast nodes' id's
#
sub parse{
    my ($slr, $input) = @_;
    $slr->read( \$input );
    my $ast = ${ $slr->value() };
#    warn Dumper $ast;
#    warn ast_show($ast);
    ast_symbol_name_to_id($ast);
    return $ast;
}


my $tt_test = {
    'infix' => {
        'postfix' => {
            '' => { 
                'e/add/0' => '(',
                'e/mul/0' => '(',
                'e/add/4' => ')',
                'e/mul/4' => ')',
            }
        }
    }
};
warn Dump $tt_test;

#
# Translation Table
#
=pod Translation Table format       
    
# source ast path and values
$tt->{ $source_id }->{ $target_id } = {
    
}

# $tt -> { infix } -> { postfix }

# scalar => scalar 
#   source_path => target_path -- 
#       copy value from source_path in the source ast to target_path in the target ast
'e/add/0/int'   => 'e/add/1/int',
'e/add/2/int'   => 'e/add/2/int',
'e/add/1/plus'  => 'e/add/3/plus',
'e/mul/1/star' => 'e/mul/3/star',

# '' => hash_ref
#   empty string (no such key in source ast):
#       copy the paths and values from the hash ref to the target ast
'' => { 
    'e/add/0' => '(',
    'e/mul/0' => '(',
    'e/add/4' => ')',
    'e/mul/4' => ')',
}

# not exists $t->{$source_ast_path} 
#   source ast path not found in translation table:
#       copy it and its value to the target ast

# scalar => hash_ref
#   ???
source_path => { target_path => target_value }

# scalar => array_ref
#   ???
source_path => [ target_path => target_value ]

# regex => array_ref
source_path_search_regex => [ path_replace_regex, value_replace_regex ]

# scalar => code_ref
#   call code_ref->($path, $value) on path eq $scalar
#   and copy the resulting hash ref to the target ast
source_path => code_ref

# regex => code_ref
#   call code_ref on source ast' paths matching $path_search_regex
#   and copy the resulting hash ref to the target ast
source_path_search_regex => code_ref

# ==========================================================
# ========================== ASTs ==========================
# ==========================================================

# English

e/0: the
e/1/op: sum
e/2: of
e/3/int: 2
e/4: and
e/5/int: 3

e/0: the
e/1/op: product
e/2: of
e/3/int: 2
e/4: and
e/5/int: 3

# JVM

e/add/0/push/0: bipush
e/add/0/push/1/int: 2
e/add/1/push/0: bipush
e/add/1/push/1/int: 3
e/add/2/add: iadd

e/mul/0/push/0: bipush
e/mul/0/push/1/int: 2
e/mul/1/push/0: bipush
e/mul/1/push/1/int: 3
e/mul/2/mul: imul

# infix

e/add/0/int: 2
e/add/1/plus: +
e/add/2/int: 3

e/mul/0/int: 2
e/mul/1/star: '*'
e/mul/2/int: 3

# postfix

e/add/0: (
e/add/1/int: 2
e/add/2/int: 3
e/add/3/plus: +
e/add/4: )

e/mul/0: (
e/mul/1/int: 2
e/mul/2/int: 3
e/mul/3/star: '*'
e/mul/4: )

# prefix

e/add/0: (
e/add/1/plus: +
e/add/2/int: 2
e/add/3/int: 3
e/add/4: )

e/mul/0: (
e/mul/1/star: '*'
e/mul/2/int: 2
e/mul/3/int: 3
e/mul/4: )

# well-formed curly double quotes

S/pair/0/S/pair/0/S/non-quoted/0/non_quotes: 'these are '
S/pair/0/S/pair/1/S/quoted/0: вЂњ
S/pair/0/S/pair/1/S/quoted/1/S/non-quoted/0/non_quotes: words in curly double quotes
S/pair/0/S/pair/1/S/quoted/2: вЂќ
S/pair/1/S/non-quoted/0/non_quotes: ' and then some'

S/pair/0/S/pair/0/S/non-quoted/0/non_quotes: 'these are '
S/pair/0/S/pair/1/S/quoted/0: вЂњ
S/pair/0/S/pair/1/S/quoted/1/S/pair/0/S/pair/0/S/non-quoted/0/non_quotes: 'words in '
S/pair/0/S/pair/1/S/quoted/1/S/pair/0/S/pair/1/S/quoted/0: вЂњ
S/pair/0/S/pair/1/S/quoted/1/S/pair/0/S/pair/1/S/quoted/1/S/non-quoted/0/non_quotes: nested curly double
S/pair/0/S/pair/1/S/quoted/1/S/pair/0/S/pair/1/S/quoted/2: вЂќ
S/pair/0/S/pair/1/S/quoted/1/S/pair/1/S/non-quoted/0/non_quotes: ' quotes'
S/pair/0/S/pair/1/S/quoted/2: вЂќ
S/pair/1/S/non-quoted/0/non_quotes: ' and then some'

S/pair/0/S/pair/0/S/non-quoted/0/non_quotes: 'these are '
S/pair/0/S/pair/1/S/quoted/0: вЂњ
S/pair/0/S/pair/1/S/quoted/1/S/pair/0/S/pair/0/S/non-quoted/0/non_quotes: 'words in '
S/pair/0/S/pair/1/S/quoted/1/S/pair/0/S/pair/1/S/quoted/0: вЂњ
S/pair/0/S/pair/1/S/quoted/1/S/pair/0/S/pair/1/S/quoted/1/S/pair/0/S/pair/0/S/non-quoted/0/non_quotes: 'nested '
S/pair/0/S/pair/1/S/quoted/1/S/pair/0/S/pair/1/S/quoted/1/S/pair/0/S/pair/1/S/quoted/0: вЂњ
S/pair/0/S/pair/1/S/quoted/1/S/pair/0/S/pair/1/S/quoted/1/S/pair/0/S/pair/1/S/quoted/1/S/non-quoted/0/non_quotes: and even mode nested
S/pair/0/S/pair/1/S/quoted/1/S/pair/0/S/pair/1/S/quoted/1/S/pair/0/S/pair/1/S/quoted/2: вЂќ
S/pair/0/S/pair/1/S/quoted/1/S/pair/0/S/pair/1/S/quoted/1/S/pair/1/S/non-quoted/0/non_quotes: ' curly double'
S/pair/0/S/pair/1/S/quoted/1/S/pair/0/S/pair/1/S/quoted/2: вЂќ
S/pair/0/S/pair/1/S/quoted/1/S/pair/1/S/non-quoted/0/non_quotes: ' quotes'
S/pair/0/S/pair/1/S/quoted/2: вЂќ
S/pair/1/S/non-quoted/0/non_quotes: ' and then some'
    
=cut            
my $tt = {
    'well-formed typewriter double quotes' => {
    },
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
        JVM => {
            'e/add' => ['e/add',['push','bipush',['int',undef]],['push','bipush',['int','3']],['add','iadd']],
            'e/mul' => ['e/mul',['push','bipush',['int',undef]],['push','bipush',['int','3']],['mul','imul']],
            'e/mul/0/push/1/int' => 'e/mul/0/int',
            'e/mul/1/push/1/int' => 'e/mul/2/int',
            'e/add/0/push/1/int' => 'e/add/0/int',
            'e/add/1/push/1/int' => 'e/add/2/int',
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
        # handle terminals
        if (@children == 1 and not ref $children[0] and not defined $children[0]){
#            warn "key: '", join('/', @$k), "'";
            my $s_keys = $t->{ join('/', @$k) }; # source keys
            $s_keys = [ $s_keys ] unless ref $s_keys eq "ARRAY";
#            warn Dump $s_keys;
            for my $s_key (@$s_keys){
                # todo: there must be only one target value
                my $tv = $h->{ $s_key };    # get target value
                next unless defined $tv;    # skip if there is no target value 
                                            # in source parse tree
                $ast->[1] = $tv;            # set target value to target parse tree
            }
            # keys are for terminals; if no key is found, 
            # the tree must be built further
            # to augment the key
            
#            warn join('/', @$k), ' => ', $ast->[1];
        }
        # handle non-terminals
        else{
            # try rhs alternatives as $s_key prefix search in $h by regexp
            #
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

for my $name (sort keys %$tests){
    my ($inputs, $grammar_source, $trees) = @{ $tests->{ $name } };

    $grammar_source = $grammar_prolog . $grammar_source . $grammar_epilog;
    my $g = Marpa::R2::Scanless::G->new( { source  => \$grammar_source } );
    
    for my $i (0 .. @$inputs - 1){

        diag "\n", $name;
        
        # parse input string
        my $input = $inputs->[ $i ];
        my $r = Marpa::R2::Scanless::R->new( { grammar => $g } );
        my $ast = parse( $r, $input );

        # derive string from ast (must parse to the same tree as the input)
        my $s = ast_derive( $ast );
        $r = Marpa::R2::Scanless::R->new( { grammar => $g } );
        my $s_ast = parse( $r, $input );

        is_deeply($s_ast, $ast, "'$s' derived from parse tree");
        
        # serialize/deserialize ast from/to hash of paths
        my $hash_ast = ast_to_hash($s_ast) ;
        # deserialized ast
        my $ds_ast = hash_to_ast($hash_ast);
        
#        warn Dump $hash_ast;
        
        is_deeply($ds_ast, $s_ast, "ast re-created from hash");

        # translate into other if there is the translation table
        for my $name_to ( keys %$tests ){

            next if $name eq $name_to;

            my $t = $tt->{$name}->{$name_to};
            next unless defined $t;

            diag ' ' x length($name), ' => ', $name_to;
#            warn Dump $t;

            my $t_ast = ast_translate( $ast, $t );
            my $t_s = ast_derive( $t_ast );

            is_deeply $t_ast, $tests->{ $name_to }->[ 2 ]->[ $i ], "$s -> $t_s";
            
        }

    }

}

done_testing();
