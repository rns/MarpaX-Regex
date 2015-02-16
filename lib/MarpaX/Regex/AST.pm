package MarpaX::Regex::AST;

use 5.010;
use strict;
use warnings;

use Data::Dumper;
local $Data::Dumper::Indent = 1;
local $Data::Dumper::Terse = 1;
local $Data::Dumper::Purity = 0;

use Carp;
use Scalar::Util qw{ blessed };

sub new {
    my ($class, $ast) = @_;

    # scalar $ast means the user wants create an empty ast
    # and $ast is the root node ID
    unless (ref $ast){
        $ast = [ $ast ];
    }

    return MarpaX::Regex::AST::bless( $ast, $class );
}

sub MarpaX::Regex::AST::bless{

    my ($ast, $class) = @_;

    # bless root
    bless $ast, $class;
    # bless descendants
    $ast->walk( {
        visit => sub { CORE::bless $_[0], $class unless blessed $_[0] }
    } );

    return $ast;

}

# set last child if caller provides it,
# return the last child
sub last_child{
    my ($ast, $child) = @_;

    if (defined $child){
        croak "Child must be a ref to " . __PACKAGE__ unless ref $child eq __PACKAGE__;
        push @{ $ast }, $child;
    }
    return $ast->[-1];
}

sub _assert_options{
    my ($opts, $spec) = @_;

    $spec //= {};
    my $meth = (caller(1))[3];

    $opts //= 'undef';
    croak $meth . ": options must be a HASH ref, not $opts." unless ref $opts eq "HASH";

    for my $opt ( sort keys %{ $spec } ){
        my ($pred, $desc) = @{ $spec->{$opt} };
        croak $meth . ": '$opt' option required." unless exists $opts->{$opt};
        $opts->{$opt} //= 'undef';
        croak qq{$meth: $opt option must be a $desc, not $opts->{$opt}}
            unless $pred->( $opts->{$opt} );
    }

}

# skip option to filter intermediate nodes

sub walk{
    my ($ast, $opts ) = @_;

    _assert_options($opts, {
        visit => [ sub{ ref $_[0] eq "CODE" }, "CODE ref" ]
    });
    $opts->{traversal} //= 'preorder';
    $opts->{max_depth} //= 1_000_000;
    $opts->{skip} //= [];
    $opts->{skip} = { map { $_ => 1 } @{ $opts->{skip} } };
    $opts->{depth} = 0;

    return do_walk( $ast, $opts );
}

sub do_walk{
    my ($ast, $opts ) = @_;

    $ast = CORE::bless [ '#text', $ast ], __PACKAGE__ unless ref $ast;
    my ($node_id, @children) = @{ $ast };

    $opts->{depth}++ unless exists $opts->{skip}->{$node_id};

    unless ($opts->{depth} > $opts->{max_depth} or exists $opts->{skip}->{$node_id} ){
        $opts->{visit}->( $ast, { depth => $opts->{depth} } );
    }

    unless (@children == 1 and not ref $children[0]){ # [ literal name, literal value ]
        do_walk( $_, $opts  ) for grep { defined } @children;
    }

    $opts->{depth}-- unless exists $opts->{skip}->{$node_id};
}

sub sprint{
    my ($ast, $opts ) = @_;

    my $s = '';

    $opts //= { };
    _assert_options( $opts, { } );
    $opts->{indent} = '  ';

    # set visitor
    $opts->{visit} = sub {
        my ($ast, $context) = @_;
        my ($node_id, @children) = @$ast;
        my $indent = $opts->{indent} x ( $context->{depth} - 1 );
        if (@children == 1 and not ref $children[0]){
            $s .= qq{$indent $node_id '$children[0]'\n};
        }
        else{
            $s .= qq{$indent $node_id\n};
        }
    };

    $ast->walk( $opts );

    return $s;
}

sub concat{
    my ($ast, $opts ) = @_;

    my $s = '';

    $opts //= { };
    _assert_options( $opts, { } );
    $opts->{indent} = '  ';

    $opts->{visit} = sub {
        my ($ast, $context) = @_;
        my ($node_id, @children) = @$ast;
        my $indent = $opts->{indent} x ( $context->{depth} - 1 );
        if ($node_id eq "lhs" ){
            my $lhs = $children[0]->[1];
            $s .= "\n" . "(?#$lhs)";
        }
        elsif($node_id eq 'bare name'){
        }
        elsif (@children == 1 and not ref $children[0]){
            $s .= qq{$children[0]};
        }
    };

    $ast->walk( $opts );

    return $s . "\n";
}

sub distill{
    my ($ast) = @_;

    my $root;
    my $parent;
    my $statement;

    local $Data::Dumper::Indent = 0;

    my $opts = {
        skip => [
            'group', 'primary',
            'alternative rule',
            'symbol', 'symbol name',
            'character class', 'literal',
        ],
        visit => sub {
            my ($ast, $context) = @_;
            my ($node_id, @children) = @$ast;
            if ($node_id eq 'statements'){
                $root = MarpaX::Regex::AST->new( $node_id );
                $parent = $root;
            }
            elsif ($node_id eq 'statement'){
                $statement = $root->last_child( MarpaX::Regex::AST->new( $node_id ) );
#                warn "# parent of $node_id\n", Dumper $parent;
            }
            elsif ($node_id eq 'lhs'){
                $parent = $statement->last_child( MarpaX::Regex::AST->new( $node_id ) );
#                warn "# parent of $node_id\n", Dumper $parent;
            }
            elsif ($node_id eq 'alternatives'){
                $parent = $statement->last_child( MarpaX::Regex::AST->new( $node_id ) );
#                warn "# parent of $node_id\n", Dumper $parent;
            }
#            elsif ($node_id eq 'quantifier'){
#                $parent = $statement->last_child( MarpaX::Regex::AST->new( $node_id ) );
#                warn "# parent of $node_id\n", Dumper $parent;
#            }
            elsif (@children == 1 and not ref $children[0]){
                $node_id = 'symbol' if $node_id eq 'bare name';
                $parent->last_child( MarpaX::Regex::AST->new( $ast ) );
            }
            else{
                warn "# unknown node type: $node_id:\n", Dumper $ast;
            }
        }
    }; ## opts

    $ast->walk( $opts );

    return $root;
}

=head2 dump()
    This method returns Data::Dumper::Dumper($ast) setting local Data::Dumper options
=cut
sub dump{
    my ($ast, $opts ) = @_;
    $opts->{Indent} //= $Data::Dumper::Indent;
    $opts->{Deepcopy} //= $Data::Dumper::Deepcopy;
    $opts->{Terse} //= $Data::Dumper::Terse;
    local $Data::Dumper::Indent = $opts->{Indent};
    local $Data::Dumper::Deepcopy = $opts->{Deepcopy};
    local $Data::Dumper::Terse = $opts->{Terse};
    return Dumper $ast;
}

1;
