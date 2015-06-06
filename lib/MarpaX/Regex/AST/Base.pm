package MarpaX::Regex::AST::Base;

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

    # scalar $ast means the application wants to create an empty ast
    # and $ast is the root node ID
    unless (ref $ast){
        $ast = [ $ast ];
    }

    return MarpaX::Regex::AST::Base::bless( $ast, $class );
}

sub MarpaX::Regex::AST::Base::bless{

    my ($ast, $class) = @_;

    # bless root
    bless $ast, $class;
    # bless descendants
    $ast->walk( {
        visit => sub { CORE::bless $_[0], $class unless blessed $_[0] }
    } );

    return $ast;

}

# set node id if caller provides it,
# return node id
sub id{
    my ($ast, $id) = @_;
    if (defined $id){
        $ast->[0] = $id;
    }
    return $ast->[0];
}

# return true if the node is literal node -- [ 'name', 'value' ]
sub is_literal{
    my ($ast) = @_;
    my ($node_id, @children) = @{ $ast };
    return ( (@children == 1) and (not ref $children[0]) );
}

sub descendant{
    my ($ast, $level) = @_;
    $ast = $ast->[1] for (1..$level);
    return $ast;
}

# set the child at index $ix if caller provides it,
# if Arg3 is an array ref, use splice
# to expand child at $ix with Arg3's contents
# return the child at index $ix
sub child{
    my ($ast, $ix, $child) = @_;
    $ix++ unless $ix == -1;
    if (defined $child){
        if (ref $child eq "ARRAY"){
            splice @$ast, $ix, 1, @$child;
        }
        else{
           croak "Child must be a blessed ref, not " . $child unless blessed $child;
            $ast->[$ix] = $child;
        }
    }
    return $ast->[$ix];
}

# set first child if the caller provides it,
# return the first child that matches $predicate if arg 1 is a code ref
sub first_child{
    my ($ast, $child) = @_;
    return $ast->child(0, $child);
}

# set last child if the caller provides it,
# return the last child that matches $predicate if arg 1 is a code ref
sub last_child{
    my ($ast, $child) = @_;
    return $ast->child(-1, $child);
}

# insert $child at position just before $ix
# return splice() return value
sub insert_before_child{
    my ($ast, $ix, $child) = @_;
    $ix++ if $ix == 0; # a node is an array and children start at position 1
    return splice @$ast, $ix, 0, $child;
}

# append $child to children
# return the last child
sub append_child{
    my ($ast, $child) = @_;

    if (defined $child){
        croak "Child must be a blessed ref, not " . $child unless blessed $child;
        push @{ $ast }, $child;
    }
    return $ast->[-1];
}

# set the node's children to $children array ref if caller provides it,
# if $children is a CODE ref, returns the nodes' children for which $children->($child) returns 1
# return children
sub children{
    my ($ast, $children) = @_;
    my ($node, @children) = @$ast;
    if (defined $children){
        if (ref $children eq "CODE"){
            my $found = [];
            for my $ix (0..$#children){
                push @$found, $children[$ix] if $children->( $children[$ix], $ix );
            }
        }
        else {
            splice @$ast, 1, @$ast - 1, @$children;
        }
    }
    return \@children;
}

sub children_count{
    my ($ast) = @_;
    return scalar ( @$ast ) - 1;
}

# remove children for which $remove sub returns a true value
sub remove_children{
    my ($ast, $remove) = @_;

    croak "Arg 2 must be defined as a CODE ref." unless defined $remove;
    croak "Arg 2 must be a ref to CODE, not " . ref $remove unless ref $remove eq "CODE";

    my ($node_id, @children) = @$ast;
    my @new_children;
    for my $child (@children){
        next if $remove->($child);
        push @new_children, $child;
    }
    $ast->children(\@new_children);

    return $ast;
}

# remove child at $ix
# return removed child
sub remove_child{
    my ($ast, $ix) = @_;
    croak "Arg 2 must be defined as an index of the child to be removed." unless defined $ix;
    return splice ( @$ast, $ix + 1, 1 );
}

sub assert_options{
    my ($ast, $opts, $spec) = @_;

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

# assert options and do_walk()
sub walk{
    my ($ast, $opts ) = @_;

    $ast->assert_options($opts, {
        visit => [ sub{ ref $_[0] eq "CODE" }, "CODE ref" ]
    });
    $opts->{traversal} //= 'preorder';
    $opts->{max_depth} //= 1_000_000;
    $opts->{skip} //= sub { 0 };
    $opts->{depth} = 0;

    return do_walk( $ast, $opts );
}

sub do_walk{
    my ($ast, $opts ) = @_;

    $ast = CORE::bless [ '#text', $ast ], __PACKAGE__ unless ref $ast;

    my $context = { depth => $opts->{depth} };

    my $skip = $opts->{skip}->( $ast, $context );

    $opts->{depth}++ unless $skip;

    unless ($opts->{depth} > $opts->{max_depth} or $skip) {
        $opts->{visit}->( $ast, $context );
    }

    my ($node_id, @children) = @{ $ast };
    # don't walk into [ 'name', 'value' ] nodes
    unless ( $ast->is_literal ){
        # todo: set siblings and parents for context
        do_walk( $_, $opts  ) for @children;
    }

    $opts->{depth}-- unless $skip;
}

sub sprint{
    my ($ast, $opts ) = @_;

    my $s = '';

    $opts //= { };
    $ast->assert_options( $opts, { } );
    $opts->{indent} = '  ';

    # set visitor
    $opts->{visit} = sub {
        my ($ast, $context) = @_;
        my ($node_id, @children) = @$ast;
        my $indent = $opts->{indent} x ( $context->{depth} );
        if ( $ast->is_literal ){
            $s .= qq{$indent $node_id '$children[0]'\n};
        }
        else{
            $s .= qq{$indent $node_id\n};
        }
    };

    $ast->walk( $opts );

    return $s;
}

sub distill{
    my ($ast, $opts) = @_;

    my $class = ref $ast;

    my $root = $class->new( $opts->{root} );
    my $parents = [ $root ];

    $ast->walk( {
        skip => sub {
            my ($ast) = @_;
            my ($node_id) = @$ast;
            state $skip = { map { $_ => 1 } @{ $opts->{skip} } };
            return exists $skip ->{ $node_id }
        },
        visit => sub {
            my ($ast, $ctx) = @_;
            my ($node_id, @children) = @$ast;

            state $dont_visit = { map { $_ => 1 } @{ $opts->{dont_visit} } };
            state $dont_visit_ix = keys %$dont_visit;

            $opts->{print_node}->($ast, $ctx) if exists $opts->{print_node};

            return if exists $dont_visit->{ $node_id };

            my $parent_ix = $ctx->{depth} - $dont_visit_ix;

            $parents->[ $parent_ix + 1 ] =
                $parents->[ $parent_ix ]->append_child(
                    $class->new( $ast->is_literal ? $ast : $node_id ) );
        }
    } );

    return $root;
}

1;

