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

# assert options and do_walk()
sub walk{
    my ($ast, $opts ) = @_;

    _assert_options($opts, {
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
    my ($node_id, @children) = @{ $ast };

    my $context = { depth => $opts->{depth} };

    my $skip = $opts->{skip}->( $ast, $context );

    $opts->{depth}++ unless $skip;

    unless ($opts->{depth} > $opts->{max_depth} or $skip) {
        $opts->{visit}->( $ast, $context );
    }

    unless (@children == 1 and not ref $children[0]){ # [ literal name, literal value ]
        do_walk( $_, $opts  ) for grep { defined } @children;
    }

    $opts->{depth}-- unless $skip;
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
        my $indent = $opts->{indent} x ( $context->{depth} );
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

    my $parent_node_id;

    $opts->{visit} = sub {
        my ($ast, $context) = @_;
        my ($node_id, @children) = @$ast;
        my $indent = $opts->{indent} x ( $context->{depth} - 1 );

        if ($node_id eq 'lhs' or $node_id eq 'alternatives'){
            $parent_node_id = $node_id;
            $s .= "\n" if $parent_node_id eq 'lhs';
        }
        elsif($node_id eq 'bare name' or $node_id eq 'bracketed name'){
            if ($parent_node_id eq 'lhs' or $parent_node_id eq 'alternatives'){
                my $lhs = $children[0];
                $s .= "(?#$lhs)";
            }
        }
        elsif (@children == 1 and not ref $children[0]){
            $s .= qq{$children[0]};
        }

    };

    $ast->walk( $opts );

    return $s . "\n";
}

# merge statement nodes having the same lhs with alternation '|'
# into the first occurrence of such statement and delete other occurrences
sub merge{
    my ($ast) = @_;

#   gather all statements having the same lhs
    my %alternatives_by_lhs;
    my $opts = {
        visit => sub {
            my ($ast, $context) = @_;
            my ($node_id, @children) = @$ast;
            if ($node_id eq 'statement'){
                my ($lhs, $alternatives) = ( $children[0]->[1]->[1], $children[1] );
                push @{ $alternatives_by_lhs{ $lhs } }, $alternatives ;
            }
        }
    }; ## opts
    $ast->walk( $opts );
    my %mergeable_alternatives = map { $_ => $alternatives_by_lhs{$_} }
        grep { @{ $alternatives_by_lhs{$_} } > 1 } keys %alternatives_by_lhs;

#    warn Dumper \%mergeable_alternatives;
#   join mergeable alternatives with '|'
    my $alternation = MarpaX::Regex::AST->new( [ 'alternation', '|' ] );
    for my $lhs (keys %mergeable_alternatives){
        my @merged;
        for my $ms ( @{ $mergeable_alternatives{ $lhs } }){
#            warn Dumper $ms;
            my ($node_id, @children) = @$ms;
            push @merged, @children, $alternation;
        }
        pop @merged;
        $mergeable_alternatives{ $lhs } = MarpaX::Regex::AST->new(
            [ 'alternatives', @merged ] );
    }
#    warn $mergeable_alternatives{$_}->sprint for keys %mergeable_alternatives;

    # replace first occurrence of mergeable alternatives' lhs
    # with merged alternatives
    $opts = {
        visit => sub {
            my ($ast, $context) = @_;
            my ($node_id, @children) = @$ast;
            if ($node_id eq 'statement'){
                my $lhs = $children[0]->[1]->[1];
                if ( exists $mergeable_alternatives{ $lhs } ){
                    if (defined $mergeable_alternatives{ $lhs }){
#                        warn "# first occurrence of $lhs:\n", $ast->[2]->sprint;
                        $ast->[2] = $mergeable_alternatives{ $lhs };
                        $mergeable_alternatives{ $lhs } = undef;
                    }
                    else{
                        # mark for deletion
                        $ast->[2] = undef;
                    }
                }
            }
        }
    }; ## opts
    $ast->walk( $opts );
    # delete all occurrences of mergeable alternatives' lhs except
    # the first
    $opts = {
        visit => sub {
            my ($ast, $context) = @_;
            my ($node_id, @children) = @$ast;
            if ($node_id eq 'statements'){
                my @new_children;
                for my $statement (@children){
#                    warn "# stat:", $statement->sprint;
                    next unless defined $statement->[2];
#                    warn "# kept";
                    push @new_children, $statement;
                }
                @{ $ast }[1..$#children + 1] = @new_children;
            }
        }
    }; ## opts

    $ast->walk( $opts );

#    warn $ast->sprint;
    return $ast;
}

# substitute named terminal nodes contents instead of name occurrencs
# and delete named terminal nodes if they become inaccessible
sub substitute{
    my ($ast) = @_;

    $ast->merge();

    # while (find_terminals()) {
    #   substitute occurrence of terminal names with their contents
    #   remove inaccessible terminals
    # }

    return $ast;
}

sub distill{
    my ($ast) = @_;

    my $root;
    my $parent;
    my $statement;

    local $Data::Dumper::Indent = 0;

    my %node_skip_list = map { $_ => 1 } (
        'group', 'primary',
        'alternative rule',
        'symbol', 'symbol name',
        'character class', 'literal'
    );

    my $opts = {
        skip => sub {
            my ($ast, $context) = @_;
            my ($node_id, @children) = @$ast;
            return exists $node_skip_list{ $node_id }
        },
        visit => sub {
            my ($ast, $context) = @_;
            my ($node_id, @children) = @$ast;
            # parent nodes
            if ($node_id eq 'statements'){
                $root = MarpaX::Regex::AST->new( $node_id );
                $parent = $root;
            }
            elsif ($node_id eq 'statement'){
                $statement = $root->last_child( MarpaX::Regex::AST->new( $node_id ) );
            }
            elsif ($node_id eq 'lhs'){
                $parent = $statement->last_child( MarpaX::Regex::AST->new( $node_id ) );
            }
            elsif ($node_id eq 'alternatives'){
                $parent = $statement->last_child( MarpaX::Regex::AST->new( $node_id ) );
            }
            # child nodes; #text nodes will also be added here
            # so we don't care about <quantifier modifier>'s (yet)
            elsif (@children == 1 and not ref $children[0]){
                $parent->last_child( MarpaX::Regex::AST->new( $ast ) );
            }
            else{
                # debug-only
                # warn "# unknown node type: $node_id:\n", Dumper $ast;
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
