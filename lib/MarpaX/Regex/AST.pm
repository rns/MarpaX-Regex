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

# set node id if caller provides it,
# return node id
sub id{
    my ($ast, $id) = @_;
    if (defined $id){
        $ast->[0] = $id;
    }
    return $ast->[0];
}

# set the child at index $ix if caller provides it,
# if $child is an array ref
#   else replace child with @$child
# return the child at index $ix
sub child{
    my ($ast, $ix, $child) = @_;
    return $ast->[$ix + 1];
}

# set first child if the caller provides it,
# return the first child that matches $predicate if arg 1 is a code ref
sub first_child{
    my ($ast, $child) = @_;
    return $ast->child(0, $child);
}

# append $child to children
# return the last child
sub append_child{
    my ($ast, $child) = @_;

    if (defined $child){
        croak "Child must be a ref to " . __PACKAGE__ unless ref $child eq __PACKAGE__;
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
                push @$found, $children[$ix] if $children->( $children[$ix] );
            }
        }
        else {
            splice @$ast, 1, @$ast - 1, @$children;
        }
    }
    return \@children;
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

    my $context = { depth => $opts->{depth} };

    my $skip = $opts->{skip}->( $ast, $context );

    $opts->{depth}++ unless $skip;

    unless ($opts->{depth} > $opts->{max_depth} or $skip) {
        $opts->{visit}->( $ast, $context );
    }

    my ($node_id, @children) = @{ $ast };
    unless (@children == 1 and not ref $children[0]){ # [ literal name, literal value ]
        # todo: set siblings and parents for context
        do_walk( $_, $opts  ) for @children;
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
                $ast->children(\@new_children);
            }
        }
    }; ## opts

    $ast->walk( $opts );

    return $ast;
}

# return symbols as a list or undef if there isn't any
sub symbols{
    my ($ast) = @_;

    my %symbol_name = map { $_ => 1 } ('bare name', 'bracketed name');
    my @symbols;
    my $opts = {
        skip => sub {
            my ($ast, $context) = @_;
            # symbol is a statement ...
            return 1 unless $ast->id eq 'statement';
            # having no symbol name or bracketed name alternatives
            my @symbols = grep { exists $symbol_name{$_->id()} }
                    @{ $ast->child(1)->children() };
            return @symbols > 0; # skip non-symbols
        },
        visit => sub {
            my ($ast, $context) = @_;
            push @symbols, $ast;
        }
    }; ## opts
    $ast->walk( $opts );

    return @symbols > 0 ? \@symbols : undef;
}

# replace occurrences of symbol's name with symbol's alternatives
sub replace_symbols{
    my ($ast, $symbols) = @_;

    my %symbols;
    for my $t (@$symbols){
        my $t_lhs = $t->first_child->first_child->first_child;
        my $t_alternatives = $t->child(1)->children;
#        warn Dumper $t_alternatives;
#        warn "# replacing $t_lhs:\n", $t->sprint;
        $symbols{$t_lhs} = $t_alternatives;
    }

    # symbol must be deleted if it's been used in replacement at least once
    my $deletable_symbols = {};
    my $opts = {
        visit => sub {
            my ($ast, $context) = @_;
            if ($ast->id eq 'statement'){
                my $lhs = $ast->[1]->[1]->[1];
#                    warn "#stat $lhs: ", $ast->sprint;
                return if exists $symbols{ $lhs }; # don't replace itself
                my $alternatives = $ast->child(1)->children;
                # in reverse order to replace from the end
                for (my $ix = @$alternatives - 1; $ix >= 0; $ix--){
                    my $alternative = $alternatives->[$ix];
                    my $id = $alternative->id();
#                    warn $id;
                    if ($id eq 'bare name' or $id eq 'bracketed name'){
                        my $symbol = $alternative->first_child();
                        if (exists $symbols{ $symbol } ){
#                            warn "# $ix-th child '$symbol' needs replacing:\n", $ast->[2]->[$ix+1]->sprint;
                            splice(@{ $ast->[2] }, $ix + 1, 1, @{ $symbols{ $symbol } });
                            $deletable_symbols->{ $symbol }++;
                        }
                    }
                }
            }
        }
    }; ## opts
    $ast->walk( $opts );

#    warn "# after symbol replacement before deletion:\n", $ast->dump;

    # delete symbols statements we've just replaced
    $opts = {
        visit => sub {
            my ($ast, $context) = @_;
            my ($node_id, @children) = @$ast;
            if ($node_id eq 'statements'){
#                warn "# checking for deletion stats:\n", Dumper $ast;
                my @new_children;
                for my $statement (@children){
#                    warn "# checking for deletion stat:\n", $statement->sprint;
                    next if exists $deletable_symbols->{ $statement->first_child->first_child->first_child };
#                    warn "# kept";
                    push @new_children, $statement;
                }
                $ast->children(\@new_children);
            }
        }
    }; ## opts
    $ast->walk( $opts );

#    warn "# After deletion:\n", $ast->dump;

    return %$deletable_symbols ? 1 : 0;
}

=head2

    find recursive statements and translate them to regex syntax

    A statement is recursive if one or more alternatives contain symbols,
    which are the same as lhs; recursive statement must become
    a named capture group (?<$lhs>...) and references to it in alternatives
    must become (?&$lhs) recurses into that group.

=cut

sub recurse{
    my ($ast) = @_;
    my $recursive_statements = $ast->children(
        sub{
            my ($statement) = @_;
#            warn $statement->sprint;
        }
    );
    return $ast;
}

# substitute symbol nodes contents instead of symbol name occurrencs
# and delete symbol nodes if they become inaccessible
sub substitute{
    my ($ast) = @_;

    $ast->merge();

#    warn "# with NO symbols replaced:\n", $ast->sprint;

    while (1){
        my $symbols = $ast->symbols();
#        warn $_->sprint for @$symbols;
        last if not $ast->replace_symbols($symbols);
#        warn "# with symbols replaced:\n", $ast->sprint;
    }

    return $ast;
}

sub distill{
    my ($ast) = @_;

    my $root;
    my $parent;
    my $statement;

    local $Data::Dumper::Indent = 0;

    my $opts = {
        skip => sub {
            my ($ast, $context) = @_;
            my ($node_id, @children) = @$ast;
            state $node_skip_list = { map { $_ => 1 } (
                'group', 'primary',
                'alternative rule',
                'symbol', 'symbol name',
                'character class', 'literal'
            ) };
            return exists $node_skip_list->{ $node_id }
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
                $statement = $root->append_child( MarpaX::Regex::AST->new( $node_id ) );
            }
            elsif ($node_id eq 'lhs'){
                $parent = $statement->append_child( MarpaX::Regex::AST->new( $node_id ) );
            }
            elsif ($node_id eq 'alternatives'){
                $parent = $statement->append_child( MarpaX::Regex::AST->new( $node_id ) );
            }
            # child nodes; #text nodes will also be added here
            # so we don't care about <quantifier modifier>'s (yet)
            elsif (@children == 1 and not ref $children[0]){
                $parent->append_child( MarpaX::Regex::AST->new( $ast ) );
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

# set local Data::Dumper options as return Data::Dumper::Dumper($ast)
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
