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
            croak "Child must be a ref to " . __PACKAGE__ unless ref $child eq __PACKAGE__;
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

sub last_child{
    my ($ast, $child) = @_;
    return $ast->child(-1, $child);
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
    _assert_options( $opts, { } );
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

#
# MarpaX::Regex-specific ast methods start here
#
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
        elsif ($ast->is_literal){
            $s .= qq{$children[0]};
        }

    };

    $ast->walk( $opts );

    return $s . "\n";
}

# merge statement nodes having the same lhs with alternation '|'
# into the first occurrence of such statement and remove other occurrences
sub merge{
    my ($ast) = @_;

#   gather all statements having the same lhs
    my %alternatives_by_lhs;
    my $opts = {
        visit => sub {
            my ($ast) = @_;
            if ($ast->id eq 'statement'){
                my $lhs          = $ast->descendant(2)->first_child();
                my $alternatives = $ast->child(1);
                push @{ $alternatives_by_lhs{ $lhs } }, $alternatives;
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
        for my $ma ( @{ $mergeable_alternatives{ $lhs } }){
#            warn Dumper $ma;
            push @merged, @{ $ma->children }, $alternation;
        }
        pop @merged; # remove traling $alternation
        $mergeable_alternatives{ $lhs } =
            MarpaX::Regex::AST->new( [ 'alternatives', @merged ] );
    }
#    warn $mergeable_alternatives{$_}->sprint for keys %mergeable_alternatives;

    # expand first occurrence of mergeable alternatives' lhs
    # with merged alternatives, mark all other occurrences for removal
    # by setting them to undef
    $opts = {
        visit => sub {
            my ($ast) = @_;
            if ($ast->id eq 'statement'){
                my $lhs = $ast->descendant(2)->first_child();
                if ( exists $mergeable_alternatives{ $lhs } ){
                    if (defined $mergeable_alternatives{ $lhs }){
#                        warn "# first occurrence of $lhs:\n", $ast->child(1)->sprint;
                        $ast->child( 1, $mergeable_alternatives{ $lhs } );
                        $mergeable_alternatives{ $lhs } = undef;
                    }
                    else{
                        # mark for removal
                        $ast->child( 1, MarpaX::Regex::AST->new( '#removable' ) );
                    }
                }
            }
        }
    }; ## opts
    $ast->walk( $opts );

    # remove statements whose alternatives have been just marked
    $ast->remove_children(sub{ $_[0]->child(1)->id eq '#removable' });

    return $ast;
}

# return symbols as a list or undef if there isn't any
sub symbols{
    my ($ast) = @_;

    my %symbol_name = map { $_ => 1 } ('bare name', 'bracketed name');
    my @symbols;
    my $opts = {
        skip => sub {
            my ($ast) = @_;
            # symbol is a statement ...
            return 1 unless $ast->id eq 'statement';
            # having no 'bare name' or 'bracketed name' alternatives
            my @symbols = grep { exists $symbol_name{$_->id()} }
                    @{ $ast->child(1)->children() };
            return @symbols > 0; # skip non-symbols
        },
        visit => sub {
            my ($ast) = @_;
            push @symbols, $ast;
        }
    }; ## opts
    $ast->walk( $opts );

    return @symbols > 0 ? \@symbols : undef;
}

# expand occurrences of symbol's name with symbol's alternatives
sub expand_symbols{
    my ($ast, $symbols) = @_;

    my %symbols;
    for my $symbol (@$symbols){
        my $symbol_lhs = $symbol->descendant(2)->first_child;
        my $symbol_alternatives = $symbol->child(1)->children;
#        warn Dumper $symbol_alternatives;
#        warn "# to be expanded: $symbol_lhs:\n", $symbol->sprint;
        $symbols{$symbol_lhs} = $symbol_alternatives;
    }

    # symbol must be removed if it's been used in expansion at least once
    my $removable_symbols = {};
    my $opts = {
        visit => sub {
            my ($ast) = @_;
            if ($ast->id eq 'statement'){
                my $lhs = $ast->descendant(2)->first_child();
#                warn "#stat $lhs:\n", $ast->dump;
                return if exists $symbols{ $lhs }; # don't expand itself
                my $alternatives = $ast->child(1)->children;
                # in reverse order to splice() from the end
                for (my $ix = @$alternatives - 1; $ix >= 0; $ix--){
                    my $alternative = $alternatives->[$ix];
                    my $id = $alternative->id();
#                    warn $id;
                    if ($id eq 'bare name' or $id eq 'bracketed name'){
                        my $symbol = $alternative->first_child();
                        if (exists $symbols{ $symbol } ){
#                            warn "# $ix-th alternative '$symbol' needs expanding:\n", $ast->[2]->[$ix+1]->sprint;
                            $ast->child(1)->child ( $ix, $symbols{ $symbol } );
                            $removable_symbols->{ $symbol }++;
                        }
                    }
                }
            }
        }
    }; ## opts
    $ast->walk( $opts );

#    warn "# after symbol expansion before removal:\n", $ast->dump;

    # remove symbols statements we've just expandd
    $ast->remove_children(sub{
        exists $removable_symbols->{ $_[0]->descendant(2)->first_child }
    });

#    warn "# After removal:\n", $ast->dump;

    return %$removable_symbols ? 1 : 0;
}

=pod

  coerce symbol names to REs
    Currently NAME is restricted to simple identifiers only.  In other words, it must match "/^[_A-Za-z][_A-Za-z0-9]*\z/" or its Unicode extension (see utf8), though it is not extended by the locale (see perllocale).

    remove angle brackets if $lhs is wrapped in them
    replace ' ' and - to _

=cut

sub regex_name{
    my ($lhs) = @_;
    $lhs =~ s/ |-/_/g;
    $lhs =~ s/^<|>$//g;
    return $lhs;
}

=head2

    find recursive statements and translate them to regex syntax

    A statement is recursive if one or more its alternatives contain symbols,
    which are the same as its lhs; recursive statement must become
    a named capture group (?<$lhs>...) and references to it in its alternatives
    must become (?&$lhs) recurses into that group.

=cut

sub recurse{
    my ($ast) = @_;

    my $opts = {
        visit => sub{
            my ($ast) = @_;
            if ($ast->id eq 'statement'){
#                warn "# recurse:\n", $ast->sprint;
                my $lhs = $ast->descendant(2)->first_child();
                my $alternatives = $ast->child(1);
                my $count = 0;
                # find recursions: references to $lhs in statement's alternatives
                for my $ix (0 .. $alternatives->children_count() - 1){
                    my $alternative = $alternatives->child($ix);
                    my $id = $alternative->id;
                    if ($id eq 'bare name' or $id eq 'bracketed name'){
                        if ( $alternative->first_child eq $lhs ){
                            warn "recursion: $lhs";
                            $lhs = regex_name ($lhs);
                            $alternatives->child($ix,
                                MarpaX::Regex::AST->new ( [ '#text', "(?&$lhs)" ] ) );
                            $count++;
                        }
                    }
                }
                warn $count;
                # recursions are found and references are prepared;
                # now, set up the named capture group to recurse to
                if ( $count > 0 ){
                    # if $alternatives is an unnamed capture group
                    if (
                            $alternatives->first_child->is_literal
                        and $alternatives->last_child->is_literal
                        and $alternatives->first_child->first_child eq '('
                        and $alternatives->last_child->first_child eq ')'
                    ){
                        warn "unnamed capture group";
                    }
                    # wrap $alternatives to a named capture group to recurse to
                    else{
                        warn "named capture group";
                    }
                # remove lhs
                $ast->remove_child(0);
                }
=pod

    for each alternative with $lhs as bare or bracketed name
        set it to [ '#text', "(?&$lhs)" ]
        $count++
    if $count > 0 such alternatives
        if $alternatives is an unnamed capture group child(0) is '(' and child(-1) is ')'
            replace first [ '#text', '(' ]
            with          [ '#text', '(?$lhs' ]
        else
            make alternatives a named capture group by wrapping them in
                [ '#text', '(?$lhs' ]
                ...
                [ '#text', ')' ]

        (or set it to [ '#text', '' ] )

=cut
            }
        }
    };
    $ast->walk($opts);

    return $ast;
}

# substitute symbol nodes contents instead of symbol name occurrencs
# and remove symbol nodes if they become inaccessible
sub substitute{
    my ($ast) = @_;

    $ast->merge();

#    warn "# with NO symbols expandd:\n", $ast->sprint;

    while (1){
        my $symbols = $ast->symbols();
#        warn $_->sprint for @$symbols;
        last if not $ast->expand_symbols($symbols);
#        warn "# with symbols expandd:\n", $ast->sprint;
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
            my ($ast) = @_;
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
            my ($ast) = @_;
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
            elsif ($ast->is_literal){
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
