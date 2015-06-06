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

use parent 'MarpaX::AST';

sub concat{
    my ($ast, $opts ) = @_;

    my $s = '';

    $opts //= { };
    $ast->assert_options( $opts, { } );
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
            $s .= $children[0];
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

    find recursive statements and compile them to regex syntax

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
#                            warn "recursion: $lhs";
                            $lhs = regex_name ($lhs);
                            $alternatives->child($ix,
                                MarpaX::Regex::AST->new ( [ '#text', "(?&$lhs)" ] ) );
                            $count++;
                        }
                    }
                }
#                warn $count;
                # recursions are found and references are prepared;
                # now, set up the named capture group to recurse to
                if ( $count > 0 ){
                    # if $alternatives is an capture group
                    # (unnamed, we don't support named capture groups yet)
                    if (
                            $alternatives->first_child->is_literal
                        and $alternatives->last_child->is_literal
                        and $alternatives->first_child->first_child eq '('
                        and $alternatives->last_child->first_child eq ')'
                    ){
#                        warn "capture group";
                        $alternatives->first_child(
                            MarpaX::Regex::AST->new ( [ '#text', "(?<$lhs>" ] ) );
                    }
                    # wrap $alternatives to a named capture group to recurse to
                    else{
#                        warn "not a capture group";
                        # insert_before_child
                        $alternatives->insert_before_child ( 0,
                            MarpaX::Regex::AST->new ( [ '#text', "(?<$lhs>" ] ) );
                        $alternatives->append_child(
                            MarpaX::Regex::AST->new ( [ '#text', ")" ] ) );
                    }
                }
                # remove $lhs: pretty printing will be handled differently
                $ast->remove_child(0);
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

    return $ast->SUPER::distill({
        root => 'statements',
        skip => [
            'statements',
            'group', 'primary',
            'alternative rule',
            'symbol', 'symbol name',
            'character class', 'literal'
        ]
    });

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
