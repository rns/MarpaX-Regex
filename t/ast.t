use 5.010;
use strict;
use warnings;

use Test::More;

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Terse = 1;
$Data::Dumper::Deepcopy = 1;

use MarpaX::Regex;
use MarpaX::Regex::AST;

# following http://perldoc.perl.org/perlretut.html
# Regex BNF source, input string, scalar-context match, list-context match, desc
my $tests = [
    # Building a regexp
    [ q{
         s ::= ( nineteen | twenty | ) \d\d
         nineteen ::= '19'
         twenty   ::= '20'
      },
      'grouping, years'
    ],
    # /^[+-]?(\d+\.\d+|\d+\.|\.\d+|\d+)([eE][+-]?\d+)?$/
    [ q{
         number              ::= ^ (<optional sign>) (<f.p. mantissa> | integer) (<optional exponent>) $
         <optional sign>     ::= [+-]?
         <f.p. mantissa>     ::= digit+ '.' digit+  # mantissa of the form a.b
                               | digit+ '.'         # mantissa of the form a.
         <f.p. mantissa>     ::= '.' digit+         # mantissa of the form .b
         integer             ::= digit+             # integer of the form a
         <optional exponent> ::= ([eE][+-]?\d+)?
         digit               ::= \d
      },
      'building a regexp, unfactored form'
    ],
];

sub terminals{
    my ($ast) = @_;
    return ast_find(
        $ast,
        sub {
            my ($node_id, @children) = @{ $_[0] };
            if ($node_id eq 'statement'){
                for my $child (@children){
                    my $symbols = ast_find($child, sub { $_[0]->[0] eq 'symbol' } );
                    return 0 if @$symbols;
                }
                return 1;
            }
        }
    );
}

sub primaries{
    my ($ast) = @_;

    local $Data::Dumper::Indent = 0;

    return ast_find( $ast, sub {
        my ($node_id, @children) = @{ $_[0] };
        return 0 unless defined $node_id;
#        warn "# group \$children[0]:\n", Dumper $children[0];
        return 0 unless $_[0]->[0] eq 'group';
#        return 0 unless ref $children[0] eq "ARRAY";
#        return 0 unless ref $children[0]->[1] eq "ARRAY";
#        return 1 if $children[0]->[1] eq 'primary';
        return 0 unless ref $_[0]->[1] eq "ARRAY";
        return 1 if $_[0]->[1]->[0] eq 'primary';
        return 0;
    } );
}

sub inaccessible{
    my ($ast, $symbol) = @_;
    my $found = ast_find( $ast, sub {
#        warn $_[0]->[0];
        return 0 unless $_[0]->[0] eq 'symbol name';
        return $_[0]->[1]->[1] eq $symbol;
    } );
#    warn "$symbol: ", @$found;
    return @$found == 1;
}

sub delete_statement_by_lhs{
    my ($ast, $lhs) = @_;

    local $Data::Dumper::Indent = 0;
    my ($node_id, @children) = @{ $ast };
    # delete_child(sub{})
    # find index
    my $ix = undef;
    for my $i (0..@children-1){
        my $statement = $children[$i];
#        warn "# statement: ", Dumper $statement;
        my $lhs_i = $statement->[1]->[1]->[1]->[1]->[1];
#        warn Dumper $i, $lhs_i;
        $ix = $i if $lhs eq $lhs_i;
    }
#    warn "# statements: ", Dumper $ast;
#    warn "deleting statement ", Dumper $ast->[$ix+1];
    splice( @{ $ast }, $ix + 1, 1 ) if defined $ix;
}

sub substitute {
    my ($ast) = @_;

    local $Data::Dumper::Indent = 0;

SUBSTITUTE:
    for my $subst_iter (0..10){

        my $terminals = terminals($ast);
        last SUBSTITUTE unless @$terminals;
        my $substitutes = {};
        for my $t (@$terminals){
            warn "# terminal:\n", Dumper($t);
            my ($node_id, @children) = @$t;
            my $lhs = $children[0]->[1]->[1]->[1]->[1];
            # get terminal's alternatives group
            my $group = $children[0]->[2]->[1];
            # TODO: merge terminals with the same lhs
            # to a group joined with '|'
            push @{ $substitutes->{$lhs} }, [ $t, $group ];
        }
    #    warn "# substitutes:\n", Dumper $substitutes;

        my $primaries = primaries($ast);
        for my $p (@$primaries){
#            warn "# primary:\n", Dumper($p);
            next unless ref $p->[1]->[1] eq "ARRAY";
            next unless $p->[1]->[1]->[0] eq "symbol";
    #        warn Dumper $p->[1]->[1]->[1]->[1]->[1];

            my $symbol_name = $p->[1]->[1]->[1]->[1]->[1];
            next unless exists $substitutes->{$symbol_name};
            warn "\n# substitute $symbol_name group\n", Dumper($p);
            warn "before substitution:", Dumper $ast;

            for my $subst (@{ $substitutes->{$symbol_name} } ){
                my ($statement, $group) = @$subst;
                warn "with this group:\n  ", Dumper $group;
#                warn "and set up new terminal statement for the next cycle:\n  ",
#                    Dumper $statement,
#                    "if there are references to it from other rules (unlike <optional sign>, which should be deleted)";
                # this doesn't work for digit group
                $p->[1]->[1] = $group;
            }
    #        local $Data::Dumper::Indent = 1;
            warn "after substitution:", Dumper $ast;
            my $inaccessible = inaccessible($ast, $symbol_name);
            if ($inaccessible) {
                warn "$symbol_name is inaccessible, deleting";
                delete_statement_by_lhs($ast, $symbol_name);
            }
        }
    } ## for
    warn "after all substitutions:", Dumper $ast;
}

for my $test (@$tests){

    my ($source, $desc) = @$test;

    $source =~ s/^\s+|\s+$//g;
    diag "Regex BNF: $source";

    # must parse unambiguously unless parse error is expected
    my $rex = MarpaX::Regex->new;
    my $value = eval { $rex->parse($source) };
    ok !$@, 'Regex BNF parsed';

    my $ast = MarpaX::Regex::AST->new( $value );
    warn $ast->sprint();
    next;
#    warn $ast->dump( { indent => 1 } );
    $ast->walk( {
        visit => sub {
            my ($ast, $context) = @_;
            warn join ( ': ', $context->{depth}, $ast->dump( { indent => 0 } ) );
        },
        traversal => 'postorder',
    } );

} ## for my $test (@$tests) ...

done_testing();
