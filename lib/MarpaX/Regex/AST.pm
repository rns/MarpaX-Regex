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

#    warn Dumper $ast;
    my $self = $ast;

    # bless root
    bless $self, $class;
    # bless descendants
    $self->walk( {
        visit => sub { bless $_[0], $class unless blessed $_[0] }
    } );

    return $self;
}

# ast transform needs this
sub deepcopy{
    my ($ast) = @_;
    # absolutize relative references
    local $Data::Dumper::Purity = 1;
    local $Data::Dumper::Deepcopy = 1;
    $ast = eval(Dumper($ast));
    return $ast;
}

sub _assert_options{
    my ($opts, $spec) = @_;

    $spec //= {};
    my $meth = (caller(1))[3];

    $opts //= 'undef';
    croak $meth . ": options must be a HASH ref, not $opts." unless ref $opts eq "HASH";

    for my $opt ( sort keys %{ $spec } ){
        my ($pred, $desc) = @{ $spec->{$opt} };
        croak $meth . ": '$opt' option required." unless exists $opts->{visit};
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

    $opts->{depth}++;

    $ast = bless [ '#text', $ast ], __PACKAGE__ unless ref $ast;

    my ($node_id, @children) = @{ $ast };

    unless ($opts->{depth} > $opts->{max_depth} or exists $opts->{skip}->{$node_id} ){
        $opts->{visit}->( $ast, { depth => $opts->{depth} } );
    }

    unless (@children == 1 and not ref $children[0]){ # [ literal name, literal value ]
        do_walk( $_, $opts  ) for grep { defined } @children;
    }

    $opts->{depth}--;
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
