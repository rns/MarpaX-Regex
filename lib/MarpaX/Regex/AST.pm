package MarpaX::Regex::AST;

use 5.010;
use strict;
use warnings;

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Terse = 1;
$Data::Dumper::Deepcopy = 1;

use Carp;

sub new {
    my ($class, $ast) = @_;
    # absolutize relative references
    local $Data::Dumper::Purity = 0;
    local $Data::Dumper::Indent = 0;
    local $Data::Dumper::Terse  = 1;
    my $self = eval(Dumper($ast));
    # bless root
    bless $self, $class;
    # bless descendants
    $self->walk( { visit => sub { bless $_[0], $class } } );
    return $self;
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

    _assert_options($opts, { visit => [ sub{ ref $_[0] eq "CODE" }, "CODE ref" ] });
    $opts->{traversal} //= 'preorder';

    return do_walk( $ast, $opts );
}

sub do_walk{
    my ($ast, $opts ) = @_;
    state $depth++;
    if (ref $ast){
        my ($node_id, @children) = @$ast;
        if ($opts->{traversal} eq 'postorder'){
            do_walk( $_, $opts  ) for @children;
            $opts->{visit}->( $ast, { depth => $depth } );
        }
        else{
            $opts->{visit}->( $ast, { depth => $depth } );
            do_walk( $_, $opts  ) for @children;
        }
    }
    $depth--;
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

sub dump{
    my ($ast, $opts ) = @_;
    if (exists $opts->{indent}){
        local $Data::Dumper::Indent = $opts->{indent};
        return Dumper $ast;
    }
    return Dumper $ast;
}

sub ast_find{
    my ($ast, $code ) = @_;
    my $found = [];
    sub find{
        my ($node, $code, $found) = @_;
        if (ref $node){
            my ($node_id, @children) = @$node;
            if ( $code->($node) ){
                local $Data::Dumper::Indent = 0;
                push @$found, $node;
            }
            find ($_, $code, $found) for reverse @children;
        }
    }
    find($ast, $code, $found);
    return $found;
}

1;
