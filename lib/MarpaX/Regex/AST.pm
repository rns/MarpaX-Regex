package MarpaX::Regex::AST;

use 5.010;
use strict;
use warnings;

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Terse = 1;
$Data::Dumper::Deepcopy = 1;

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

sub ast{ $_[0]->{ast} }

sub walk{
    my ($ast, $opts ) = @_;
    state $depth++;
    if (ref $ast){
        my ($node_id, @children) = @$ast;
        $opts->{visit}->( $ast, { depth => $depth } );
        walk( $_, $opts  ) for @children;
    }
    $depth--;
}

sub sprint{
    my ($ast, %opts ) = @_;
    my $s = '';
    $ast->walk( { } );
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
