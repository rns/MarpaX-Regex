use 5.010;
use strict;
use warnings;

use Test::More;

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Terse = 1;
$Data::Dumper::Deepcopy = 1;

use Marpa::R2;

my $dsl = <<DSL;
:default ::= action => [ name, values ]
lexeme default = action => [ name, value ] latm => 1

    expr ::= literal

    literal ::= ( ["] ) <string without double quotes> ( ["] )
    literal ::= ( ['] ) <string without single quotes> ( ['] )
    <string without double quotes> ~ [^"]+ #"
    <string without single quotes> ~ [^']+ #'

:discard ~ whitespace
    whitespace ~ [ ]+

DSL

# following http://perldoc.perl.org/perlretut.html
my $tests = [
    # source, input, scalar context match, list context match, desc
    [ q{ 'hello' }, 'hello world', 1, [ 1 ], 'Simple word matching' ],
    [ q{ "hello" }, 'hello world', 1, [ 1 ], 'Simple word matching' ],
];

my $slg = Marpa::R2::Scanless::G->new( { source  => \$dsl } );

sub translate{
    my ($ast) = @_;
    state $depth++;
    my $s;
    my $indent = "  " x ($depth - 1);
    if (ref $ast){
        my ($node_id, @children) = @$ast;
        if ($node_id eq 'literal'){
#            warn Dumper $node_id, \@children;
            $s .= $children[0]->[1];
        }
        else{
            $s .= join '', map { translate( $_ ) } @children;
        }
    }
    else{
        $s .= $ast;
    }
    $depth--;
    return $s;
}

for my $test (@$tests){
    my ($source, $input, $expected_scalar, $expected_list, $desc) = @$test;
    my $slr = Marpa::R2::Scanless::R->new( {
        grammar => $slg,
#        trace_terminals => 99,
    } );
    eval { $slr->read(\$source) } || warn "$@\nProgress report is:\n" . $slr->show_progress;
    ok !$@, 'parsed';
    is $slr->ambiguity_metric(), 1, "parsed unambiguously";
    my $ast = ${ $slr->value() };
    my $re = translate($ast);
    ok $input =~ /$re/x, "$desc: match";
    my $got = $input =~ /$re/x;
    is $got, $expected_scalar, "$desc: return in scalar context";
    my @got = $input =~ /$re/x;
    is_deeply \@got, $expected_list, "$desc: return in list context";
}

done_testing();
