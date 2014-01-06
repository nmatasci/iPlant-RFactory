#!perl 
use 5.006;
use strict;
use warnings FATAL => 'all';
use Test::More;

plan tests => 1;

BEGIN {
    use_ok( 'iPlant::RFactory' ) || print "Bail out!\n";
}

diag( "Testing iPlant::RFactory $iPlant::RFactory::VERSION, Perl $], $^X" );
