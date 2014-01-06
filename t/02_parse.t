#!perl 
use 5.006;
use strict;
use warnings FATAL => 'all';
use Test::More;
use iPlant::RFactory;
use JSON;
plan tests => 2;

my $json_test_file="data/bayesB.json";
open my$JTF, "<$json_test_file" or die "Cannot load json test file $json_test_file: $!\n";
my$json_string=join "",(<$JTF>);
close $JTF;
my$bayesb=from_json($json_string);

#ok( iPlant::RFactory::_parse_R("data/Bayes_C.R") == 0, 'Standard directory');
#ok( iPlant::RFactory::_make_flags($bayesb->{'parameters'}) == 0, 'Standard directory');
ok( iPlant::RFactory::make_script($json_string) eq '', 'Standard directory');


#ok( @{iPlant::RFactory::_load_package("data/Files")} == 2, 'Files drectory');
#ok( @{iPlant::RFactory::_load_package("data/standard.tar.gz")} == 2, 'Standard tarball');
#ok( @{iPlant::RFactory::_load_package("data/files.tar.gz")} == 2, 'Files tarball');
#ok( @{iPlant::RFactory::_load_package("data/rfile.R")} == 1, 'R file');
#ok( @{iPlant::RFactory::_load_package("data/rfiles.tar.gz")} == 3, 'Rfiles tarball');
#ok( @{iPlant::RFactory::_load_package("data/Wrong")} == 0, 'Wrong directory should return ERROR');
#ok( @{iPlant::RFactory::_load_package("data/Empty")} == 0, 'Empty directory should return ERROR');
#ok( @{iPlant::RFactory::_load_package("data/wrong.tar.gz")} == 0, 'Wrong tarball should return ERROR');
#ok( @{iPlant::RFactory::_load_package("data/empty.tar.gz")} == 0, 'Empty tarball should return ERROR');
#
