#!perl 
use 5.006;
use strict;
use warnings FATAL => 'all';
use Test::More;
use iPlant::RFactory;
plan tests => 10;


ok( @{iPlant::RFactory::_load_package("data/Standard")} == 2, 'Standard directory');
ok( @{iPlant::RFactory::_load_package("data/Files")} == 2, 'Files drectory');
ok( @{iPlant::RFactory::_load_package("data/standard.tar.gz")} == 2, 'Standard tarball');
ok( @{iPlant::RFactory::_load_package("data/files.tar.gz")} == 2, 'Files tarball');
ok( @{iPlant::RFactory::_load_package("data/rfile.R")} == 1, 'R file');
ok( @{iPlant::RFactory::_load_package("data/rfiles.tar.gz")} == 3, 'Rfiles tarball');
ok( @{iPlant::RFactory::_load_package("data/Wrong")} == 0, 'Wrong directory should return ERROR');
ok( @{iPlant::RFactory::_load_package("data/Empty")} == 0, 'Empty directory should return ERROR');
ok( @{iPlant::RFactory::_load_package("data/wrong.tar.gz")} == 0, 'Wrong tarball should return ERROR');
ok( @{iPlant::RFactory::_load_package("data/empty.tar.gz")} == 0, 'Empty tarball should return ERROR');

