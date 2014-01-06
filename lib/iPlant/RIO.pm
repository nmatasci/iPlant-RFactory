package iPlant::RIO;

use 5.006;
use strict;
use warnings FATAL => 'all';
use JSON;

=head1 NAME

iPlant::RIO - The great new iPlant::RIO!

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

=head1 SYNOPSIS

Factory to generate R code blocks to handle input/output functions

=head1 EXPORT

A list of functions that can be exported.  You can delete this section
if you don't export anything, such as for a purely object-oriented module.


=head1 SUBROUTINES/METHODS

=head2 make_script
Generates a self contained R script
=cut


my$TABLE_DEFAULT=>{
	'header'=>'TRUE',
	'sep'=>'/t',
	'quote'=>'',
	'dec'=>'.',
	'row.names'=>'NULL',
	'na.string'=>'NA',
	'fill'=>'TRUE'
};


sub load_spec(){
	my$spec=shift;
	my@ins;
	my@outs;
	my$paras=from_json($spec)->{'parameters'};
	for my$para (@{$paras}){
		if(!$para->{'type'}){
			warn "Parameter ".$para->{'para'}." has no type definition\n";
			next;	
		}
		if($para->{'type'} eq 'input'){	
			my$input=build_input($para);
			push @ins,$input;
		}elsif($para->{'type'} eq 'output'){
			my$output=build_output($para);	
			push @outs,$output;
		}else{
			next;
		}	
	}
	return { 'inputs'=>\@ins,
			'outputs'=>\@outs,
	};
	
}

sub build_input(){
	my$para=shift;
	if(!$para->{'format'}){
		warn "Input parameter ".$para->{'para'}." has no format definition\n";
		next;	
	}
}

sub set_table() {
	
	
	
}
