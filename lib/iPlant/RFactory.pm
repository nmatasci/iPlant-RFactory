package iPlant::RFactory;

use 5.006;
use strict;
use warnings FATAL => 'all';
use File::Temp qw/tempdir/;
use Archive::Tar;
use Text::Balanced qw/extract_bracketed/;
use JSON;

#TODO: need to handle multi value params.

=head1 NAME

iPlant::RFactory - The great new iPlant::RFactory!

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

=head1 SYNOPSIS

Factory to generate R code blocks to expose package functions

=head1 EXPORT

A list of functions that can be exported.  You can delete this section
if you don't export anything, such as for a purely object-oriented module.


=head1 SUBROUTINES/METHODS

=head2 make_script
Generates a self contained R script
=cut

sub make_script {
	my $json_string = shift;
	my $json_R      = from_json($json_string);

	my $shebang =
'#!/bin/env Rscript --vanilla --default-packages="datasets, utils, grDevices, graphics, stats,methods"';

	my $note =
'#This script has been generated automagically using iPlant Collaborative RFactory service version '
	  . $VERSION . '
#from '
	  . $json_R->{'origin'}
	  . ' written by '
	  . $json_R->{'author'}
	  . '. Please contact the original author for any questions.
#This script is released under the same copyright terms as '
	  . $json_R->{'origin'} . '
#More information are available at http://github.com/iPlantCollaborativeOpenSource/RFactory';

	my $libs = join ",", @{ $json_R->{'dependencies'} };
	$libs = "\nlibrary(getopt,$libs)\n";

	my $options = _make_opt($json_R);

	my $function;
	if ( !$json_R->{'package'} ) {
		$function =
		    $json_R->{'name'}
		  . '<-function'
		  . $json_R->{'call'}
		  . $json_R->{'fun'};
	}
	else {
		$function = 'library("' . $json_R->{'package'} . '")';
	}

 #	if(!$json_R->{'format'} || lc($json_R->{'format'}) eq 'script' ){
 #		$function=$json_R->{'name'}.'<-function'.$json_R->{'call'}.$json_R->{'fun'};
 #	} elsif (lc($json_R->{'format'}) eq 'file'){
 #		$function='source("'.$json_R->{'origin'}.'")';
 #	} elsif (lc($json_R->{'format'}) eq 'file'){
 #		$function='library("'.$json_R->{'origin'}.'")';
 #	} else{
 #		$json_R->{'name'}.'<-function'.$json_R->{'call'}.$json_R->{'fun'};
 #	}

	my $call = $json_R->{'name'} . $json_R->{'call'};

	my $script = join "\n",
	  ( $shebang, $note, $libs, $options, $function, $call );

	print "$script\n";

}

=head2 _make_opt
Creates the R Getopt and help block based on a parameter json
=cut

sub _make_opt {
	my $json_R = shift;
	my @spec;
	my @defaults;
	my $block;

	my %TYPEOF = (
		'integer'   => 'integer',
		'double'    => 'double',
		'numeric'   => 'double',
		'string'    => 'character',
		'character' => 'character',
		'boolean'   => 'logical',
		'logical'   => 'logical',
		'input'     => 'input'
	);
	print Dumper($json_R);
	#TODO:should eval
	my @params = @{ _make_flags( $json_R->{'parameters'} ) };
	if ( !grep /help/i, @params ) {
		my $help = {
			'para'        => 'help',
			'flag'        => 'h',
			'type'        => 'logical',
			'description' => "Print this help message"
		};
		unshift @params, $help;
	}
	my @options;
	my @descriptions;
	for (@params) {

		my $para     = $_->{'para'};
		my $flag     = $_->{'flag'};
		my $type     = 'character';
		my $required = $_->{'value'} ? 2 : 1;

		if ( $_->{'type'} && $TYPEOF{ lc( $_->{'type'} ) } ) {
			$type = $TYPEOF{ lc( $_->{'type'} ) };
			if ( $type eq 'logical' ) {
				$required = 0;
			}
		}

		if ($required) {
			my $arg;
			if ( $_->{'arg'} ) {

				$arg = $_->{'arg'};
				$arg =~ s/\s+/_/g;
			}
			else {
				$arg = "<$type>";
			}
			if ( $required == 2 ) {
				push @options, "[--$para $arg]";
			}
			else {
				push @options, "--$para $arg";
			}

		}

		#TODO: model input

		my $desc = "\t--$para, -$flag\t" . $_->{'description'};
		if ( defined( $_->{'value'} ) ) {
			$desc .= "(defaults to " . $_->{'value'} . ")";
		}
		push @descriptions, $desc;

		if ( defined( $_->{'value'} ) ) {
			push @defaults, "
if( is.null(opt\$$para)) {
	$para <- " . $_->{'value'} . "
} else {
	$para <- opt\$$para	
}";

		}
		else {
			push @defaults, "$para <- opt\$$para";
		}

		push @spec,
		  "\'$para\',\'$flag\',$required,\'$type\'";    #TODO: model input

	}
	shift @defaults;                                    #removing help
	my $spec = "spec<-matrix(c(\n";
	$spec .= join "\n", @spec;
	$spec .= "\n),byrow=TRUE,ncol=4)\n\nopt<-getopt(spec)\n";

	my $help_text =
	    "Usage: RScript "
	  . $json_R->{'name'} . ".R "
	  . ( join ' ', @options ) . "\n";
	$help_text .= join "\n", @descriptions;

	my $help_block = '
if( ! is.null(opt$help) ){
	cat(\'' . $help_text . '\')
	q("no")	
}';

	$block = join "\n", ( $spec, $help_block, @defaults );

	return $block;
}

sub _make_flags {
	my $params = shift;
	my @alpha = ( 'a' .. 'g', 'i' .. 'z', 'A' .. 'Z' );
	my %alpha;

	for ( my $i = 0 ; $i < @alpha ; $i++ ) {
		$alpha{ $alpha[$i] } = $i;
	}

	my @duplicates;

	my $i = 0;
	for ( @{$params} ) {

		my $flag = lc( substr( $_->{'para'}, 0, 1 ) );
		if ( !$alpha{$flag} ) {
			if ( $alpha{ uc($flag) } ) {
				delete $alpha{ uc($flag) };
				$_->{'flag'} = uc($flag);
			}
			else {
				push @duplicates, $i;
			}
		}
		else {
			delete $alpha{$flag};
			$_->{'flag'} = $flag;
		}
		$i++;
	}
	my @free = keys %alpha;
	for (@duplicates) {
		my $flag = shift @free;
		$params->[$_]->{'flag'} = $flag;
	}

	return $params;
}

=head2 _load_package

Identify and loads all the files that belong to an R package

=cut

use Exporter;
our @ISA = qw(Exporter);

use Cwd;
use Data::Dumper;

sub _load_package {
	my $package_loc = shift;
	my $format      = 'script';
	my @rfiles;
	if ( !-e $package_loc ) {
		warn "$package_loc: No such file or directory.\n";
	}
	if ( $package_loc =~ m/\.tar(?:\.gz)?$/i ) {

		#unzip
		my $tar = Archive::Tar->new($package_loc);

		my $here = cwd();
		my $dir = tempdir( CLEANUP => 0 );
		chdir $dir;

		my @out = $tar->extract();

		chdir $here;

		opendir my $TDIR, $dir
		  or die "Cannot read temporary directory $dir: $!\n";
		my @arc_content = grep !/^\.+/, readdir $TDIR;
		close $TDIR;
		if ( !@arc_content ) {
			warn
"ERROR: $package_loc does not seem to be a valid R package or file\n";
		}
		elsif ( @arc_content == 1 && -d "$dir/$arc_content[0]/" ) {

			$package_loc = "$dir/$arc_content[0]";
		}
		else {
			$package_loc = $dir;
		}

	}

	if ( -d $package_loc ) {
		opendir my $PDIR, $package_loc
		  or die "Cannot open the R package directory $package_loc: $!\n";
		my @arc_content = grep !/^\.+/, readdir $PDIR;
		close $PDIR;
		my @rdir = grep /^R$/, @arc_content;
		my $rdir = @rdir ? $rdir[0] : '';
		if ( $rdir && -d "$package_loc/$rdir" )
		{    #it's a well structired package

			#TODO:Read info from DESCRIPTION file
			opendir my $RDIR, "$package_loc/$rdir"
			  or die
			  "Cannot open the R package directory $package_loc/$rdir: $!\n";
			@rfiles = grep /\.[RSqrs]$/, readdir $RDIR;
			close $RDIR;
			for ( my $i = 0 ; $i < @rfiles ; $i++ ) {
				$rfiles[$i] = "$package_loc/R/$rfiles[$i]";
			}

		}
		else {
			@rfiles = grep /\.[RSqrs]$/, @arc_content;
			for ( my $i = 0 ; $i < @rfiles ; $i++ ) {
				$rfiles[$i] = "$package_loc/$rfiles[$i]";
			}
		}

	}
	elsif ( $package_loc =~ m/\.[RSqrs]$/ ) {    #it's a single R file
		push @rfiles, $package_loc;
	}

	if ( !@rfiles ) {
		warn
		  "ERROR: $package_loc does not seem to be a valid R package or file\n";
	}
	return \@rfiles;
}

=head2 _parse_R

=cut

sub _parse_R {
	my @functions;

	#load file
	my $in_file = shift;
	my %r_content;
	open my $RFILE, "<$in_file" or die "Cannot load input R file: $!\n";
	my $r_file = join "", grep !/^\s*#.*$/, (<$RFILE>);
	close $RFILE;

	$r_file =~ s/(?:\s*#.*$)//gm;    #|(?:^\s+)
	$r_file =~ s/\s*\n+/\n/g;

	my @libs;

	#identify libraries
	while ( $r_file =~ s/library\(((?:\w,?)+)\)//gx ) {
		push @libs, split /,/, $1;
	}

	#deduplicate
	my %libs;
	for (@libs) {
		$libs{$_} = 1;
	}

	#identify functions
	my $remainder = $r_file;
	while ( $remainder =~ m/(\w+)\s*(?:=|<-)\s*function/xg ) {
		my %fun;
		$fun{'name'} = $1;
		( my $call, $remainder ) = extract_bracketed( $remainder, '(' );
		$fun{'call'} = $call;
		$call =~ s/(?:,?\s*\w*\s*\()/,/xg;
		$call =~ s/\)|\s+//g;

		#		$call=~ s/(\w+)\s*=\s*(\w+)\s*\((.*)\)/
		my @para = grep !/^\s*$/, split /,/, $call;
		$fun{'para'} = _extract_para( \@para );

		( my $fun, $remainder ) = extract_bracketed( $remainder, '{' );

		#				$fun = '{OK...}';
		$fun{'fun'} = $fun;
		push @functions, \%fun;
		$fun{'dependencies'} = [ keys %libs ];
		$fun{'origin'}       = $in_file;
	}
	print to_json( \@functions );
	return \@functions;
}

=head2 _extract_para

=cut

sub _extract_para {

	#extract parameters
	my @para_list = @{ shift() };
	my @para_objs;
	for (@para_list) {
		my %para_obj;
		my ( $para, $def ) = split /=/, $_;
		%para_obj = (
			'para'        => $para,
			'description' => '',
			'type'        => '',
			'arg'         => '',
		);

		if ($def) {
			$para_obj{'value'} = $def;
			if ( $def =~ /^"(\S+)"$/ ) {
				$para_obj{'type'} = 'string';
			}
			elsif ( $def =~ /^(?:(?:TRUE)|(?:FALSE)|T|F)$/ ) {
				$para_obj{'type'} = 'logical';
			}
			elsif ( $def =~ /^(\d+)$/ ) {
				$para_obj{'type'} = 'integer';
			}
			elsif ( $def =~ /^(\d*.\d+)$/ ) {
				$para_obj{'type'} = 'double';
			}
		}

		push @para_objs, \%para_obj;
	}
	return \@para_objs;
}

=head1 AUTHOR

Naim Matasci, C<< <nmatasci at iplantcollaborative.org> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-iplant-rfactory at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=iPlant-RFactory>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.




=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc iPlant::RFactory


You can also look for information at:

=over 4

=item * RT: CPAN's request tracker (report bugs here)

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=iPlant-RFactory>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/iPlant-RFactory>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/iPlant-RFactory>

=item * Search CPAN

L<http://search.cpan.org/dist/iPlant-RFactory/>

=back


=head1 ACKNOWLEDGEMENTS


=head1 LICENSE AND COPYRIGHT

Copyright 2013 Naim Matasci.

This program is free software; you can redistribute it and/or modify it
under the terms of the the Artistic License (2.0). You may obtain a
copy of the full license at:

L<http://www.perlfoundation.org/artistic_license_2_0>

Any use, modification, and distribution of the Standard or Modified
Versions is governed by this Artistic License. By using, modifying or
distributing the Package, you accept this license. Do not use, modify,
or distribute the Package, if you do not accept this license.

If your Modified Version has been derived from a Modified Version made
by someone other than you, you are nevertheless required to ensure that
your Modified Version complies with the requirements of this license.

This license does not grant you the right to use any trademark, service
mark, tradename, or logo of the Copyright Holder.

This license includes the non-exclusive, worldwide, free-of-charge
patent license to make, have made, use, offer to sell, sell, import and
otherwise transfer the Package with respect to any patent claims
licensable by the Copyright Holder that are necessarily infringed by the
Package. If you institute patent litigation (including a cross-claim or
counterclaim) against any party alleging that the Package constitutes
direct or contributory patent infringement, then this Artistic License
to you shall terminate on the date that such litigation is filed.

Disclaimer of Warranty: THE PACKAGE IS PROVIDED BY THE COPYRIGHT HOLDER
AND CONTRIBUTORS "AS IS' AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.
THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
PURPOSE, OR NON-INFRINGEMENT ARE DISCLAIMED TO THE EXTENT PERMITTED BY
YOUR LOCAL LAW. UNLESS REQUIRED BY LAW, NO COPYRIGHT HOLDER OR
CONTRIBUTOR WILL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, OR
CONSEQUENTIAL DAMAGES ARISING IN ANY WAY OUT OF THE USE OF THE PACKAGE,
EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


=cut

1;    # End of iPlant::RFactory
