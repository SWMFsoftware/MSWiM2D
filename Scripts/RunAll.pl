#!/usr/bin/perl -s

my $start_year = ($s or $start or 2000);
my $end_year = ($e or $end or 2001);
my $Help = ($h or 0);

push @INC, ".";

use strict;

&print_help if $Help;

my $gitclone = './BATSRUS/share/Scripts/gitclone -s';
my $rundir = './BATSRUS/run';
my $output = './Output';
my $input  = './Input';

# Compile BATSRUS and PIDL; make run directory
# add if statement to download BATSRUS if missing
print "Updating BATSRUS Config.pl...\n";
qx(cd ./BATSRUS; ./Config.pl -noopenmp -u=OuterHelio2d -e=Mhd -f -g=10,10,2);
print "Making BATSRUS and PIDL...\n";
qx(cd ./BATSRUS; make -j BATSRUS);
qx(cd ./BATSRUS; make PIDL);
if (-e $rundir and -d $rundir){
    print "Run directory already exists.\n";
}else{
    print "Creating OH run directory...\n";
    qx(cd ./BATSRUS; make rundir COMPONENT=OH);
}

foreach my $year ($start_year..$end_year) 
{
    print "Running $year...   ";

    # Select correct data files.
    my $StereoA = ($year >= 2007 and $year <= 2019);
    my $StereoB = ($year >= 2007 and $year <= 2014);
    
    # Unzip the data.
    qx(gunzip -c data/L1/l1_$year\.dat > $rundir/L1.dat);
    qx(gunzip -c data/STEREOA/STEREOA_$year\.dat > $rundir/STEREOA.dat) 
	if $StereoA;
    qx(gunzip -c data/STEREOB/STEREOB_$year\.dat > $rundir/STEREOB.dat) 
	if $StereoB;
    
    # Replace necessary text in PARAM.in file.
    open(my $in,  '<', "$input/PARAM.in") or die "Can't read old file: $!";
    open(my $out, '>', "$rundir/PARAM.in") or die "Can't write new file: $!";
    while( <$in> )
    {
	s/YYYY/$year/g;
	print $out $_;
      	if (/^ascii.*TypeFile$/){
	    print $out "
#LOOKUPTABLE
SW2           NameTable
load          NameCommand
STEREOA.dat   NameFile
ascii         TypeFile
" if $StereoA;
	    print $out "

#LOOKUPTABLE
SW3           NameTable
load          NameCommand
STEREOB.dat   NameFile
ascii         TypeFile
" if $StereoB;
	}
    }
    close $out;
    
    # Execute the code.
    qx(cd $rundir; mpiexec -n 2 ./BATSRUS.exe > runlog);

    # Process the results.
    qx(rm -rf $output/$year);
    qx(cd $rundir; ./PostProc.pl -M ../../$output/$year);

    # things will be removed by make clean and make cleanall

    print "complete.\n";
}
exit 0;



###############################################################################
sub print_help{
    print "
Options and description for MSWIM2D/Scripts/RunAll.pl

   Execute BATSRUS in stand-alone OH component for 2D outer heliosphere
   runs over the given time interval in yearly increments.

Usage:

   RunAll.pl [-h] [-s=YYYY] [-e=YYYY]
   
   -h -help
                        Display this help message.
   
   -s=YYYY -start=YYYY
                        Start year of the desired run.
   
   -e=YYYY -end=YYYY
                        End year of the desired run (inclusive).
   
Examples:

   RunAll.pl -h
          
         Display this help message.

   RunAll.pl -s=2003 -e=2007

         Complete the 2D outer heliosphere runs in annual increments from
         2003 until 2007, inclusive.

   RunAll.pl -start=2000 -end=2015

         Complete the 2D outer heliosphere runs in annual increments from
         2000 until 2015, inclusive.
\n";
    exit 0;   
}
