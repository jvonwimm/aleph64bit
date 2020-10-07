#!/usr/local/bin/perl

$username = $ENV{USER};
# $scrfile = "/aleph/scweek/$username/sqlp.temp";
#$scrfile = "/tmp/sqlp.temp";

# Problems during scanbook installation on 2/5/2002
# A scratch file "/tmp/sqlp.temp" has been written by alsoft 
# (bookkeeping update) before this installation
# The perl program has given no error when trying to rewrite it
# So the alsoft file was used and it was not what we wanted.
# F.Loverre 7/5/2002
# To avoid later problems insert the user name in the file name
# but test also on the existence of the file.

$scrfile = "/tmp/" . $username . "_sqlp.temp";

if (-e $scrfile) {
    print "\n ** File $scrfile exists already - We will remove it \n";
    if (!unlink ($scrfile)) {
        print " ** ERROR !!! $scrfile cannot be deleted - EXIT **\n";
        exit;
    }
}

if($file = shift){
  compileproc($file);
}else{
  compileproc('jscanbook/plsql/scanbook_WWprocedures');
  compileproc('jscanbook/plsql/scanbook_procedures');
  compileproc('jscanbook/plsql/general_procedures');
}

sub compileproc{

  $file = shift;

  print "\n --> Compiling and loading $file .....\n";

  open CMD, ">$scrfile";
  print CMD '@'."$file\n";
  print CMD "quit\n";
  close CMD;

  my $database = 'pdb01';    
#  my $database = 'cerndb1';       # Changed for pdb01 (oracle9) 07-10-2003
#  my $database = 'maillogdb';     # Changed for cerndb1 24-09-2001
  ($usr, $passwd) = getpassw($database);
  $cmd = "sqlplus $usr/$passwd\@$database \@$scrfile |";

  if(open IN, $cmd){
    $nbok = 0;
    @reply = ();
    while(<IN>){
#       print "** $_\n";        # Debug option 
      push @reply, $_;
      if(/^No errors.$/){$nbok++}
    }
  }else{
      die " Could not start subprocess";
  }
  if ($nbok != 2) { 
     print @reply;
     die "Error compiling $file\n";
  }

  print "\n  ...... OK\n";
}

sub getpassw{
    my $machine = $_[0];
    my $login;
    my $passwd;
    my $phonenumber;
    
    my $netrc = $ENV{"HOME"}."/.netrc";
    if(open NETRC, $netrc){
        system("chmod 600 $netrc");
        while (<NETRC>){
           if(/^machine\s*$machine\s*login\s*(\w+)\s*password\s*(\w+).*$/){
	       ($login, $passwd) = ($1,$2);
              return ($login, $passwd);
           }
        }
        die "Could not find information on $machine in $netrc";
    }else{
        die "Could not open $netrc";
    }
}









