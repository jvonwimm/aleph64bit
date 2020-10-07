#!/usr/local/bin/perl
#
# This Perl program will:
#  - Read a file containing the bad runs defined by B.Bloch
#  - Via DBI access to Oracle databases devdb and pdb01 
#    under Brigitte Bloch accounts.
#    Fill the table WBADRUNS in the 2 Oracle databases.
#
#  - Execute a PL/SQL procedure to recreate the table WRUNSEL
#    on the Oracle databases devdb and pdb01  
#    from the updated table WBADRUNS.
#############################################################################
# Will use DBI to connect to Oracle databases
use DBI;

# Set the Oracle environment
&set_oracle_env;

# Store the full name and base name of this Perl program into local variables
$script_file = $0;
($script_name = $0) =~ s#.*/(.*)$#$1#;

#*********************************************************************************************
# If no parameter given a default input file name is used
$input_file = "/afs/cern.ch/user/b/bbloch/ww2k/wbadrun.list";
#
$net_file = $ENV{"HOME"}."/.netrc";
# 
#*********************************************************************************************

# Store the date and time into a local variable
$date_start = " Begin $script_file on : " .`date`;

# Store the table name
$table = "WBADRUNS";

# Store the Oracle databases in an array
@ora_dbase = ("devdb" , "pdb01");

# Verify if an argument has been given
if ($#ARGV ge 0) { 
    $input_file = $ARGV[0];
}

# Open the input file
&OPEN_FILE("WLIST",$input_file,"read");
 
# List the file and ask for a confirmation to continue
print "\n";
print " List of your file $input_file \n";
print "\n";

$nline = 0;

while (<WLIST>) {
    $line = $_;
    print $line;

##    next if(/^\s*\n/);          # Skip empty lines
    if (/^\s*(\d+)\s+(\d+)\s+(\d+)\s+(.*?)\s*$/) {   # Lines of no interest 
        $nline++;
    }
}
if ($nline eq 0) {
    print " No Runs appear in your file - If your table $table is not empty all rows will be deleted \n";
}

# Close the input file
close WLIST;

print "\n";
print " Can we GO ? (answer <yes> or <no> please) "; 
$answer = <STDIN>;
chomp($answer);
$answer =~ tr/A-Z/a-z/;      
if ($answer ne 'yes' ) {
    print " You answer $answer -   We ABEND \n";
    &ABEND;
} 

print " \n";

$nline = 0;
$nerr = 0;

# Open the input file
&OPEN_FILE("WLIST",$input_file,"read");

while (<WLIST>) {
    next if(/^\s*\n/);          # Skip empty lines
    $line = $_;
    $nline++;

    if (/^\s*(\d+)\s+(\d+)\s+(\d+)\s+(.*?)\s*$/) { 

        $firstrun = $1;
        $lastrun = $2;
        $energy = $3;
        $comment = "'" . $4 . "'"; 
    
        $sql_line = "INSERT into $table values ( $firstrun , $lastrun , $energy , $comment )\n";
        push(@save_line,$sql_line);
    } else {
        $nerr++;
        print " \n*** ERROR found in $input_file on line $nline \n";
        print $line;
        next;

    }
}
# Close the input file
close WLIST;

if ($nerr > 0) { &ABEND; } 

# Retrieve the necessary parameters to open the Oracle databases
foreach $database (@ora_dbase) {
    ($user, $passwd) = getpassw($database);

# Connects to database - If cannot connect abend
    $dbh = db_open($user,$passwd,$database);

# delete rows on table WBADRUNS before inserting
    $dbh->do('truncate table WBADRUNS');

# Loop on list of sql buffer - For each line do an insert
    foreach $savlin (@save_line) {
        eval {                                      # eval to obtain the return code
            &insert($dbh,$savlin);
        };
        if ($@) {                                   # something failed
            print "\n *** Error inserting line : \n $savlin \n";
            db_close($dbh);
            die $@;
        }
        else {
            $dbh->commit;
        }
    }

# Execute now the procedure which will write the table WRUNSEL from
# the table WBADRUNS just written above
    exec_proc($database);

    db_close($dbh);

# Close the .netrc file
    close NETFIL;
}


print "\n";
print " ---- $script_name ended normally ---- \n";
print "\n";

exit;


sub getpassw{
    my $machine = $_[0];
    my $login;
    my $passwd;

#   Open the .netrc file
    &OPEN_FILE("NETFIL",$net_file,"read"); 

    @stat = stat ($net_file);
    $mode = $stat[2];
    $perm = $mode & 07777;
    if ($perm != 0600) {
        print " *** File $net_file has not the correct permissions - Abend \n";
        close NETFIL;
        ABEND;
    }

    while (<NETFIL>) {
        $line = $_;
##        if (/^oracle\s+(.*?)\s+login\s+(.*?)\s+password\s+(.*?)\s*$/) {
        if (/^oracle\s+$machine\s+login\s+(.*?)\s+password\s+(.*?)\s*$/) {
            ($login, $passwd) = ($1, $2);
            return ($login, $passwd); 
        }
    }
    die " *** Could not find information on $machine in $net_file \n"; 
}


sub insert {
    my($dbh,$sql) = @_;
    $sth = $dbh->prepare($sql) or die "Can't prepare statement ($sql)";
    $sth_resul = $sth->execute() or die "Can't execute statement";
}


sub exec_proc{
    my $dbase = $_[0];

    if ($dbase eq 'devdb') {
        $csr = $dbh->prepare(q{
        BEGIN
            scanbook.scanbook_WWprocedures.Update_WRUNSEL;
        END;
        });
    } else {
        $csr = $dbh->prepare(q{
        BEGIN
            prod_scanbook.scanbook_WWprocedures.Update_WRUNSEL;
        END;
        });
    }

    $csr->execute;
    if(!defined $csr){
       die "Execution failed : $DBI::errstr\n";
    }
}


sub set_oracle_env {
    $ENV{"ORACLE_HOME"} = '/afs/cern.ch/project/oracle/@sys/prod';
    $ENV{"TNS_ADMIN"} = '/afs/cern.ch/project/oracle/admin';
    $ENV{"LD_LIBRARY_PATH"} = $ENV{"ORACLE_HOME"}."/lib";
    1;
}


sub db_open {
    my($user,$passwd,$database) = @_;
    my($tmp_dbh);
    $tmp_dbh = DBI->connect("dbi:Oracle:".$database, $user, $passwd, 
                        {
                           PrintError => 1,
                           AutoCommit => 0,
                           RaiseError => 1
                        } 
                    ) or die " *** Cannot connect to $database";
    return $tmp_dbh;
}


sub db_close {
    my($dbh) = @_;

    # As Oracle automatically commits any outstanding changes
    # at disconnect time, we ropllback any uncommited transaction
    $dbh->rollback;  
    $dbh->disconnect;
}


sub OPEN_FILE {
# Open a file in read or write or append mode  
# If an error occurs -> abend 
#
my($fhandle,$filname,$mode)= @_;
    if ($mode eq "read") {
        $open_way = $filname;
    }
    elsif ($mode eq "write") {
        $open_way = "> $filname";
    }
    elsif ($mode eq "append") {
        $open_way = ">> $filname";
    }
    if (!open($fhandle,$open_way)) {
       print "       ***** Could not open file $input_file in $mode mode - ABEND \n";
       &ABEND;
    }
}


sub ABEND {
# Abnormal end
    print " \n";
    print " ---- Abend ---- \n";
    exit;
}













