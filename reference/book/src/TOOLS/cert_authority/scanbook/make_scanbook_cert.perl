#!/usr/local/bin/perl

$homedir = $ENV{HOME};
$SCWEEK = $ENV{SCWEEK};
# The request should have been built by the web page :
# http://alephwww.cern.ch/~bonissen/cert_authority/getcert.html
# via the cgi :
# http://alephwww.cern.ch:8080/bonissen-bin/cert-request.cgi


$req_file    = "$SCWEEK/scanbook.req";

system("rfcp alephwww:/tmp/scanbook.req afal58:$req_file");

$CONFIG      = "openssl.cnf";
$CA          = "../openssl/bin/openssl ca ";
$newcert_file = "./scanbookcert.pem";
$CAcert      = "../authority/ALEPHcert.pem";
$CAkey       = "../authority/private/ALEPHkey.pem ";
$validity    = 360 * 5;                                   # validity 5 years

my $cmd      = "$CA -config $CONFIG -spkac $req_file ".
               " -keyfile $CAkey -cert $CAcert -days $validity ".
               " -outdir ./ -out $newcert_file ";

print "cmd = $cmd\n";

# Do some cleanup and initial settings before we start ...

system("rm $newcert_file");
system("rm index.txt");
system("touch index.txt");
system("cp ../openssl/reference/serial .");
system($cmd);

$dest = "\~/WWW/scanbook/scanbook.cacert";
print "I can now install the certificate in  $dest\n";

do {
  print "Install ? y/n ";
  $input = <STDIN>;
  $input =~ s/Y/y/;
}while($input =~ /^$/);

$dest =~ s/\~/$homedir/;

print "Installing the Scanbook certificate in $dest \n";

if($input =~ /^y/){
   system("cp $newcert_file $dest");
   print "Certificate was installed\n";
}

exit(0);








# $alephcert    = " ALEPHcert.pem";

# Make the Aleph root certificate.
# All useful informations are in the openssl.cnf file, in the same directory

$request_cmd  = "../openssl/bin/openssl req ";
$request_args = " -x509 -config openssl.cnf ".
                " -days 3650 -newkey rsa:512 ";
$alephcert    = " ALEPHcert.pem";
$alephkey     = " private/ALEPHkey.pem";
$outfiles     = " -keyout $alephkey -out $alephcert";

$string = $request_cmd.$request_args.$outfiles;

system($string);

print "Do you want to verify ? y/n [y]";

$input = <STDIN>;
$input =~ s/Y/y/;
$input =~ s/^$/y/;
if($input =~ /^y/){
   print "verifying\n";
}

system("../openssl/bin/openssl x509 -in $alephcert -text");

$dest   = "\~/www/cert_authority/ALEPHcert.cacert";




