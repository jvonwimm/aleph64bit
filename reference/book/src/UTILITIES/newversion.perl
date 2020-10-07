#!/usr/local/bin/perl

$histfile = "versions.history";
$incfile = "version.h";
$javaversfile = "scanbook_version.java";
$sqlversfile = "scanbook_version.sql";
$current_version = "0000";

$now = scalar localtime;

if($argvers = shift){
  $current_version = $argvers;
  $current_version =~ s/^V//;
}elsif(-e $histfile){
  open HIST, $histfile;
  while (<HIST>){
    unless(/^#/){
      if(/^VERSION=/){
        $current_version = $_;
        $current_version =~ s/^VERSION=(.*?)\s.*$/$1/;
      }
    }
  }
  close HIST;
  $current_version++;
  $current_version = sprintf("%4d",$current_version);
  $current_version =~ s/ /0/g;
}

if(open HIST, ">>$histfile"){
  $str =  "VERSION=$current_version DATE=$now\n";
  print HIST $str;
  close HIST;
}else{
  die "Could not open history file  $histfile";
}

$current_str = "V$current_version";

if(open JAVAVERS, ">$javaversfile"){
  print JAVAVERS "public class scanbook_version{\n";
  print JAVAVERS '  public static String VERSION="'.$current_str.'";'."\n";
  print JAVAVERS "}\n";

  close JAVAVERS;
}else{
  die "Could not open javaversion file  $javaversfile";
}

if(open SQLVERS, ">$sqlversfile"){

  print SQLVERS "CREATE or REPLACE PACKAGE scanbook_version \n";
  print SQLVERS "IS \n";
  print SQLVERS "   FUNCTION give_ProgramsVersion RETURN VARCHAR2; \n";
  print SQLVERS "END scanbook_version; \n";
  print SQLVERS "/ \n";
  print SQLVERS "show errors \n";
  print SQLVERS "CREATE or REPLACE PACKAGE BODY scanbook_version \n";
  print SQLVERS "IS \n";
  print SQLVERS "FUNCTION give_ProgramsVersion \n";
  print SQLVERS "   RETURN VARCHAR2 \n";
  print SQLVERS "IS \n";
  print SQLVERS "BEGIN \n";
  print SQLVERS "   RETURN \'".$current_str.' | '. $now. "\' ; \n";
  print SQLVERS "END; \n";
  print SQLVERS "END scanbook_version; \n";
  print SQLVERS "/ \n";
  print SQLVERS "show errors \n";
  print SQLVERS "alter package general_procedures compile ; \n";

  close SQLVERS;

}else{
  die "Could not open sqlversion file  $sqlversfile";
}

if(open INC, ">$incfile"){
  print INC "VERSION= -r $current_str\n";
  close INC;

  system("cvs co jscanbook");
  chdir "jscanbook";
  system("cvs tag $current_str .");
  chdir "../";
  system("rm -r jscanbook");
  print "Version is now : $current_str\n";
}else{
  die "Could not open inc file  $incfile";
}



