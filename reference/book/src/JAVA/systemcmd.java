import java.io.*;
import java.lang.String;
import java.util.Vector;
import netscape.security.PrivilegeManager;
import java.security.*;

public class systemcmd {  
      static Vector output = new Vector();
public static void execute(String command, boolean waitOutput){
      Runtime r = Runtime.getRuntime();    
      String thisLine;
      //      System.out.println("command = "+command);
      scanbook.dbg.out("command = "+command+"****");
      try{
        if(scanbook.netscaperun){
          try{
            PrivilegeManager.enablePrivilege("UniversalExecAccess");
          }
          catch (Exception exc) {
            System.err.println(exc);
            return;
          }
        }
        Process p = r.exec(command);
	if(waitOutput){
          DataOutputStream ostr = new DataOutputStream(p.getOutputStream());
          p.waitFor();
          output.removeAllElements();
          BufferedReader theRdr = new BufferedReader (
               new InputStreamReader(p.getInputStream()));
          while ((thisLine = theRdr.readLine()) != null) {
	    //   System.out.println(thisLine);
            scanbook.dbg.out(thisLine);
            output.addElement(thisLine);
          }
          theRdr = new BufferedReader (
               new InputStreamReader(p.getErrorStream()));
          while ((thisLine = theRdr.readLine()) != null) {
	    //   System.out.println(thisLine);
            scanbook.dbg.out(thisLine);
            output.addElement(thisLine);
          }
    	}
        scanbook.dbg.out(" *** Done ");
      }
      catch (Exception exc) {
        System.err.println(exc);
      }
}
public static void printResult(){
    int n = output.size();
    for(int i=0; i<n; i++){
       System.out.println((String)output.elementAt(i));
    }       
}
public static boolean inOutput(String s){
    int n = output.size();
    for(int i=0; i<n; i++){
	if(((String)output.elementAt(i)).indexOf(s) != -1){
          return true;
	}
    }       
    return false;
}
}

    




