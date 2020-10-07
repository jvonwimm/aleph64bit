import java.io.*;         
import netscape.security.PrivilegeManager;
import java.util.Calendar;

public class debugger{
    FileWriter dbgfile;
    int level = 1;
    static String dbgFileName;
  public void out(String s){
    if(level > 0){
	System.out.println(s);
    }else{
	//      dbgFileName = scanbook.homedir+File.separator+"scanbook.dbg";
      setFileName();
      if(dbgFileName != null){
        try{
          if(scanbook.netscaperun){
            PrivilegeManager.enablePrivilege("UniversalFileRead");  
            PrivilegeManager.enablePrivilege("UniversalFileWrite");  
	  }
          File dbg = new File(dbgFileName);
          boolean fileExists = dbg.exists();
          dbgfile = new FileWriter(dbgFileName,fileExists);
          dbgfile.write(s+"\n", 0, s.length()+1);
          dbgfile.flush();        
          dbgfile = null;
        }catch (Exception exc) {
          System.err.println(exc);
        }
      }
    }
  }
  public void setlevel(int lvl){
    level = lvl;
    setFileName();
    if(level == 0 && dbgFileName != null){
      try{  
        if(scanbook.netscaperun){
          PrivilegeManager.enablePrivilege("UniversalFileRead");  
          PrivilegeManager.enablePrivilege("UniversalFileWrite");  
	}
        Calendar workCal = Calendar.getInstance();
        String now = workCal.getTime().toString();
        dbgfile = new FileWriter(dbgFileName,false);
        dbgfile.write("*\n*\n");
        dbgfile.write("* Scanbook debugging started on "+now + "\n");
        dbgfile.write("*\n*\n");
        dbgfile.flush();        
        dbgfile = null;
      }catch (Exception exc) {
        System.err.println(exc);
      }
    }
  }
  void setFileName(){
      if(scanbook.homedir != null){
        dbgFileName = scanbook.homedir+File.separator+"scanbook.dbg";
      }
  }
}







