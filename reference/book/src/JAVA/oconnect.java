import java.net.*;               // Network
import java.io.*;         
import java.applet.*;               // Applets 
import java.awt.*;               // ScrollPane, PopupMenu, MenuShortcut, etc..
import java.awt.event.*;         // New event model.
import java.util.*;              // Utilities.
import netscape.security.PrivilegeManager;
public class  oconnect{
  static Frame currentFrame;
  URL u;
  Vector allLines = new Vector(10,1);
  String username, password;
  Enumeration lines;
  String head = "";
  String headsql = "";
  static String pckg = "general_procedures";
//static String oraweb = "http://edmsoraweb.cern.ch:8001/prod_scanbook/owa/";
//static String oraweb = "http://edmsoraweb.cern.ch:8001/prod_scanbook8/owa/";
//static String orawebDev = "http://edmsoraweb.cern.ch:8001/scanbook/owa/";

  static String oraweb = "http://oraweb03.cern.ch:9000/pls/prod_scanbook/";
  static String orawebDev = "http://oraweb03.cern.ch:9000/pls/scanbook/";

  InputStream istr;
  oconnect(String proc){
     if(scanbook.netscaperun){
       try{
	   PrivilegeManager.enablePrivilege("UniversalConnect");
       }
       catch (Exception exc) {
          System.err.println(exc);
       }
     }
     try {
       String strurl = oraweb+pckg+".general";
       if(proc.equals("ImpatientStream")){
        //strurl = "http://alephwww.cern.ch:8080/bonissen-bin/filicards.cgi";
        //strurl="http://alephwww.cern.ch:8080/bonissen-bin/filicards_nrj.cgi";
         strurl="http://alephwww.cern.ch/bonissen-bin/filicards_nrj.cgi";
         u = new URL(strurl);
         scanbook.dbg.out("\nAccess Impatient Stream");
       }else{

  //Progress bar
	   //           pgf = new progressFrame("Transferring data");
	   //           pgf.setValue(0.);

         u = new URL(strurl);
         scanbook.dbg.out("\nexec "+pckg+".general(");
         put_value("procname",proc,false);
	 put_value("user_name",scanbook.userName,false);
         put_value("os_name",scanbook.osName,false);
         put_value("navigator",scanbook.Navigator,false);
         put_value("hardware",scanbook.osArch,false);
         put_value("ip_address",scanbook.ipAddress,false);
	 put_value("ProgVers",scanbook_version.VERSION,false);
       }
     }
     catch (Exception exc) {
        System.err.println("Connect -> "+exc);
	errorMessage msg = new errorMessage(currentFrame,"");
        msg.newline(" Cannot connect to the Oracle web server");
        msg.newline(" Reason : " + exc);
        msg.newline(" Most likely : Network error");
        msg.newline(" Try later");
        msg.display();
      }
  }
    //    void testException() throws Exception{
    //	throw new Exception("Test exception");
    //    }
  public static void setoraweb(String p){
     oraweb = p;
  }
  public static void setPackage(String p){
     pckg = p;
  }
  public static void setCurrentFrame(Frame f){
    currentFrame = f;
  }  
  boolean put_value(){
     return put_value(null,null,false);
  }
  boolean put_value(String name, String value){
     return put_value(name,value,false);
  }
  boolean put_value(String name, String value, boolean end){
    String thisLine;
    boolean wasOK = true;
    if(name != null && value != null){
      String locstr = head+URLEncoder.encode(name)+"="+
                          URLEncoder.encode(value);
      allLines.addElement(locstr);
      scanbook.dbg.out(headsql+name+"=>'"+value+"'");
      head = "&";
      headsql = ",";
    }
    if(end){
      scanbook.dbg.out(");\n");
      head = "";
      headsql = "";
      try {
        URLConnection conn;
        BufferedReader theHTML;
	conn = u.openConnection();
        conn.setDoInput(true);
        conn.setDoOutput(true);
        conn.setUseCaches (false); 
        conn.setRequestProperty
                       ("Content-Type", "application/x-www-form-urlencoded");
	//        conn.setRequestProperty
	//                       ("Connection", "Keep-Alive");
	DataOutputStream str = new DataOutputStream(conn.getOutputStream());
        lines = allLines.elements();
        while (lines.hasMoreElements()){
            thisLine = (String)lines.nextElement();
            scanbook.dbg.out(thisLine);
            str.writeBytes(thisLine);
	}
        str.flush();
        str.close();
	//        System.out.println("Start..5.");
	//	System.out.println("Wait reply");
        allLines.setSize(0);
        boolean first = true;
        if(scanbook.netscaperun){
	    //          System.out.println("Netscaperun");
          try{
	      PrivilegeManager.enablePrivilege("UniversalConnect");
          }
          catch (Exception exc) {
             System.err.println(exc);
          }
        }
	//          System.out.println("Wait reply  1");
          istr = conn.getInputStream();
	  //          System.out.println("Wait reply  1-1");
	  InputStreamReader isrd = new InputStreamReader(istr);
	  //          System.out.println("Wait reply  1-2");
	  theHTML = new BufferedReader (isrd);
	  //               new InputStreamReader(conn.getInputStream()));
	  //          System.out.println("Wait reply 2");
	  errorMessage infos = null;          
readloop:  while ((thisLine = theHTML.readLine()) != null) {
	  scanbook.dbg.out("Receiving "+thisLine);
          if(thisLine.length() >2  && thisLine.startsWith("-9"))
                break readloop;    
          first = false;
          if(first)System.out.println("Receiving ");
	  //	  System.out.println("Receiving "+thisLine);
          if(thisLine.startsWith(
                  "<HEAD><TITLE>Request Failed</TITLE></HEAD>")){
             allLines.addElement("-1 Oracle Error");
             allLines.addElement("-1 Could not connect to Scanbook database");
             allLines.addElement("-1 Please try again later");
             wasOK = false;
             break readloop;
	  }
          if(thisLine.length() >2  && 
           !thisLine.startsWith("Content-type: text/html")){
             String c = thisLine.substring(0,2);
             int index = 0;
             if(!Character.isSpaceChar(thisLine.charAt(0)) &&
                !Character.isSpaceChar(thisLine.charAt(1))){
                index = Integer.parseInt(c.replace('+',' ').trim());
	     }
             if(index == -1){
                 wasOK = false;
             }
             if(index == -2){
		if(infos == null){
	          infos = new errorMessage(currentFrame,"");
                }
                infos.newline(thisLine.substring(2));
	        scanbook.dbg.out(" *DBG : "+thisLine.substring(2));
             }
             
             allLines.addElement(thisLine);
	     //         System.out.println("Receiving "+thisLine);
	     //             pgf.setValue((float)allLines.size()/100.);
	  }
        }
        lines = allLines.elements();
        theHTML.close();
        theHTML = null;
        isrd.close();
        isrd = null;
        istr.close();
        istr = null;
        conn = null;
        scanbook.dbg.out("Input closed.");
        if(infos != null){
	    infos.display();
	}
        if(!wasOK){
	    //	  if(currentFrame == null){
	    //	     currentFrame = new Frame();
	    //	  }
	  errorMessage msg = new errorMessage(currentFrame,"");
          fillError(msg);
          msg.display();
	}
      }
      catch (Exception exc) {
        System.err.println(exc);
        return false;
      }
    }
    return wasOK;
  }
  void sendSelection(fillable f){
     String value = f.getSelection();
     if(value != null){
       put_value(f.getMenuName(),value );
     }
  }
  void fillList(fillable f[]){
     String thisLine;
     String c;
     scanbook.dbg.out("***** Start filling ...");    
//     System.out.println("***** Start filling ...");    
     for( int i=0; i<f.length; i++){
        fillable ff = f[i];
        ff.removeAll();
     }
     while((thisLine = read_line()) != null){
       //	 System.out.println("Parsing*"+thisLine+"*");    
         c = thisLine.substring(0,2);
	 //	 System.out.println("Parsing c = *"+c+"*");    
         int index = Integer.parseInt(c.replace('+',' ').trim());
	 //	 System.out.println("Parsed "+String.valueOf(index));    
         thisLine = thisLine.substring(2);
         boolean select = false;
         if(thisLine.substring(0,1).equals("*")){select = true;}
         boolean enabled = true;
         if(thisLine.substring(0,1).equals("-")){enabled = false;}
         thisLine = thisLine.substring(1);
         if(index >= 0){
	   //           System.out.println("adding "+thisLine);    
           f[index].addItem(thisLine,select,enabled); 
	 }
     }
  }
  void fillListTest(fillable f[]){
     String thisLine;
     String c;
     for( int i=0; i<f.length; i++){
        fillable ff = f[i];
        ff.removeAll();
     }
     for( int i=0; i<f.length; i++){
         f[i].addItem("line1",false,true); 
         f[i].addItem("line2",false,true); 
     }
  }
  void fillList(fillable f){
     f.removeAll();
     String thisLine;
     while((thisLine = read_line()) != null){
       if(!thisLine.startsWith("-")){
         thisLine = thisLine.substring(2);
         boolean select = false;
         if(thisLine.substring(0,1).equals("*")){select = true;}
         boolean enabled = true;
         if(thisLine.substring(0,1).equals("-")){enabled = false;}
         thisLine = thisLine.substring(1);
         f.addItem(thisLine,select,enabled); 
       }
     }
  }
  void fillError(errorMessage errmsg){
      //       System.out.println("fillError");
     String thisLine;
     while((thisLine = read_line()) != null){
	 //         System.out.println("line =>"+thisLine+"-");
         String c = thisLine.substring(0,2);
         int index = Integer.parseInt(c.replace('+',' ').trim());
         if(index == -1 ){
           thisLine = thisLine.substring(2);
           boolean select = false;
           thisLine = thisLine.substring(1);
           errmsg.newline(thisLine);
         }
     }
  }
  String read_line(){ 
    String thisLine;
    while (lines.hasMoreElements()){
        thisLine = (String)lines.nextElement();
        scanbook.dbg.out("read line =>"+thisLine+"-");    
        if(thisLine.length()>0 && 
          !thisLine.startsWith("Content-type: text/html")){
            return thisLine;
	}
     }
     allLines.setSize(0);
     return null;
  }
}




















