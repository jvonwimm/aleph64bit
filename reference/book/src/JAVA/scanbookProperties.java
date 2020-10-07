import java.awt.*;               // ScrollPane, PopupMenu, MenuShortcut, etc..
import java.awt.event.*;         // New event model.
import java.util.*;
import java.io.*;
import java.util.Calendar;
import java.util.Properties;
import netscape.security.PrivilegeManager;
import java.security.*;

public class scanbookProperties extends Properties{
  String saveFile;
  boolean netscaperun;
  configFrame config;
  scanbookProperties(boolean netscr){
    super();    
    netscaperun = netscr;
    //    System.out.println("Start security");
    put("NetscapeFontSize","8");
    //    put("NetscapeFontSize","10");
    put("fontSize","12");
    put("defaultEdir","Default");
    if(netscaperun){
     try{
       PrivilegeManager.enablePrivilege("UniversalPropertyRead");
     }
     catch (Exception exc) {
       System.err.println(exc); 
     }
    }
    //    System.out.println("Accessing system properties ...");
    //    if(!scanbook.inBrowser || netscaperun){
	//      System.out.println("Accessing system properties ...");
      String homedir = System.getProperty("user.home");
      //      System.out.println("Homedir = "+homedir);
      saveFile = homedir+File.separator+".scanbookrc";
      readProperties();
      //      System.out.println("End reading properties");
      //    }
  }
  public void editProperties(){
    cancelDisplay();
    config = new configFrame("Preferences");
    config.packshow();
  }
  public void cancelDisplay(){
    if(config != null)config.setVisible(false);
  }
  public void saveProperties(){
    if(netscaperun){
      try{
        PrivilegeManager.enablePrivilege("UniversalFileWrite");
      }
      catch (Exception exc) {
        System.err.println("UniversalFileWrite not given"); 
        return;
      }
    }
    try{
       FileOutputStream os = new FileOutputStream(saveFile);
       //       System.out.println("Saving into "+saveFile+" ...");
       save(os,"Scanbook preferences");
       //       System.out.println("... Done");
    }catch (Exception exc) {
       System.err.println(exc); 
    }
  }
  void readProperties(){
    boolean netscaperun = scanbook.netscaperun;
    if(netscaperun){
      try{
         PrivilegeManager.enablePrivilege("UniversalFileRead");
      }
      catch (Exception exc) {
         System.err.println("UniversalFileRead not given"); 
         return;
      }
    }
    File in = new File(saveFile);
    if(in.exists() && in.canRead()){
      try{
        FileInputStream inputs = new FileInputStream(saveFile);
	//        System.out.println("Reading from "+saveFile+" ...");
        load(inputs);
	/*
        System.out.println("... Done");
        System.out.println("... NetscapeFontSize="+
                           getProperty("NetscapeFontSize"));
        System.out.println("... Font="+
                           getProperty("fontSize"));
        System.out.println("... Done");
	*/
      }catch (Exception exc) {
        System.err.println(exc); 
      }
    }
  }
} 

class configFrame extends Frame implements ActionListener{

  configPanel parms;
  buttonsMenu boutons;
  GridBagConstraints gbc = new GridBagConstraints();
  String mode = "";
  String dfltFont;
  Color fg, bg;
  simpleChoice fontChooser;

  configFrame(String name) {
    super(name);
    fg = scanbook.fg;
    bg = scanbook.bg;
    GridBagLayout gbl = new GridBagLayout();
    setLayout(gbl);
    gbc.gridx = 0;
    gbc.gridy = GridBagConstraints.RELATIVE;
    gbc.gridwidth = GridBagConstraints.REMAINDER;
    gbc.fill = GridBagConstraints.HORIZONTAL;
    gbc.weightx = 1.;

    parms = new configPanel(name, fg, bg);
    gbl.setConstraints(parms,gbc);
    add(parms);
    boutons = new buttonsMenu(name, fg, bg);
    boutons.setListener(this);
    boutons.addItem("Apply",false,true);
    boutons.addItem("Save",false,true);
    boutons.addItem("Cancel",false,true);
    gbl.setConstraints(boutons,gbc);
    add(boutons);
    addTxtItem("Edir directory",false);
    addItem(scanbook.defaultEdir,false);
    fontChooser = addMenuItem("Font");
    dfltFont = scanbook.myFont.fontSize;
    addFont(8);
    addFont(10);
    addFont(12);
    addFont(14);
    addFont(16);
    addFont(18);
    addFont(20);
    setLocation(100,50);
  }
  void addFont(int newFont){
    String siz;
    siz = String.valueOf(newFont);
    boolean select = false;
    if(siz.equals(dfltFont)) select = true; 
    if(siz.length() < 2){
      siz = " "+siz;
    }
    //    System.out.println("Adding"+"*"+siz+"*"+dfltFont+"*"+String.valueOf(select)+" "+String.valueOf(siz.length()));    
    fontChooser.addItem(siz,select,true);
  }
  public void removeAllBoxes(){
    parms.removeAllBoxes();
  }
  public void removeAll(){
    parms.removeAll();
  }
  void addCbItem(String lbl, boolean prot){
    parms.addCbItem(lbl,false,prot);
    //    pack();
  }
  void addTxtItem(String lbl, boolean prot){
    parms.addTxtItem(lbl,false,prot);
    //    pack();
  }
  void addItem(String lbl, boolean prot){
    parms.addItem(lbl,false,prot);
    //    pack();
  }
  simpleChoice addMenuItem(String lbl){
    simpleChoice sc = parms.addMenuItem(lbl);
    //    pack();
    return sc;
  }
  public void packshow(){
      pack();
      show();
      toFront();
  }
  public void actionPerformed(ActionEvent e) {
    String lbl = ((Button)e.getSource()).getLabel();
	System.out.println("Button  : *"+lbl+"*");
    if(lbl.equals("Cancel")){
	setVisible(false);
    }    
    if(lbl.equals("Apply")){
        apply();
    }    
    if(lbl.equals("Save")){
        apply();
        scanbook.scbProp.saveProperties();
    }     
  }
  void apply(){
    System.out.println("Selection : "+parms.getSelection());
    doActions(parms.getSelection());
  }
  void doActions(String sin){
    String name;
    String value;
    String token;
    int poseq;
    String s = sin.substring(1);
    StringTokenizer tkz = new StringTokenizer(s,"&");
    int nbtok = tkz.countTokens();
    System.out.println("Nb tokens = "+String.valueOf(nbtok));
    for(int i=0;i<nbtok;i++){
      token = tkz.nextToken();
      System.out.println("Tokens : *"+token+"*");
      poseq = token.indexOf("=");
      name = token.substring(0,poseq);
      value = token.substring(poseq+1);
      System.out.println("Name  : *"+name+"*"+" Value *"+value+"*");
      setValue(name,value);
    }     
    scanbook.callbacks("configFrame","rebuild");
  }
  void setValue(String name, String value){
    boolean netscaperun = scanbook.netscaperun;
    if(name.equals("Font")){
      int newfont = Integer.parseInt(value.trim());
      scanbook.myFont = new scanbookFont(newfont);       
      if(netscaperun){
         System.out.println("Netscape Font Size ****************");
         scanbook.scbProp.put("NetscapeFontSize",value);;
      }else{
         System.out.println("NormalFont Size ****************");
         scanbook.scbProp.put("fontSize",value);;
      }
    }
    if(name.equals("Edir directory")){
      scanbook.defaultEdir = value;       
      scanbook.scbProp.put("defaultEdir",value);;
    }
  }
}













