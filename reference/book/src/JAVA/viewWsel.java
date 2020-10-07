import java.awt.*;               // ScrollPane, PopupMenu, MenuShortcut, etc..
import java.awt.event.*;         // New event model.
import systemcmd;
//import java.applet.*;
import java.net.*;               // Network

public class viewWsel extends Frame implements ActionListener{
    Color fg, bg;  
    buttonsMenu boutons;
    GridBagConstraints gbc = new GridBagConstraints();
    listDisplay runsList;

    viewWsel(){
	super("WW bad runs");

	fg = scanbook.fg;
	bg = scanbook.bg;
	GridBagLayout gbl = new GridBagLayout();
	setLayout(gbl);
	gbc.gridx = 0;
	gbc.gridy = GridBagConstraints.RELATIVE;
	gbc.gridwidth = GridBagConstraints.REMAINDER;
	gbc.fill = GridBagConstraints.HORIZONTAL;
	gbc.weightx = 1.;

        runsList = new listDisplay(
           "                                                "+
           "                                                ",
           "WRunslist", fg,bg);
        gbl.setConstraints(runsList,gbc);
        add(runsList);

        boutons = new buttonsMenu("viewWselButtons", fg, bg);
        boutons.setListener(this);
        boutons.addItem("OK");
        if(scanbook.osName.indexOf("Windows") == -1 ||
           scanbook.netscaperun){
           boutons.addItem("Open web page");
	}
        gbl.setConstraints(boutons,gbc);
        add(boutons);

        pack();
        show();
        relocate();
    }
  public void relocate(){
    setLocation(10,50);
  }
    /*
  public static void openWeb(Frame f){
        String theURL="http://alephwww.cern.ch/~sical/Wlumi";
	URL url;
	AppletContext ac=null;

        f.setCursor(scanbook.watchCursor);

        if(scanbook.netscaperun){
            try{
              url = new URL(theURL);	  
              if(ac == null){
	        ac = scanbook.a.getAppletContext();
	      }
              ac.showDocument(url,"Scanbook WWW sel");
	    }catch(MalformedURLException me){
             System.err.println("Incorrect URL : "+theURL);
            }
	}else{
          systemcmd.execute("netscape -remote "+
	    "'openURL("+theURL+",new-window)'", true);
          f.setCursor(scanbook.defaultCursor);
          if(systemcmd.inOutput("netscape: not running on display")){
              errorMessage msg = new errorMessage(f,"");
              msg.newline("If you want to see the web page,");
              msg.newline("you should be running Netscape.");
              msg.newline(" ");
              msg.newline("Please launch Netscape on some shell"+
                        " window on this machine");
              msg.newline("and try again");
              msg.display();
          }
	}
  }
    */
  public void actionPerformed(ActionEvent e) {
	String lbl = ((Button)e.getSource()).getLabel();
	//	System.out.println("Button  : *"+lbl+"*");
	if(lbl.equals("OK")){
	    setVisible(false);
            scanbook.m2.useWsel.c.select(0);
	}    
	if(lbl.equals("Open web page")){
            scanbook.openWeb(this,"http://alephwww.cern.ch/~sical/Wlumi");
	}
  }
}











