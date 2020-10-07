import java.io.*;
import java.awt.*;  
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.util.*;              // Utilities.

public class errorMessage{
  Dialog d;
  TextArea ta;
  int ncols;
  Frame f;

  errorMessage(Frame finput, String s){
     if(finput == null){
         f = new Frame();
     }else{
         f = finput;
     }
     ncols = 0;
     d = new Dialog(f,true);
     d.setBackground(Color.white);
     ta = new TextArea("",10,80,TextArea.SCROLLBARS_NONE);
     ta.setEditable(false);
     //     ta.setFont(new Font("Courier",Font.BOLD,20));
     ta.setFont(scanbook.myFont);
     d.add("North",ta);
     newline(s);
     Panel p = new Panel();
     Button b = new Button("OK");
     b.addActionListener(new ActionListener(){
        public void actionPerformed(ActionEvent e) {
         d.dispose();
   }});
     p.add("Center",b);
     d.add("South",p);
  }
  void newline(String sin){
     int pos;
     if( (pos = sin.indexOf("\n")) > -1){
       newline(sin.substring(0,pos));
       newline(sin.substring(pos+1));
       return;
     }
     String s = sin.trim();
     int siz = s.length() + 4;
     if(siz > ncols){ncols = siz;}
     ta.append("\n");
     ta.append("  ");
     ta.append(s);
     ta.append("  ");
  }
  void display(){
     String s = ta.getText();
     StringTokenizer lines = new StringTokenizer(s,"\n");
     ta.setText("\n");
     int nlines = 0;
     while(lines.hasMoreElements()){
       String currentLine = lines.nextToken();
       StringBuffer buf = new StringBuffer(currentLine);
       int toAdd = (ncols - currentLine.length()) / 2;
       for(int i=0; i<toAdd;i++){
           buf.insert(0,' ');
           buf.append(' ');
       } 
       ta.append(buf.toString()+"\n");
       nlines++;
     }
     newline("  ");
     ta.setColumns(ncols);
     ta.setRows(nlines+4);
     Toolkit.getDefaultToolkit().beep();
     d.pack();
     d.show();
  }
}






