import java.awt.*;               // ScrollPane, PopupMenu, MenuShortcut, etc..
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.ItemListener;
import java.awt.event.ItemEvent;

class listDisplay extends Panel
            implements ItemListener, fillable{

  List list = new List(20);
  Panel mainPanel = new Panel();
  String menuName;
  TextArea hdr;
  listDisplay(String header, String m, Color fg, Color bg) {
    super();
    setFont(scanbook.myFont);
    menuName = m;
    GridBagLayout gblmain =  new GridBagLayout();
    setLayout(gblmain);
    GridBagConstraints gbcmain = new GridBagConstraints();
    gbcmain.fill = GridBagConstraints.HORIZONTAL;
    gbcmain.weightx = 1;
    gblmain.setConstraints(mainPanel,gbcmain);
    add(mainPanel);
    mainPanel.setBackground(fg);

    GridBagLayout gbl =  new GridBagLayout();
    mainPanel.setLayout(gbl);
    GridBagConstraints gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = GridBagConstraints.RELATIVE;
    gbc.fill = GridBagConstraints.BOTH;
    gbc.weightx = 1;
    gbc.weighty = 0;
    //
    String work = new String(header);
    int nrows = 1;
    //    System.out.println("work start ="+work+"***");        
    int iw = work.indexOf("\n");
    int ncols = header.length();
    if(iw >0){
     ncols = header.indexOf("\n");
     while(work.length()>0){
      iw = work.indexOf("\n");
      if(iw >0){
        nrows++;
        if( iw < work.length()-2){
          String w2 = new String(work.substring(iw+2));  
	  //          System.out.println("w2 ="+w2+"***");        
          work = new String(w2);
	  //	  System.out.println("work ="+work+"***");        
        }else{
          work = new String("");
	}
      }
     }
    }
    //    System.out.println("Finiiii nrows, ncols = "+String.valueOf(nrows)+" "+String.valueOf(ncols));
    hdr = new TextArea(header,nrows,ncols,TextArea.SCROLLBARS_NONE);
    hdr.setFont(scanbook.myFont);
    hdr.setEditable(false);

    gbl.setConstraints(hdr,gbc);
    mainPanel.add(hdr);

    gbl.setConstraints(list,gbc);
    mainPanel.add(list);
    list.addItemListener(this);
    Dimension dh = hdr.getPreferredSize();
    Dimension dl = list.getPreferredSize();
    setSize(dh.width,dh.height+dl.height);
  }
  //  public void sendSelection(oconnect c){
  //    c.put_value(menuName,getSelection());
  //  }
  public void resetEmpty(){
    removeAll();
    showIfNeeded();
  }
  public void showIfNeeded(){
    if(list.getItemCount() > 0){
      setVisible(true);
    }else{
      setVisible(false);
    }
    if(scanbook.m1 != null){
      scanbook.m1.packshow();
      //    }else{
      //     System.out.println("Null ---");        
    }
  }
  public String getSelection(){
    return list.getSelectedItem();
  }
  public String getMenuName(){
    return menuName;
  }
  void setTitle(String s){
      hdr.setText(s);
  }
  public void addItem(String lbl){
    addItem(lbl,false,true);
  }
  public void addItem(String lbl, boolean select, boolean enabled){
    if(enabled){
      list.addItem(lbl);
      if(select){
        list.select(list.getItemCount()-1);
      }
    }else{
      setTitle(lbl);
    }
  }
    /*
  public void addItem(String lbl, boolean select, boolean enabled){
    list.addItem(lbl);
    if(select){
      list.select(list.getItemCount()-1);
    }
  }
    */
  public void removeAll(){
    if(list.getItemCount()>0){
      list.removeAll();
    }
  }
  public Insets getInsets(){
    return new Insets(10,10,10,10);
  }
  public void itemStateChanged(ItemEvent e) {
        String lbl = list.getItem(((Integer)(e.getItem())).intValue());
	//      System.err.println("2");
        scanbook.callbacks(menuName,lbl);
  }
}
