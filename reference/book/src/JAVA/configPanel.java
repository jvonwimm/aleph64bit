import java.awt.*;     
import java.awt.event.*;

public class configPanel extends Panel 
                      implements ActionListener, fillable, Cloneable{


  Label parameters[] = new Label[20];
  TextField values[] = new TextField[20];
  Checkbox  cbxes[] = new Checkbox[20];
  simpleChoice choices[] = new simpleChoice[20];
  int nparam = 0;
  int currentPos;

  String mode ="";
  String menuName;

  GridBagLayout gbl;
  GridBagConstraints gbc1, gbc2;

  public configPanel(String name, Color fg, Color bg){
    super();
    gbl = new GridBagLayout();
    setLayout(gbl);
    gbc1 = new GridBagConstraints();
    gbc1.gridx = 0;
    gbc1.gridy = 0;
    gbc1.weightx = 1.;
    gbc1.weighty = 1.;
    gbc1.anchor = GridBagConstraints.WEST;
    gbc2 = new GridBagConstraints();
    gbc2.gridx = 1;
    gbc2.gridy = 0;
    gbc2.weightx = 1.;
    gbc2.weighty = 1.;
    gbc2.anchor = GridBagConstraints.WEST;

    menuName = name;
  }
  public void removeAllBoxes(){
    if(nparam > 0){
      for(int i=0; i<nparam;i++){
        remove(parameters[i]);
        if(values[i] != null)remove(values[i]);
        if(choices[i] != null)remove(choices[i]);
        if(cbxes[i] != null)remove(cbxes[i]);
      }
    }
    nparam = 0;
  }
  public void removeAll(){
    currentPos = 0;
  }
  public String getMenuName(){
    return menuName+mode;
  }
  public void setMode(String s){
    mode = s;
  }
  public void addItem(String str, boolean select, boolean prot){
/*
||  Fill current input field
*/
    if(currentPos < nparam){
      TextField t = values[currentPos];
      if(t != null && t.isEditable()){
         t.setText(str);
      }
// Menu
      simpleChoice c = choices[currentPos];
      if(c != null){
         c.select(str);
      }
// Checkbox
      Checkbox cb = cbxes[currentPos];
      if(cb != null){
         if(str.startsWith("t"))cb.setState(true);
         else cb.setState(false);
      }
      currentPos++;
    }
  }
  public void addTxtItem(String name, boolean select, boolean prot){
/*
||  Add one text field in the frame
*/
    parameters[nparam]= new Label(name);
    values[nparam]= new TextField(40);
    if(prot){
      values[nparam].setEchoChar('#');
    }
    gbl.setConstraints(parameters[nparam],gbc1);
    add(parameters[nparam]);
    gbl.setConstraints(values[nparam],gbc2);
    add(values[nparam]);
    nparam++;
    gbc1.gridy++;
    gbc2.gridy++;
  }
  public simpleChoice  addMenuItem(String name){
/*
||  Add one menu in the frame
*/
    parameters[nparam]= new Label(name);
    values[nparam]= null;
    choices[nparam] = new simpleChoice(name);
    gbl.setConstraints(parameters[nparam],gbc1);
    add(parameters[nparam]);
    gbl.setConstraints(choices[nparam],gbc2);
    add(choices[nparam]);
    nparam++;
    gbc1.gridy++;
    gbc2.gridy++;
    return choices[nparam-1];
  }
  public void addCbItem(String name, boolean select, boolean prot){
/*
||  Add one checkbox in the frame
*/
    parameters[nparam]= new Label(name);
    cbxes[nparam]= new Checkbox();
    cbxes[nparam].setState(prot);
    gbl.setConstraints(parameters[nparam],gbc1);
    add(parameters[nparam]);
    gbl.setConstraints(cbxes[nparam],gbc2);
    add(cbxes[nparam]);
    nparam++;
    gbc1.gridy++;
    gbc2.gridy++;
  }
  public String getSelection(){
/*
|| Return all current selections in the form 
|| of a character string suitable for CGI input
*/
    String out = " &";
    String header = "";
    for(int i=0;i<nparam;i++){
	  if(values[i] != null){
             out = out.concat(header+parameters[i].getText()+"="+
                        values[i].getText());
             header = "&";
	  }
          if(choices[i] != null){
             out = out.concat(header+parameters[i].getText()+"="+
                        choices[i].getSelectedItem());
             header = "&";
	  }
          if(cbxes[i] != null){
             String boxval = "false";
             if(cbxes[i].getState()) boxval = "true";
             out = out.concat(header+parameters[i].getText()+"="+boxval);
             header = "&";
	  }
    }
    return out;
  }
  public void actionPerformed(ActionEvent e) {
    String action = e.getActionCommand(); 
    if(action.equals("Quit")){
      //      System.exit(0);
    }
  }
  public void noeditLast(){
    //     System.out.println(" Noedit "+ String.valueOf(nparam));
    TextField t = values[nparam-1];
    if(t != null && t.isEditable())t.setEditable(false);
  }
  public Object clone(){
    configPanel work = new configPanel(menuName,scanbook.fg,scanbook.bg);
    work.removeAll();
    //      System.out.println(" Debut clone");
    for(int i=0;i<nparam;i++){
      if(values[i] != null){
        work.addTxtItem(parameters[i].getText(),false, 
                        values[i].echoCharIsSet());
      }
      if(choices[i] != null){
        Choice cloc = work.addMenuItem(parameters[i].getText());
        int nn = choices[i].getItemCount();
        for(int j=0;j<nn;j++){
          cloc.addItem((String)choices[i].getItem(j));
	}
      }
      if(cbxes[i] != null){
        work.addCbItem(parameters[i].getText(),false, cbxes[i].getState());
      }
    }
    return work;
  }
}









