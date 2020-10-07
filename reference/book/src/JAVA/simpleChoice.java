import java.awt.*; 
import java.awt.event.*;

public class simpleChoice extends Choice implements ItemListener, fillable{

  String menuName;
  simpleChoice(String m) {
    super();
    setFont(scanbook.myFont);
    menuName = m;
    addItemListener(this);
  }
  public void itemStateChanged(ItemEvent e) {
    String lbl = (String)e.getItem();
    scanbook.callbacks(menuName,lbl);
  }
  public void addItem(String lbl, boolean select, boolean enabled){
    addItem(lbl);
    setFont(scanbook.myFont);
    if(select){
      select(getItemCount()-1);
    }
  }
  public String getSelection(){
    return getSelectedItem();
  }
  public String getMenuName(){
    return menuName;
  }
}
