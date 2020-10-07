import java.awt.*;               // ScrollPane, PopupMenu, MenuShortcut, etc..
import java.awt.event.ActionListener;
import java.util.Vector;
import java.awt.event.ActionEvent;

public class buttonsMenu extends Panel 
                   implements ActionListener, fillable {

  String menuName;
  Panel mainPanel = new Panel();
  Panel lastP = new Panel();
  Color bg, fg;
  ActionListener defaultListener;
  Vector allButtons = new Vector();
  boolean spacing = true;
  buttonsMenu(String m,Color ffg, Color bbg) {
    super();
    setFont(scanbook.myFont);
    defaultListener = this;
    menuName = m;
    bg = bbg;
    fg = ffg;
    setLayout(new GridLayout(1,1));
    add(mainPanel);
    mainPanel.setBackground(bg);
    mainPanel.setLayout(new GridLayout(1,0));
    mainPanel.add(lastP);
    lastP.setBackground(bg);
  }
  public String getSelection(){
    return null;
  }
  public void setSpacing(boolean s){
    spacing = s;
    if(!spacing)mainPanel.remove(lastP);
  }
  public void setListener(ActionListener al){
    defaultListener = al;
  }
  public Insets getInsets(){
    return new Insets(10,10,10,10);
  }
  public void actionPerformed(ActionEvent e) {
    String lbl = ((Button)e.getSource()).getLabel();
    scanbook.callbacks(menuName,lbl);
  }
  void setEnabled(String lbl, boolean state){
    for(int i = 1;i<allButtons.size();i++){
      Button b = (Button)allButtons.elementAt(i);
      if(b.getLabel().equals(lbl)){
        b.setEnabled(state);
      }
    }
  }
  public void removeAll(){}
  public String getMenuName(){
    return menuName;
  }
  public void addItem(String lbl){
    addItem(lbl,false,true);
  }
  public void addItem(String lbl, boolean select, boolean enabled){
    Button b = new Button(lbl);
    b.setBackground(fg);
    mainPanel.remove(lastP);
    if(spacing)mainPanel.add(new Panel());
    mainPanel.add(b);
    if(spacing)mainPanel.add(lastP);
    b.addActionListener(defaultListener);
    allButtons.addElement(b);
  }    
  public void highlightButton(String label){
    Component children[] = mainPanel.getComponents();
    for(int ic=0; ic< children.length; ic++){
        Object o = children[ic];
	if(o instanceof Button){
	  String s = ((Button)o).getLabel();
          if(s.equals(label)){
	      ((Button)o).setBackground(fg.darker());
	  }else{
	      ((Button)o).setBackground(fg);
	  }
	}
    } 
  }
}









