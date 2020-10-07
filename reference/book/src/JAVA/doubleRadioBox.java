import java.awt.*;               // ScrollPane, PopupMenu, MenuShortcut, etc..
import java.awt.event.ItemListener;
import java.awt.event.ItemEvent;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.util.*;


public class doubleRadioBox extends Panel
                implements ActionListener, fillable{
  static int HORIZONTAL = 0;
  static int VERTICAL = 1;
  Checkbox currentChoice;
  String menuName;

  Panel mainPanel = new Panel();
  int orientation;
  Vector selections = new Vector();

doubleRadioBox(String m, String label, int ori,  Color fg, Color bg) {

    super();

    menuName = m;
    setLayout(new GridLayout(1,1));
    add(mainPanel);
    mainPanel.setBackground(fg);

    orientation = ori;

    //    setBackground(fg);
    if(orientation == HORIZONTAL){
       mainPanel.setLayout(new GridLayout(1,0));
    }
    if(orientation == VERTICAL){
       mainPanel.setLayout(new GridLayout(0,1));
    }
    setTitle();
  }
  void setTitle(){
    Panel localPanel = new Panel();
    mainPanel.add(localPanel);

    GridBagLayout gbl =  new GridBagLayout();
    GridBagConstraints gbc = new GridBagConstraints();
    localPanel.setLayout(gbl);
    gbc.fill = GridBagConstraints.HORIZONTAL;

    Label l  = new Label("good/bad");
    gbc.weightx = 0;
    gbl.setConstraints(l,gbc);
    localPanel.add(l);

    Panel space = new Panel();
    gbc.weightx = 1;
    gbl.setConstraints(space,gbc);
    localPanel.add(space);

    gbc.weightx = 0;
    Button reset = new Button("Reset");
    localPanel.add(reset);
    reset.addActionListener(this);
  }
  public String getSelection(){
    String word = new String("");
    //    System.out.println("Selection  0");
    //    radioItem selections[] = (radioItem[])mainPanel.getComponents();
    //    System.out.println("Selection  1");
    int maxBit = 2;
    for(int i = 1;i<selections.size();i++){
      maxBit = Math.max(maxBit,
                  ((radioItem)selections.elementAt(i)).bitPosition);
    }
    //    System.out.println("Maxbit : "+String.valueOf(maxBit));
    StringBuffer buf = new StringBuffer();
    for(int i = 0; i< maxBit; i++)buf.append('_');
    buf.setCharAt(0,'F');

    for(int i = 0;i<selections.size();i++){
      //     System.out.println("Selection  2");
      Component radios[] = ((Container)selections.elementAt(i)).getComponents();
      //     System.out.println("Selection  3");
      char c = '_';
      //      String add = String.valueOf('_');
      if(((Checkbox)radios[0]).getState()) c = 'T';
      if(((Checkbox)radios[1]).getState())c = 'F';
      int bp = ((radioItem)selections.elementAt(i)).bitPosition - 1;
      //      System.out.println("bitPos :  " + String.valueOf(bp));
      buf.setCharAt(bp,c);
    }
	  //    word = "______________F_F_%";
    word = buf.toString()+"%";
    //    System.out.println("Selection  was : " + word);
    return word;
  }
  public String getMenuName(){
    return menuName;
  }
  public void addItem(String lbl){
    addItem(lbl,false,true);
  }
  public void addItem(String lbl, boolean select, boolean enabled){
    radioItem item = new radioItem(lbl,this);
    mainPanel.add(item);
    selections.addElement(item);
  }
  public Insets getInsets(){
    return new Insets(10,10,10,10);
  }
  public void removeAll(){
    mainPanel.removeAll();
    selections.removeAllElements();
    setTitle();
  }
  public void actionPerformed(ActionEvent e) {
    for(int i = 0;i<selections.size();i++){
      Component radios[] = ((Container)selections.elementAt(i)).getComponents();
      //      Component radios[] = ((Container)selections[i]).getComponents();
      //      System.out.println("Reset");
      CheckboxGroup g = ((Checkbox)radios[0]).getCheckboxGroup();
      g.setSelectedCheckbox(null);
    }
    scanbook.callbacks(menuName,"Reset");
  }
}

class radioItem extends Panel implements ItemListener {
  CheckboxGroup box = new CheckboxGroup();
  doubleRadioBox caller;
  public int bitPosition;
  radioItem(String lbl, doubleRadioBox boss){
    super();
    caller = boss;
    GridBagLayout gbl =  new GridBagLayout();
    GridBagConstraints gbc = new GridBagConstraints();

    setLayout(gbl);
    gbc.fill = GridBagConstraints.HORIZONTAL;

    //      System.out.println("lbl :*"+String.valueOf(lbl)+"*");
    //      System.out.println("Bit pos :*"+String.valueOf(lbl.substring(0,2))+"*");
    bitPosition = Integer.parseInt(lbl.substring(0,2).replace(' ','0'));
    String trueLabel = lbl.substring(3);

    //    System.out.println("******* Quality Bit : "+lbl);

    Checkbox cgood = new Checkbox("",false,box);
    gbc.weightx = 0;
    gbl.setConstraints(cgood,gbc);
    add(cgood);
    Checkbox cbad = new Checkbox(trueLabel,false,box);
    gbc.weightx = 0;
    gbl.setConstraints(cbad,gbc);
    add(cbad);

    Panel space = new Panel();
    gbc.weightx = 1;
    gbl.setConstraints(space,gbc);
    add(space);
    cgood.addItemListener(this);
    cbad.addItemListener(this);
  }
  public void itemStateChanged(ItemEvent e) {
    //     System.out.println("Selected bit "+String.valueOf(bitPosition));
     scanbook.callbacks(caller.menuName,caller.getSelection());
  }
}



