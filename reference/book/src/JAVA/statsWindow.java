import java.awt.*;
import java.util.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

public class statsWindow extends Frame implements ActionListener{
   MenuBar mb = new MenuBar();
   Menu fileMenu = new Menu("File");
   Menu helpMenu = new Menu("Help");
   buttonsMenu buttons; 
   String labels[]; 
   pieChart currentPlot;
   Panel pie = new Panel();

   statsWindow(){
      super();
      setMenuBar(mb);       
      mb.add(fileMenu);
      mb.setHelpMenu(helpMenu);
      fileMenu.addActionListener(this);
      fileMenu.add("Close");
      fileMenu.add("List");
      setLocation(100,100);
      valuesList vl = new valuesList();
      labelsList ll = new labelsList();
      show();
      Graphics g = getGraphics();
      if(g == null){
        System.out.println(" g is null");
      }
      setVisible(false);
      GridBagLayout gbl = new GridBagLayout();
      setLayout(gbl);
      GridBagConstraints gbc = new GridBagConstraints();
      gbc.gridx = 0;
      gbc.gridy = GridBagConstraints.RELATIVE;
      gbc.gridwidth = GridBagConstraints.REMAINDER;
      buttons = new buttonsMenu("PieButtons", scanbook.fg,scanbook.bg);      
      buttons.setSpacing(false);
      gbl.setConstraints(buttons,gbc);
      buttons.setListener(this);
      add(buttons);      
      pie.setLayout(new CardLayout());
      gbl.setConstraints(pie,gbc);
      add(pie);
      oconnect cloc;
      labelsList buttonLabels = new labelsList();
      cloc = new oconnect("give_stats_buttons");
      if(cloc.put_value(null,null,true)){
         cloc.fillList(buttonLabels);
        labels = buttonLabels.getLabels();
        for( int i=0; i<labels.length; i++){
          cloc = new oconnect("give_statistics");
          if(cloc.put_value("stat_by",labels[i],false) 
                      && cloc.put_value(null,null,true)){
            buttons.addItem(labels[i]);
	    cloc.fillList(new fillable[]{ll,vl});
            double values[] = vl.getValues();
            String names[] = ll.getLabels();
            pie.add(labels[i],new pieChart(this,values,names,g));
          }
        }
      }
      String originalSel = buttonLabels.getSelection();
      switchcards(originalSel);
      buttons.highlightButton(originalSel);
      pack();
      show();
   }
   public void switchcards(String selection){
     ((CardLayout)pie.getLayout()).show(pie,selection);
   }
   public void actionPerformed(ActionEvent e) {
     String action = e.getActionCommand(); 
     if(action.equals("Close")){
       setVisible(false);
       return;
     }
     if(action.equals("List")){
       currentPlot.listValues();
       return;
     }
     switchcards(action);
     buttons.highlightButton(action);
   }
}
class valuesList extends Vector implements fillable{
  public void addItem(String s, boolean select, boolean enabled){
    try{
      Double val = Double.valueOf(s);
      addElement(val);
    }catch (Exception exc) {
      System.err.println(exc);
    }
  }
  public void removeAll(){
    removeAllElements();
  }
  public String getSelection(){
    return "";
  }
  public String getMenuName(){
    return "";  
  }
  double[] getValues(){
    int n = elementCount;
    double values[] = new double[n];
    for(int i=0; i<n; i++){
	values[i] = ((Double)elementAt(i)).doubleValue();
    }
    return values;
  }
}

class labelsList extends Vector implements fillable{
  String currentSelection;
  public void addItem(String s, boolean select, boolean enabled){
      addElement(s);
      if(select){
	  currentSelection = s;
      }
  }
  public void removeAll(){
    removeAllElements();
  }
  public String getSelection(){
    return currentSelection;
  }
  public String getMenuName(){
    if(elementCount ==1){
      return (String)elementAt(0);  
    }
    return "";
  }
  String[] getLabels(){
    int n = elementCount;
    String values[] = new String[n];
    for(int i=0; i<n; i++){
	values[i] = (String)elementAt(i);
    }
    return values;
  }
}

class pieChart extends Canvas{
  private static double piby2 = Math.PI / 2.0;
  private static double twopi = Math.PI * 2.0;
  private static double d2r   = Math.PI / 180.0; // degrees to radians.

  protected Color colors[] = new Color[]{
      Color.red, Color.blue, Color.yellow, Color.black, Color.green,
      Color.white, Color.gray, Color.cyan, Color.magenta, Color.darkGray,
      Color.orange, Color.lightGray, Color.pink
  };
  double rawValues[];
  double values[];
  String labels[];
  double radius; 
  int originX = 250;
  int originY = 250;
  int diameter = 250;
  statsWindow theCaller;
  public pieChart(statsWindow inParent, 
                  double inputValues[], String inputLabels[], Graphics g){
      super();
      theCaller = inParent;
      values = inputValues;
      labels = inputLabels;
      double sum = 0;
      for (int i = 0; i < values.length; i++) {
	  sum += values[i];
      }    
      rawValues = new double[values.length];
      for (int i = 0; i < values.length; i++) {
          rawValues[i] = values[i];
	  values[i] /= sum;
      }    
      setSize(500,500);
      paint(g);
  }
  public void paint(Graphics g) {

    theCaller.currentPlot = this;
    radius = (diameter / 2) + 1;

    int cornerX = (originX - (diameter / 2));
    int cornerY = (originY - (diameter / 2));
    
    int startAngle = 0;
    int arcAngle = 0;
    if(g == null){
      System.out.println(" g is null");
    }
    for (int i = 0; i < values.length; i++) {
      arcAngle = (int)(i < values.length - 1 ?
                       Math.round(values[i] * 360) :
                       360 - startAngle);
      g.setColor(colors[i % colors.length]);
      g.fillArc(cornerX, cornerY, diameter, diameter, 
                startAngle, arcAngle);
      drawLabel(g, labels[i], startAngle + (arcAngle / 2));
      startAngle += arcAngle;
    }
    g.setColor(Color.black);
    g.drawOval(cornerX, cornerY, diameter, diameter);  // cap the circle
  }
  public void listValues(){
    for (int i = 0; i < rawValues.length; i++) {
        String out = labels[i];
        while(out.length() < 20){
        	out += " ";
        }
        out += rawValues[i];
	System.out.println(out);
    }
  }
  public void drawLabel(Graphics g, String text, double angle) {
      //    g.setFont(textFont);
      //    g.setColor(textColor);
    double radians = angle * d2r;
    int x = (int) ((radius + 5) * Math.cos(radians));
    int y = (int) ((radius + 5) * Math.sin(radians));
    if (x < 0) { 
	//      x -= SwingUtilities.computeStringWidth(g.getFontMetrics(), text);
      x -= g.getFontMetrics().stringWidth(text);
    }
    if (y < 0) {
      y -= g.getFontMetrics().getHeight();
    }
    g.drawString(text, x + originX, originY - y);
  }
}












