import java.awt.*;         

public class scanbookFont extends Font{
    String fontSize;
    scanbookFont(String s){
	super("Courier",Font.BOLD,Integer.parseInt(s));
        fontSize = s;
    }
    scanbookFont(int i){
	super("Courier",Font.BOLD,i);
        fontSize = String.valueOf(i);
    }
}
