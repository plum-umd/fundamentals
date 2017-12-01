import edu.neu.ccs.gui.*;
import edu.neu.ccs.util.*;

import javax.swing.*;

/**
 * A simple GUI for selecting the start and end point for 
 * graph traversal algorithm between the 48 US states, and
 * invoking one of the three algorithms: Breadth First Search,
 * Depth First Search, or Shortest Path
 * 
 * @(#)GraphAlgoView.java  30 November 2007
 *  Viera Proulx
 */
public class GraphAlgoView extends Display implements JPTConstants{

  /** a list of all 48 states */
  protected String[] states = 
    new String[]{"AL", "AR", "AZ", "CA", "CO", "CT", "DE", "FL", 
      "GA", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", 
      "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", 
      "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", 
      "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY"};

  /** The view to select the start state */
  protected DropdownView fromView = new DropdownView(states);

  /** The view to select the start state */
  protected DropdownView toView = new DropdownView(states);

  /** The display to hold the view for selecting the start state */
  protected ScrollableDisplay fromDisplay = new ScrollableDisplay(fromView);

  /** The display to hold the view for selecting the start state */
  protected ScrollableDisplay toDisplay = new ScrollableDisplay(toView);

  /** A list of three choices of algorithms for user to select */
  protected String[] algoChoices = 
    new String[]{"Breadth First Search", 
      "Depth First Search", 
  "Shortest Path"};



  /** perform breadth first search when this choice is made */
  SimpleAction bfsAction = new SimpleAction(){
    public void perform(){
      bfs(fromView.getViewState(), toView.getViewState());
    };
  };

  /** perform depth first search when this choice is made */
  SimpleAction dfsAction = new SimpleAction(){
    public void perform(){
      dfs(fromView.getViewState(), toView.getViewState());
    };
  };

  /** perform shortest path search when this choice is made */
  SimpleAction spAction = new SimpleAction(){
    public void perform(){
      sp(fromView.getViewState(), toView.getViewState());
    };
  };

  /** The list of three actions the program can take */
  protected Action[] algoActions = new Action[]{bfsAction, dfsAction, spAction};

  /** The view for selecting which algorithm to perform */
  protected OptionsView algoChoiceView = 
    new OptionsView(algoChoices, algoActions);

  /** a stub for the breadth first search algorithm: 
   * replace the body with a call to your algorithm implementation */
  public void bfs(String fromState, String toState){
    System.out.println("BFS selected: from " + fromState + " to " + toState);
  }

  /** a stub for the breadth first search algorithm: 
   * replace the body with a call to your algorithm implementation */
  public void dfs(String fromState, String toState){
    System.out.println("DFS selected: from " + fromState + " to " + toState);
  }

  /** a stub for the breadth first search algorithm: 
   * replace the body with a call to your algorithm implementation */
  public void sp(String fromState, String toState){
    System.out.println("SP selected: from " + fromState + " to " + toState);
  }

  /**
   * The constructor
   */
  public GraphAlgoView() {
    super();

    // add the internal panel to the Display
    add(createDisplay());

    // set the title and annotation for the Display
    setTitleText("Graph Algorithms");
  }

  /**
   * Construct a table panel display for selecting
   * from and to state and the algorithm to run
   */
  protected TablePanel createDisplay() {

    TablePanel graphAlgoDisplay = new TablePanel(new Object[][]{
        {"From:", this.fromView},
        {"To:", this.toView},
        {"Algorithm:", this.algoChoiceView}},
        VERTICAL, 5, 5);

    return graphAlgoDisplay;          
  }

  /** Run a simple test */
  public static void main(String[]argv){
    GraphAlgoView graphAlgoGUI = new GraphAlgoView();
    GeneralDialog.showOneButtonDialog(graphAlgoGUI, "Travel USA", "Exit");
  }

}