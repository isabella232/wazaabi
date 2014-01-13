package org.eclipse.wazaabi.engine.fx.snippets;

import javafx.application.Application;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.StackPane;
import javafx.scene.text.Font;
import javafx.scene.text.FontWeight;
import javafx.scene.text.Text;
import javafx.stage.Stage;

import org.eclipse.wazaabi.engine.fx.nonosgi.FXHelper;
import org.eclipse.wazaabi.engine.fx.viewers.FXViewer;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;

//import org.eclipse.wazaabi.engine.fx.nonosgi.FXHelper;
//import org.eclipse.wazaabi.engine.fx.viewers.FXViewer;
//import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
//import org.eclipse.wazaabi.mm.core.widgets.Container;
//import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
//import org.eclipse.wazaabi.mm.core.widgets.PushButton;


public class HelloWorld extends Application {
    public static void main(String[] args) {
        System.out.println("launching the app");
        
        try {
            Application.launch(args);
        } catch (Exception e) {
            e.printStackTrace();
        }
        System.out.println("after launch");
        
//        // create the shell
//        Display display = new Display();
//        Shell mainShell = new Shell(display, SWT.SHELL_TRIM);
//        mainShell.setLayout(new FillLayout());
//        mainShell.setSize(300, 300);
//
//        // create the viewer
//        SWTControlViewer viewer = new SWTControlViewer(mainShell);
//        // init SWT Engine in standalone mode
//        SWTHelper.init(viewer);
//
//        // create a container and set its layout
//        Container container = CoreWidgetsFactory.eINSTANCE.createContainer();
//        RowLayoutRule layoutRule = SWTStylesFactory.eINSTANCE
//                .createRowLayoutRule();
//        ColorRule color = CoreStylesFactory.eINSTANCE.createColorRule();
//        color.setBlue(50);
//        layoutRule.setPropertyName("layout");
//        container.getStyleRules().add(layoutRule);
//
//        // create a pushButton
//        PushButton pushButton = CoreWidgetsFactory.eINSTANCE.createPushButton();
//        pushButton.setText("Hello World"); //$NON-NLS-1$
//        pushButton.getStyleRules().add(color);
//
//        BooleanRule flat = CoreStylesFactory.eINSTANCE.createBooleanRule();
//        flat.setPropertyName(AbstractButtonEditPart.FLAT_PROPERTY_NAME);
//        flat.setValue(true);
//        pushButton.getStyleRules().add(flat);
//
//        // append the button to the container's children list.
//        container.getChildren().add(pushButton);
//
//        // inject the container into the viewer
//        viewer.setContents(container);
//
//        mainShell.open();
//
//        while (!mainShell.isDisposed()) {
//            if (!display.readAndDispatch())
//                display.sleep();
//        }
//        display.dispose();
    }
    
    private void buildUI(Scene scene) {
        GridPane grid = new GridPane();
        grid.setAlignment(Pos.CENTER);
        grid.setHgap(10);
        grid.setVgap(10);
        grid.setPadding(new Insets(25, 25, 25, 25));
        
        scene.setRoot(grid);
        
        Text title = new Text("Hello JavaFX application");
        title.setFont(Font.font("Tahoma", FontWeight.NORMAL, 24));
        grid.add(title, 0, 0, 2, 1);

        grid.add(new Label("Enter text:"), 0, 1);

        TextField text = new TextField();
        grid.add(text, 1, 1);

        Button btn = new Button("OK");
        HBox hbBtn = new HBox(10);
        hbBtn.setAlignment(Pos.BOTTOM_RIGHT);
        hbBtn.getChildren().add(btn);
        grid.add(hbBtn, 1, 4);
        
        btn.setOnAction(new EventHandler<ActionEvent>() {
            public void handle(ActionEvent e) {
                System.out.println("OK clicked");
            }
        });
    }
    
    @Override
    public void init() throws Exception {
        System.out.println("before init");
        super.init();
        System.out.println("after init");
    }

    public void start(Stage stage) throws Exception {
        System.out.println("start");
        stage.setTitle("Hello JavaFX");
        
        Scene scene = new Scene(new StackPane(), 400, 300);
        stage.setScene(scene);
     
        FXViewer viewer = new FXViewer(scene);
        FXHelper.init(viewer);
        
        Container container = CoreWidgetsFactory.eINSTANCE.createContainer();
        PushButton pushButton = CoreWidgetsFactory.eINSTANCE.createPushButton();
        pushButton.setText("Hello World");
        container.getChildren().add(pushButton);

        viewer.setContents(container);
        
        buildUI(scene);
        
        stage.show();       
    }
}
