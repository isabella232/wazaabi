/***********************************************************************************************************************
 * Copyright (c) 2008 Olivier Moises, 2014 Pavel Erofeev
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises - initial API and implementation
 *   Pavel Erofeev - rendering engine for JavaFX
***********************************************************************************************************************/

package org.eclipse.wazaabi.engine.fx.snippets;

import javafx.application.Application;
import javafx.event.ActionEvent;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.TextField;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.StackPane;
import javafx.scene.text.Font;
import javafx.scene.text.FontWeight;
import javafx.scene.text.Text;
import javafx.stage.Stage;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.wazaabi.engine.fx.nonosgi.FXHelper;
import org.eclipse.wazaabi.engine.fx.snippets.model.ModelFactory;
import org.eclipse.wazaabi.engine.fx.snippets.model.Winnie;
import org.eclipse.wazaabi.engine.fx.viewers.FXViewer;
import org.eclipse.wazaabi.locationpaths.nonosgi.LocationPathsHelper;
import org.eclipse.wazaabi.locator.urn.java.nonosgi.URNJavaLocatorHelper;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;
import org.eclipse.wazaabi.mm.core.widgets.TextComponent;
import org.eclipse.wazaabi.mm.core.widgets.Label;
import org.eclipse.wazaabi.mm.edp.events.EDPEventsFactory;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.events.PropertyChangedEvent;
import org.eclipse.wazaabi.mm.edp.handlers.Binding;
import org.eclipse.wazaabi.mm.edp.handlers.Converter;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersFactory;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.wazaabi.mm.edp.handlers.StringParameter;
import org.eclipse.wazaabi.mm.edp.handlers.Validator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

//import org.eclipse.wazaabi.engine.fx.nonosgi.FXHelper;
//import org.eclipse.wazaabi.engine.fx.viewers.FXViewer;
//import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
//import org.eclipse.wazaabi.mm.core.widgets.Container;
//import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
//import org.eclipse.wazaabi.mm.core.widgets.PushButton;


public class HelloWorld extends Application {
    
    private static final Logger log = LoggerFactory.getLogger(HelloWorld.class);

    
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

//    private void buildUI(Scene scene) {
//        GridPane grid = new GridPane();
//        grid.setAlignment(Pos.CENTER);
//        grid.setHgap(10);
//        grid.setVgap(10);
//        grid.setPadding(new Insets(25, 25, 25, 25));
//
//        scene.setRoot(grid);
//
//        Text title = new Text("Hello JavaFX application");
//        title.setFont(Font.font("Tahoma", FontWeight.NORMAL, 24));
//        grid.add(title, 0, 0, 2, 1);
//
//        grid.add(new Label("Enter text:"), 0, 1);
//
//        TextField text = new TextField();
//        grid.add(text, 1, 1);
//
//        Button btn = new Button("OK");
//        HBox hbBtn = new HBox(10);
//        hbBtn.setAlignment(Pos.BOTTOM_RIGHT);
//        hbBtn.getChildren().add(btn);
//        grid.add(hbBtn, 1, 4);
//
//        btn.setOnAction(new javafx.event.EventHandler<ActionEvent>() {
//            public void handle(ActionEvent e) {
//                System.out.println("OK clicked");
//            }
//        });
//    }

    @Override
    public void init() throws Exception {
        System.out.println("before init");
        super.init();
        System.out.println("after init");
    }

    @Override
    public void start(Stage stage) throws Exception {
        System.out.println("start");
        stage.setTitle("Hello JavaFX");
        
        Scene scene = new Scene(new StackPane(), 400, 300);
        stage.setScene(scene);

        Winnie w = createBusinessModel();
        
        FXViewer viewer = new FXViewer(scene);
        FXHelper.init(viewer);
        URNJavaLocatorHelper.init(viewer);
        LocationPathsHelper.init(viewer);

        Container container = CoreWidgetsFactory.eINSTANCE.createContainer();
        container.set("input", w);

        Label label1 = CoreWidgetsFactory.eINSTANCE.createLabel();
        label1.setText("Enter some text into the first field:");
        container.getChildren().add(label1);

        TextComponent text1 = CoreWidgetsFactory.eINSTANCE.createTextComponent();
        container.getChildren().add(text1);

//        EventHandler eh1 = EDPHandlersFactory.eINSTANCE.createEventHandler();
//        eh1.setUri("org.eclipse.wazaabi.engine.fx.snippets.FocusOutAction");
//        Event e1 = EDPEventsFactory.eINSTANCE.createEvent();
//        e1.setId("core:ui:focus:out");
//        eh1.getEvents().add(e1);
//        text1.getHandlers().add(eh1);
        text1.getHandlers().add(createBinding(false, "$input/@name"));

        Label label2 = CoreWidgetsFactory.eINSTANCE.createLabel();
        label2.setText("Which is bound to the second field:");
        container.getChildren().add(label2);

        TextComponent text2 = CoreWidgetsFactory.eINSTANCE.createTextComponent();
        container.getChildren().add(text2);
        text2.getHandlers().add(createBinding(true, "$input/@name"));


//        Binding binding = EDPHandlersFactory.eINSTANCE.createBinding();
//        StringParameter source = EDPHandlersFactory.eINSTANCE.createStringParameter();
//        StringParameter target = EDPHandlersFactory.eINSTANCE.createStringParameter();
//        source.setName("source");
//        source.setValue("@text");
//        target.setName("target");
//        target.setValue("../TextComponent[1]/@text");
//        binding.getParameters().add(source);
//        binding.getParameters().add(target);
//        Event bindingEvent = EDPEventsFactory.eINSTANCE.createEvent();
//        binding.getEvents().add(bindingEvent);
//        bindingEvent.setId("core:ui:focus:out");
//        text1.getHandlers().add(binding);
        
        PushButton pushButton = CoreWidgetsFactory.eINSTANCE.createPushButton();
        pushButton.setText("Say Hello");
        container.getChildren().add(pushButton);

        EventHandler eventHandler = EDPHandlersFactory.eINSTANCE.createEventHandler();
        eventHandler.setUri("org.eclipse.wazaabi.engine.fx.snippets.ButtonClickAction");
        pushButton.getHandlers().add(eventHandler);

        Event event = EDPEventsFactory.eINSTANCE.createEvent();
        event.setId("core:ui:selection");
        eventHandler.getEvents().add(event);
        viewer.setCodeLocatorBaseUri("urn:java:");

        viewer.setContents(container);

        //buildUI(scene);
        


        stage.show();
    }
    
    private static Winnie createBusinessModel() {
        final Winnie w = ModelFactory.eINSTANCE.createWinnie();
        w.setName("Pooh");
        
        w.eAdapters().add(new AdapterImpl() {
            @Override
            public void notifyChanged(Notification notification) {
                log.info("Model changed: {}", w);
            }
        });
        
        return w;
    }
    
    private static Binding createBinding(boolean toUI, String property) {
        Binding binding = EDPHandlersFactory.eINSTANCE.createBinding();
        StringParameter source = EDPHandlersFactory.eINSTANCE.createStringParameter();
        StringParameter target = EDPHandlersFactory.eINSTANCE.createStringParameter();
        source.setName("source");
        target.setName("target");
        if (toUI) {
            source.setValue(property);
            target.setValue("@text");
        } else {
            source.setValue("@text");
            target.setValue(property);
        }
        binding.getParameters().add(source);
        binding.getParameters().add(target);

        if (toUI) {
            PropertyChangedEvent bindingEvent = EDPEventsFactory.eINSTANCE.createPropertyChangedEvent();
            bindingEvent.setPath(property);
            binding.getEvents().add(bindingEvent);
        } else {
            Event bindingEvent = EDPEventsFactory.eINSTANCE.createEvent();
            bindingEvent.setId("core:ui:focus:out");
            binding.getEvents().add(bindingEvent);
        }
        return binding;
    }
}
