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
import javafx.scene.Scene;
import javafx.scene.layout.StackPane;
import javafx.stage.Stage;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.wazaabi.engine.core.gef.EditPartViewer;
import org.eclipse.wazaabi.engine.fx.nonosgi.FXHelper;
import org.eclipse.wazaabi.engine.fx.snippets.model.ModelFactory;
import org.eclipse.wazaabi.engine.fx.snippets.model.Winnie;
import org.eclipse.wazaabi.engine.fx.viewers.FXViewer;
import org.eclipse.wazaabi.locationpaths.nonosgi.LocationPathsHelper;
import org.eclipse.wazaabi.locator.urn.java.nonosgi.URNJavaLocatorHelper;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.Label;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;
import org.eclipse.wazaabi.mm.core.widgets.TextComponent;
import org.eclipse.wazaabi.mm.edp.events.EDPEventsFactory;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.events.PropertyChangedEvent;
import org.eclipse.wazaabi.mm.edp.handlers.Binding;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersFactory;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.wazaabi.mm.edp.handlers.StringParameter;
import org.eclipse.wazaabi.mm.fx.styles.FXStylesFactory;
import org.eclipse.wazaabi.mm.fx.styles.VBoxRule;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class HelloWorld extends Application {

    private static final Logger log = LoggerFactory.getLogger(HelloWorld.class);


    public static void main(String[] args) {
        //startSWT(args);
        startFX(args);
    }

    private static void startSWT(String[] args) {
        log.info("Starting SWT version");

//        Display display = new Display();
//        Shell mainShell = new Shell(display, SWT.SHELL_TRIM);
//        mainShell.setLayout(new FillLayout());
//        mainShell.setSize(300, 300);
//
//        SWTControlViewer viewer = new SWTControlViewer(mainShell);
//        SWTHelper.init(viewer);
//        URNJavaLocatorHelper.init(viewer);
//        LocationPathsHelper.init(viewer);
//
//        Container container = createWazaabiUI(viewer);
//        
//        RowLayoutRule layout = SWTStylesFactory.eINSTANCE.createRowLayoutRule();
//        layout.setPropertyName("layout");
//        container.getStyleRules().add(layout);
//
//        mainShell.open();
//        while (!mainShell.isDisposed()) {
//            if (!display.readAndDispatch())
//                display.sleep();
//        }
//        display.dispose();
    }

    private static void startFX(String[] args) {
        log.info("Starting FX version");
        try {
            Application.launch(args);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Override
    public void start(final Stage stage) throws Exception {
        System.out.println("start");
        stage.setTitle("Hello JavaFX");

        Scene scene = new Scene(new StackPane(), 400, 300);
        stage.setScene(scene);

        final FXViewer viewer = new FXViewer(scene);
        FXHelper.init(viewer);
        URNJavaLocatorHelper.init(viewer);
        LocationPathsHelper.init(viewer);

        createWazaabiUI(viewer);

        stage.show();
        
//        Executors.newScheduledThreadPool(1).schedule(new Runnable() {
//            public void run() {
//                log.debug("timer");
//                stage.sizeToScene();
//            }
//        }, 5, TimeUnit.SECONDS);
    }

    private static Container createWazaabiUI(EditPartViewer viewer) {
        final Container container = CoreWidgetsFactory.eINSTANCE.createContainer();
        container.set("input", createBusinessModel());
        
        VBoxRule layout = FXStylesFactory.eINSTANCE.createVBoxRule();
        layout.setSpacing(5);
        layout.setPropertyName("layout");
        container.getStyleRules().add(layout);

        container.getChildren().add(createButton("Change Layout", "ChangeLayoutAction"));

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

        container.getChildren().add(createButton("Say Hello", "ButtonClickAction"));
        container.getChildren().add(createButton("Save text", "ReplaceTextAction"));

        viewer.setCodeLocatorBaseUri("urn:java:");
        viewer.setContents(container);
        
        return container;
    }

    private static Winnie createBusinessModel() {
        final Winnie w = ModelFactory.eINSTANCE.createWinnie();
        w.setName("Pooh");

        w.eAdapters().add(new AdapterImpl() {
            @Override
            public void notifyChanged(Notification notification) {
                log.info(" -> Model changed: {}", w);
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
    
    private static PushButton createButton(String label, String action) {
        PushButton pushButton = CoreWidgetsFactory.eINSTANCE.createPushButton();
        pushButton.setText(label);

        EventHandler eventHandler = EDPHandlersFactory.eINSTANCE.createEventHandler();
        eventHandler.setUri("org.eclipse.wazaabi.engine.fx.snippets." + action);
        pushButton.getHandlers().add(eventHandler);

        Event event = EDPEventsFactory.eINSTANCE.createEvent();
        event.setId("core:ui:selection");
        eventHandler.getEvents().add(event);

        return pushButton;
    }
}
