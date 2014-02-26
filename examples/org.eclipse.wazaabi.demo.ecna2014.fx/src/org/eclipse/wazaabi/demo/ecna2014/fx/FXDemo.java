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

package org.eclipse.wazaabi.demo.ecna2014.fx;

import org.eclipse.wazaabi.demo.ecna2014.core.ui.DemoUI;
import org.eclipse.wazaabi.engine.fx.nonosgi.FXHelper;
import org.eclipse.wazaabi.engine.fx.viewers.FXViewer;
import org.eclipse.wazaabi.locationpaths.nonosgi.LocationPathsHelper;
import org.eclipse.wazaabi.locator.urn.java.nonosgi.URNJavaLocatorHelper;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.fx.styles.FXStylesFactory;
import org.eclipse.wazaabi.mm.fx.styles.VBoxRule;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.layout.StackPane;
import javafx.stage.Stage;


public class FXDemo extends Application {

    private static final Logger log = LoggerFactory.getLogger(FXDemo.class);


    public static void main(String[] args) {
        log.info("Starting FX demo");
        try {
            Application.launch(args);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void start(Stage stage) throws Exception {
        log.debug("fx.Application.start");

        stage.setTitle("Hello JavaFX");

        Scene scene = new Scene(new StackPane(), 400, 300);
        stage.setScene(scene);

        FXViewer viewer = new FXViewer(scene);
        FXHelper.init(viewer);
        URNJavaLocatorHelper.init(viewer);
        LocationPathsHelper.init(viewer);

        Container container = DemoUI.create(viewer, "org.eclipse.wazaabi.demo.ecna2014.fx.handlers.");

        // set engine-specific layout
        VBoxRule layout = FXStylesFactory.eINSTANCE.createVBoxRule();
        layout.setPropertyName("layout");
        container.getStyleRules().add(layout);

        viewer.setContents(container);

        stage.show();
    }
}