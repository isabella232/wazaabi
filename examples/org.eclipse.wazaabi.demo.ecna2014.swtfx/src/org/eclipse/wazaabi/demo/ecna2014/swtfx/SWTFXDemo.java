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

package org.eclipse.wazaabi.demo.ecna2014.swtfx;

import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.layout.StackPane;
import javafx.stage.Stage;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.demo.ecna2014.core.ui.DemoUI;
import org.eclipse.wazaabi.engine.fx.nonosgi.FXHelper;
import org.eclipse.wazaabi.engine.fx.viewers.FXViewer;
import org.eclipse.wazaabi.engine.swt.nonosgi.SWTHelper;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.locationpaths.nonosgi.LocationPathsHelper;
import org.eclipse.wazaabi.locator.urn.java.nonosgi.URNJavaLocatorHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class SWTFXDemo extends Application {

    private static final Logger log = LoggerFactory.getLogger(SWTFXDemo.class);


    public static void main(String[] args) {
        log.info("Starting FX demo");
        try {
            
            Display display = new Display();
            Shell mainShell = new Shell(display, SWT.SHELL_TRIM);
            mainShell.setLayout(new FillLayout());
            mainShell.setSize(600, 400);

            SWTControlViewer viewer = new SWTControlViewer(mainShell);
            SWTHelper.init(viewer);
            URNJavaLocatorHelper.init(viewer);
            LocationPathsHelper.init(viewer);

            DemoUI.create(viewer);

            mainShell.open();
            while (!mainShell.isDisposed()) {
                if (!display.readAndDispatch())
                    display.sleep();
            }
            display.dispose();

            Application.launch(args);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void start(Stage stage) throws Exception {
        log.debug("fx.Application.start");

        stage.setTitle("Hello JavaFX");

        Scene scene = new Scene(new StackPane(), 600, 400);
        stage.setScene(scene);

        FXViewer viewer = new FXViewer(scene);
        FXHelper.init(viewer);
        URNJavaLocatorHelper.init(viewer);
        LocationPathsHelper.init(viewer);

        DemoUI.create(viewer);
        stage.show();
    }
}