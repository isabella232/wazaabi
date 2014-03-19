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

import java.io.IOException;
import java.io.InputStream;

import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.layout.StackPane;
import javafx.stage.Stage;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.emf.ecore.xmi.XMIResource;
import org.eclipse.emf.ecore.xmi.impl.XMIResourceImpl;
import org.eclipse.wazaabi.demo.ecna2014.core.model.ModelFactory;
import org.eclipse.wazaabi.demo.ecna2014.core.model.Winnie;
import org.eclipse.wazaabi.engine.edp.coderesolution.ICodeLocator;
import org.eclipse.wazaabi.engine.fx.nonosgi.FXHelper;
import org.eclipse.wazaabi.engine.fx.viewers.FXViewer;
import org.eclipse.wazaabi.locationpaths.nonosgi.LocationPathsHelper;
import org.eclipse.wazaabi.locator.urn.java.nonosgi.URNJavaLocatorHelper;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


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

        stage.setTitle("JavaFX");

        Scene scene = new Scene(new StackPane(), 600, 400);
        stage.setScene(scene);

        final Winnie w = ModelFactory.eINSTANCE.createWinnie();
        w.eAdapters().add(new AdapterImpl() {
            @Override
            public void notifyChanged(Notification notification) {
                System.out.println(w);
            }
        });

        FXDemo.createUI(scene, "urn:java:demo.ui", w);
        stage.show();
    }

    public static void createUI(Scene scene, String path, Object domainModel) {
        XMIResource res = new XMIResourceImpl();

        FXViewer viewer = new FXViewer(scene);
        FXHelper.init(viewer);
        URNJavaLocatorHelper.init(viewer);
        LocationPathsHelper.init(viewer);
        ICodeLocator codeLocator = (ICodeLocator) viewer.getFactoryFor(null,
                path, null, ICodeLocator.class);
        if (codeLocator != null) {
            InputStream in = null;
            try {
                in = codeLocator.getResourceInputStream(path);
            } catch (IOException e) {
                log.error("Cannot get the resource's inputStream {}", e); //$NON-NLS-1$
            }
            if (in != null) {
                try {
                    res.load(in, null);

                } catch (IOException e) {
                    log.error("Cannot load the resource {}", e); //$NON-NLS-1$
                }
                try {
                    in.close();
                } catch (IOException e) {
                    log.error("Error while closing {}, {}", path, e); //$NON-NLS-1$
                }
            }
            if (res != null && res.getContents().size() == 1
                    && res.getContents().get(0) instanceof AbstractComponent) {
                ((AbstractComponent) res.getContents().get(0)).set(
                        "domain", domainModel);
                viewer.setContents(res.getContents().get(0));
            }
        }
    }

}
