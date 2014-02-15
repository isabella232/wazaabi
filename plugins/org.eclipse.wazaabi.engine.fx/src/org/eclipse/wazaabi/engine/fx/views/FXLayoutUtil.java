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

package org.eclipse.wazaabi.engine.fx.views;

import javafx.scene.Node;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.FlowPane;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;


public class FXLayoutUtil {
    
    public static void addChild(Node child, Pane parent) {
        parent.getChildren().add(child);
//        if (parent instanceof GridPane) {
//            ((GridPane) parent).getChildren().add(child, 0, 0);
//        } else if (parent instanceof FlowPane) {
//            ((FlowPane) parent).
//        } else if (parent instanceof HBox) {
//            
//        } else if (parent instanceof VBox) {
//            
//        } else if (parent instanceof BorderPane) {
//            
//        } else if (parent instanceof StackPane) {
//            ((StackPane) parent).getChildren().add(child);
//        }
    }
}
