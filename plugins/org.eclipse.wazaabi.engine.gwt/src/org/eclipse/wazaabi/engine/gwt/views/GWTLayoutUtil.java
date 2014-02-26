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

package org.eclipse.wazaabi.engine.gwt.views;

import com.google.gwt.user.client.ui.Panel;
import com.google.gwt.user.client.ui.Widget;


public class GWTLayoutUtil {
    public static void addChild(Widget child, Panel parent, int index) {
        parent.add(child); // TODO index
    }
}
