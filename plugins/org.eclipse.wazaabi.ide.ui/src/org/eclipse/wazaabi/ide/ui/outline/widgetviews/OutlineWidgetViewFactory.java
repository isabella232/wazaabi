/*******************************************************************************
 * Copyright (c) 2008 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.ui.outline.widgetviews;

import org.eclipse.wazaabi.engine.core.CoreSingletons;
import org.eclipse.wazaabi.engine.core.editparts.TextComponentEditPart;
import org.eclipse.wazaabi.engine.core.editparts.WidgetEditPart;
import org.eclipse.wazaabi.engine.core.views.WidgetView;
import org.eclipse.wazaabi.engine.swt.views.SWTWidgetViewFactory;

public class OutlineWidgetViewFactory extends SWTWidgetViewFactory {

	public WidgetView createWidgetView(WidgetEditPart editPart,
			Object creationHint) {
		if (editPart instanceof TextComponentEditPart)
			return new OutlineTextComponentView();
		return CoreSingletons.getComposedWidgetViewFactory().createWidgetView(
				editPart, creationHint);
	}

}
