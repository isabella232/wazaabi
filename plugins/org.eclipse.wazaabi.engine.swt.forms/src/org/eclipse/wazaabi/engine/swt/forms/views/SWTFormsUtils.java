/*******************************************************************************
 * Copyright (c) 2013 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.engine.swt.forms.views;

import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.wazaabi.engine.core.editparts.ContainerEditPart;
import org.eclipse.wazaabi.engine.core.editparts.WidgetEditPart;
import org.eclipse.wazaabi.engine.core.gef.EditPart;
import org.eclipse.wazaabi.engine.swt.commons.views.SWTWidgetView;

public class SWTFormsUtils {

	/**
	 * Looks up into containment hierarchy of the given EditPart and returns the
	 * SWTContainer which owns (or will own) a {@link Form} widget if exists.
	 * Returns null otherwise.
	 * 
	 * @param host
	 *            A non null <code>EditPart</code>
	 * @return A SWTContainer if found, null otherwise
	 */
	public static SWTContainerView getSWTContainer(WidgetEditPart host) {
		EditPart parent = host;
		while ((parent = parent.getParent()) != null)
			if (parent instanceof ContainerEditPart
					&& ((ContainerEditPart) parent).getWidgetView() instanceof SWTContainerView)
				return (SWTContainerView) ((ContainerEditPart) parent)
						.getWidgetView();
		return null;
	}

	/**
	 * Returns true only if this host is a child of a container whose WidgetView
	 * is a {@link Form}
	 * 
	 * @param host
	 * @return
	 */
	public static boolean isDirectChildOfForm(WidgetEditPart host) {
		if (host != null
				&& host.getParent() instanceof ContainerEditPart
				&& ((SWTWidgetView) ((ContainerEditPart) host.getParent())
						.getWidgetView()).getSWTWidget() instanceof Form)
			return true;
		return false;
	}

}
