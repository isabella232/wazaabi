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

package org.eclipse.wazaabi.ide.ui.propertysheets;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.wazaabi.ide.ui.internal.Activator;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;

public class UISelector {

	public static final String URI_PREFIX = "platform:/plugin/" //$NON-NLS-1$
			+ Activator.PLUGIN_ID + "/UIs/propertypages/"; //$NON-NLS-1$

	public AbstractComponent getUi(Object element, Object uiId) {
		// System.out.println("getUI(" + uiId + ")");
		return null;
	}

	public String getUiURI(Object element, Object uiId) {
		if (!(element instanceof EObject))
			return null;
		return buildURI(((EObject) element).eClass().getName() + "PropertyPage");
	}

	// public Object getUiId(Object element) {
	// return "toto";
	// }

	protected String buildURI(String pageId) {
		return URI_PREFIX + pageId + ".ui";
	}

	protected boolean match(EClass eClass, String packageNsURI, String className) {
		if (eClass == null)
			return false;
		return eClass.getEPackage().getNsURI().equals(packageNsURI)
				&& eClass.getName().equals(className);
	}
}
