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

package org.eclipse.wazaabi.ui.runtime.parts.views;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.wazaabi.engine.core.editparts.WidgetEditPart;
import org.eclipse.wazaabi.engine.core.views.WidgetView;
import org.eclipse.wazaabi.engine.core.views.factories.WidgetViewFactory;
import org.eclipse.wazaabi.engine.swt.commons.viewers.AbstractSWTViewer;
import org.eclipse.wazaabi.engine.swt.commons.views.SWTContainerView;
import org.eclipse.wazaabi.ui.model.parts.PartsPackage;

public class PartsWidgetViewFactory implements WidgetViewFactory {

	public WidgetView createWidgetView(WidgetEditPart editPart,
			Object creationHint) {
		if (editPart != null && editPart.getModel() instanceof EObject) {
			EClass eClass = ((EObject) editPart.getModel()).eClass();
			if (eClass == PartsPackage.Literals.PAGE)
				return new SWTContainerView();
		}
		return null;
	}

	public boolean isFactoryFor(Object type) {
		if (type instanceof AbstractSWTViewer)
			return true;
		return false;
	}

}
