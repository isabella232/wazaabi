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

package org.eclipse.wazaabi.engine.core.editparts;

import org.eclipse.wazaabi.engine.core.gef.EditPart;
import org.eclipse.wazaabi.engine.core.views.WidgetView;
import org.eclipse.wazaabi.engine.edp.adapters.EventDispatcherAdapter;

public interface WidgetEditPart extends EditPart, EventDispatcherAdapter {

	/**
	 * Returns the WidgetView controlled by this WidgetEditPart.
	 * 
	 * @return The controlled WidgetView.
	 */
	public WidgetView getWidgetView();

	/**
	 * Removes this WidgetEditPart's WidgetView and recreate it at the same
	 * place. If this WidgetEditPart have children, they are removed and
	 * recreated also. This method refreshes the WidgetView (and its children if
	 * they exist). Note that it is the responsibility of the caller to call
	 * validate on its container. We do not call validate inside this method to
	 * let the caller to add its own code before the validation of the parent.
	 */
	@Deprecated
	public void renewVisuals();

//	public FeatureAdapterManager getFeatureAdapterManager(
//			EStructuralFeature feature);

}
