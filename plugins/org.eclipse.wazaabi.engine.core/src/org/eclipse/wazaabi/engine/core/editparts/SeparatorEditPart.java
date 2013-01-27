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

package org.eclipse.wazaabi.engine.core.editparts;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;

public class SeparatorEditPart extends AbstractComponentEditPart {

	public static final String ORIENTATION_PROPERTY_NAME = "orientation"; //$NON-NLS-1$

	public EClass getModelEClass() {
		return CoreWidgetsPackage.Literals.SEPARATOR;
	}

	protected void refreshFeaturesAndStyles() {
		super.refreshFeaturesAndStyles();
		refreshUniqueStyleRule(ORIENTATION_PROPERTY_NAME);
	}
}
