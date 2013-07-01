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

package org.eclipse.wazaabi.engine.swt.forms.editparts;

public class ContainerEditPart extends
		org.eclipse.wazaabi.engine.core.editparts.ContainerEditPart {

	public static final String HEADER_IMAGE = "header-image"; // $NON-NLS-1$
	public static final String FORM_DECORATE_FORM_HEADING = "form-decorate-form-heading"; // $NON-NLS-1$

	public void refreshFeaturesAndStyles() {
		super.refreshFeaturesAndStyles();
		refreshUniqueStyleRule(HEADER_IMAGE);
		refreshUniqueStyleRule(FORM_DECORATE_FORM_HEADING);
	}

}
