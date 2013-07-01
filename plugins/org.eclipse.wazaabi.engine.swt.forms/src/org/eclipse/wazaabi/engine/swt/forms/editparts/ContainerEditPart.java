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
	public static final String DECORATE_FORM_HEADING = "decorate-form-heading"; // $NON-NLS-1$
	public static final String EXPANSION_TOGGLE = "expansion-toggle"; //$NON-NLS-1$
	public static final String DESCRIPTION = "description"; //$NON-NLS-1$
	public static final String EXPANDED = "expanded"; //$NON-NLS-1$
	public static final String TITLE_BAR = "title-bar"; //$NON-NLS-1$
	public static final String SHORT_TITLE_BAR = "short-title-bar"; //$NON-NLS-1$
	public static final String CLIENT_INDENT = "client-indent"; //$NON-NLS-1$
	public static final String COMPACT = "compact"; //$NON-NLS-1$

	public void refreshFeaturesAndStyles() {
		super.refreshFeaturesAndStyles();
		refreshUniqueStyleRule(HEADER_IMAGE);
		refreshUniqueStyleRule(DECORATE_FORM_HEADING);
		refreshUniqueStyleRule(EXPANSION_TOGGLE);
		refreshUniqueStyleRule(DESCRIPTION);
		refreshUniqueStyleRule(EXPANDED);
		refreshUniqueStyleRule(TITLE_BAR);
		refreshUniqueStyleRule(SHORT_TITLE_BAR);
		refreshUniqueStyleRule(CLIENT_INDENT);
		refreshUniqueStyleRule(COMPACT);
	}

}
