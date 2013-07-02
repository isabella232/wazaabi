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

public class LabelEditPart extends
		org.eclipse.wazaabi.engine.core.editparts.LabelEditPart {

	public static final String RENDER_XML = "render-xml"; //$NON-NLS-1$

	protected void refreshFeaturesAndStyles() {
		super.refreshFeaturesAndStyles();
		refreshUniqueStyleRule(RENDER_XML);
	}
}
