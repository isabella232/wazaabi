/*******************************************************************************
 * Copyright (c) 2014 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.propertysheets.forms.viewers;

import org.eclipse.wazaabi.ide.propertysheets.editinghelpers.EditingHelperFactory;
import org.eclipse.wazaabi.ide.propertysheets.forms.editinghelpers.FormBasedEditingHelperFactory;
import org.eclipse.wazaabi.ide.propertysheets.viewers.stylerules.StyleRuleTableViewer;

public class FormBasedStyleRuleTableViewer extends StyleRuleTableViewer {

	protected EditingHelperFactory createEditingHelperFactory() {
		return new FormBasedEditingHelperFactory();
	}
}