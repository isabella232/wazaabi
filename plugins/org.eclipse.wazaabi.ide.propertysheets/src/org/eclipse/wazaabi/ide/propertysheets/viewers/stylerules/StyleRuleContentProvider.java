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

package org.eclipse.wazaabi.ide.propertysheets.viewers.stylerules;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.StyledElement;

public class StyleRuleContentProvider implements IStructuredContentProvider {

	private final StyleRule blankRuleforInsertion;

	public StyleRuleContentProvider(StyleRule blankRuleforInsertion) {
		this.blankRuleforInsertion = blankRuleforInsertion;
	}

	public void dispose() {
	}

	public Object[] getElements(Object inputElement) {
		if (inputElement instanceof StyledElement) {
			List<StyleRule> styleRules = new ArrayList<StyleRule>(
					((StyledElement) inputElement).getStyleRules());
			styleRules.add(blankRuleforInsertion);
			return styleRules.toArray();
		}
		return new Object[] {};
	}

	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
	}
}
