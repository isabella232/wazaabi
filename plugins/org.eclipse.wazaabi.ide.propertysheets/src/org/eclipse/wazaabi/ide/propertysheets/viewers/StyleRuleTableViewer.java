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

package org.eclipse.wazaabi.ide.propertysheets.viewers;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.wazaabi.ide.propertysheets.descriptors.AbstractDescriptorFactory;
import org.eclipse.wazaabi.ide.propertysheets.descriptors.StyleRuleDescriptorFactory;
import org.eclipse.wazaabi.ide.propertysheets.viewers.DescriptorLabelColumn.LabelPrinter;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.impl.BlankRuleImpl;

public class StyleRuleTableViewer extends AbstractTableViewer {

	public final BlankRuleImpl RULE_FOR_INSERTION = new BlankRuleImpl() {
	};

	@Override
	public String getLabel() {
		return "Styles";
	}

	@Override
	protected AbstractDescriptorFactory createAbstractDescriptorFactory() {
		return new StyleRuleDescriptorFactory();
	}

	@Override
	protected EObject getBlankRow() {
		return RULE_FOR_INSERTION;
	}

	@Override
	protected IContentProvider getContentProvider() {
		return new StyleRuleContentProvider((StyleRule) getBlankRow());
	}

	@Override
	protected LabelPrinter getLabelPrinter() {
		return new LabelPrinter() {

			@Override
			public String getLabel(EObject item) {
				return ((StyleRule) item).getPropertyName();
			}
		};
	}

}