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

package org.eclipse.wazaabi.engine.swt.views;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.wazaabi.engine.core.editparts.SeparatorEditPart;
import org.eclipse.wazaabi.engine.core.views.SeparatorView;
import org.eclipse.wazaabi.mm.core.Orientation;
import org.eclipse.wazaabi.mm.core.styles.OrientationRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.swt.descriptors.SWTDescriptorsPackage;

public class SWTSeparatorView extends SWTControlView implements SeparatorView {

	public EClass getWidgetViewEClass() {
		return SWTDescriptorsPackage.Literals.SEPARATOR;
	}

	protected Widget createSWTWidget(Widget parent, int swtStyle, int index) {
		Label label = new Label((org.eclipse.swt.widgets.Composite) parent,
				computeSWTCreationStyle(getHost()) | SWT.SEPARATOR);
		return checkParentLayout((Composite) parent, label);
	}

	@Override
	public boolean needReCreateWidgetView(StyleRule styleRule, org.eclipse.swt.widgets.Widget widget) {
		if (styleRule == null)
			return false;
		if (SeparatorEditPart.ORIENTATION_PROPERTY_NAME.equals(styleRule
				.getPropertyName()) && styleRule instanceof OrientationRule) {
			return !(isStyleBitCorrectlySet(widget,
					org.eclipse.swt.SWT.HORIZONTAL,
					Orientation.HORIZONTAL == ((OrientationRule) styleRule)
							.getValue()) & isStyleBitCorrectlySet(widget,
					org.eclipse.swt.SWT.VERTICAL,
					Orientation.VERTICAL == ((OrientationRule) styleRule)
							.getValue()));
		} else
			return super.needReCreateWidgetView(styleRule, widget);
	}

	protected int computeSWTCreationStyle(StyleRule rule) {
		final String propertyName = rule.getPropertyName();
		if (rule instanceof OrientationRule
				&& SeparatorEditPart.ORIENTATION_PROPERTY_NAME
						.equals(propertyName))
			if (((OrientationRule) rule).getValue() == Orientation.HORIZONTAL)
				return SWT.HORIZONTAL;
			else
				return SWT.VERTICAL;
		return super.computeSWTCreationStyle(rule);
	}
}
