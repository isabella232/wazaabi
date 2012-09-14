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

package org.eclipse.wazaabi.engine.swt.views;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.wazaabi.engine.core.editparts.ProgressBarEditPart;
import org.eclipse.wazaabi.engine.core.views.ProgressBarView;
import org.eclipse.wazaabi.mm.core.Orientation;
import org.eclipse.wazaabi.mm.core.styles.BooleanRule;
import org.eclipse.wazaabi.mm.core.styles.IntRule;
import org.eclipse.wazaabi.mm.core.styles.OrientationRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.swt.descriptors.SWTDescriptorsPackage;

public class SWTProgressBarView extends SWTControlView implements
		ProgressBarView {

	public EClass getWidgetViewEClass() {
		return SWTDescriptorsPackage.Literals.PROGRESS_BAR;
	}

	@Override
	public boolean needReCreateWidgetView(StyleRule rule) {
		org.eclipse.swt.widgets.Widget widget = getSWTWidget();
		if (rule instanceof OrientationRule
				&& ProgressBarEditPart.ORIENTATION_PROPERTY_NAME.equals(rule
						.getPropertyName())) {
			return !(isStyleBitCorrectlySet(widget,
					org.eclipse.swt.SWT.HORIZONTAL,
					Orientation.HORIZONTAL == ((OrientationRule) rule)
							.getValue()) & isStyleBitCorrectlySet(widget,
					org.eclipse.swt.SWT.VERTICAL,
					Orientation.VERTICAL == ((OrientationRule) rule).getValue()));
			// we catch the border rule since apparently this SWT widget does
			// not manage it
		} else if (rule instanceof BooleanRule
				&& ProgressBarEditPart.BORDER_PROPERTY_NAME.equals(rule
						.getPropertyName()))
			return false;
		else
			return super.needReCreateWidgetView(rule);
	}

	@Override
	protected int computeSWTCreationStyle(StyleRule rule) {
		final String propertyName = rule.getPropertyName();
		if (rule instanceof OrientationRule
				&& ProgressBarEditPart.ORIENTATION_PROPERTY_NAME
						.equals(propertyName))
			if (((OrientationRule) rule).getValue() == Orientation.HORIZONTAL)
				return SWT.HORIZONTAL;
			else
				return SWT.VERTICAL;
		// we catch the border rule since apparently this SWT widget does not
		// manage it
		else if (rule instanceof BooleanRule
				&& ProgressBarEditPart.BORDER_PROPERTY_NAME.equals(rule
						.getPropertyName()))
			return SWT.None;
		return super.computeSWTCreationStyle(rule);
	}

	protected Widget createSWTWidget(Widget parent, int swtStyle, int index) {
		int style = computeSWTCreationStyle(getHost());
		// if no orientation has been given, we set it by default to HORIZONTAL
		if ((style & SWT.HORIZONTAL) == 0 && ((style & SWT.VERTICAL) == 0))
			style |= SWT.HORIZONTAL;
		org.eclipse.swt.widgets.ProgressBar progressBar = new org.eclipse.swt.widgets.ProgressBar(
				(org.eclipse.swt.widgets.Composite) parent, style);
		return checkParentLayout((Composite) parent,progressBar);
	}

	public void setValue(int value) {
		((org.eclipse.swt.widgets.ProgressBar) getSWTWidget())
				.setSelection(value);
	}

	public int getValue() {
		return ((org.eclipse.swt.widgets.ProgressBar) getSWTWidget())
				.getSelection();
	}

	@Override
	public void updateStyleRule(StyleRule rule) {
		if (rule == null)
			return;
		if (ProgressBarEditPart.MAXIMUM_PROPERTY_NAME.equals(rule.getPropertyName()))
			if (rule instanceof IntRule)
				setMaximum((IntRule) rule);
			else
				setMaximum(null);
		else if (ProgressBarEditPart.MINIMUM_PROPERTY_NAME.equals(rule
				.getPropertyName()))
			if (rule instanceof IntRule)
				setMinimum((IntRule) rule);
			else
				setMinimum(null);
		else
			super.updateStyleRule(rule);
	}

	protected void setMaximum(IntRule rule) {
		int currentValue = ((org.eclipse.swt.widgets.ProgressBar) getSWTControl())
				.getMaximum();
		if (rule == null && currentValue != 100)
			((org.eclipse.swt.widgets.ProgressBar) getSWTControl()).setMaximum(100);
		else
			((org.eclipse.swt.widgets.ProgressBar) getSWTControl()).setMaximum(rule
					.getValue());
	}

	protected void setMinimum(IntRule rule) {
		int currentValue = ((org.eclipse.swt.widgets.ProgressBar) getSWTControl())
				.getMinimum();
		if (rule == null && currentValue != 0)
			((org.eclipse.swt.widgets.ProgressBar) getSWTControl()).setMinimum(0);
		else
			((org.eclipse.swt.widgets.ProgressBar) getSWTControl()).setMinimum(rule
					.getValue());
	}
}
