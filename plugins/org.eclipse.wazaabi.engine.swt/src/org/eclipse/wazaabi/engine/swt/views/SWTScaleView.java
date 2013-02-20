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
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.wazaabi.engine.core.editparts.AbstractComponentEditPart;
import org.eclipse.wazaabi.engine.core.editparts.ScaleEditPart;
import org.eclipse.wazaabi.engine.core.views.ScaleView;
import org.eclipse.wazaabi.mm.core.Orientation;
import org.eclipse.wazaabi.mm.core.styles.BooleanRule;
import org.eclipse.wazaabi.mm.core.styles.IntRule;
import org.eclipse.wazaabi.mm.core.styles.OrientationRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.widgets.Scale;
import org.eclipse.wazaabi.mm.swt.descriptors.SWTDescriptorsPackage;

public class SWTScaleView extends SWTControlView implements ScaleView {

	private SelectionListener selectionListener = new SelectionListener() {
		public void widgetSelected(SelectionEvent e) {
			int newValue = ((org.eclipse.swt.widgets.Scale) e.widget)
					.getSelection();
			if (((Scale) getHost().getModel()).getValue() != newValue)
				((Scale) getHost().getModel()).setValue(newValue);
		}

		public void widgetDefaultSelected(SelectionEvent e) {
		}
	};

	protected SelectionListener getSelectionListener() {
		return this.selectionListener;
	}

	public EClass getWidgetViewEClass() {
		return SWTDescriptorsPackage.Literals.SCALE;
	}

	@Override
	protected boolean needReCreateWidgetView(StyleRule rule, org.eclipse.swt.widgets.Widget widget) {
		if (rule instanceof OrientationRule
				&& ScaleEditPart.ORIENTATION_PROPERTY_NAME.equals(rule
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
				&& AbstractComponentEditPart.BORDER_PROPERTY_NAME.equals(rule
						.getPropertyName()))
			return false;
		else
			return super.needReCreateWidgetView(rule, widget);
	}

	@Override
	protected int computeSWTCreationStyle(StyleRule rule) {
		final String propertyName = rule.getPropertyName();
		if (rule instanceof OrientationRule
				&& ScaleEditPart.ORIENTATION_PROPERTY_NAME
				.equals(propertyName))
			if (((OrientationRule) rule).getValue() == Orientation.HORIZONTAL)
				return SWT.HORIZONTAL;
			else
				return SWT.VERTICAL;
		// we catch the border rule since apparently this SWT widget does not
		// manage it
		else if (rule instanceof BooleanRule
				&& ScaleEditPart.BORDER_PROPERTY_NAME.equals(rule
						.getPropertyName()))
			return SWT.None;
		return super.computeSWTCreationStyle(rule);
	}

	protected Widget createSWTWidget(Widget parent, int swtStyle, int index) {
		int style = computeSWTCreationStyle(getHost());
		// if no orientation has been given, we set it by default to HORIZONTAL
		if ((style & SWT.HORIZONTAL) == 0 && ((style & SWT.VERTICAL) == 0))
			style |= SWT.HORIZONTAL;

		final org.eclipse.swt.widgets.Scale scale = new org.eclipse.swt.widgets.Scale(
				(org.eclipse.swt.widgets.Composite) parent, style);
		if (getSelectionListener() != null)
			scale.addSelectionListener(getSelectionListener());

		return wrapForSpecificParen((Composite) parent, scale);
	}

	public void setValue(int value) {
		((org.eclipse.swt.widgets.Scale) getSWTWidget()).setSelection(value);
	}

	public int getValue() {
		return ((org.eclipse.swt.widgets.Scale) getSWTWidget()).getSelection();
	}

	// FIXME : rename this ugly named method !!
	protected void widgetDisposed() {
		super.widgetDisposed();
		if (getSWTControl() != null && !getSWTControl().isDisposed()
				&& getSelectionListener() != null)
			((org.eclipse.swt.widgets.Scale) getSWTControl())
			.removeSelectionListener(getSelectionListener());
	}

	@Override
	public void updateStyleRule(StyleRule rule) {
		if (rule == null)
			return;
		if (ScaleEditPart.MAXIMUM_PROPERTY_NAME.equals(rule.getPropertyName()))
			if (rule instanceof IntRule)
				setMaximum((IntRule) rule);
			else
				setMaximum(null);
		else if (ScaleEditPart.MINIMUM_PROPERTY_NAME.equals(rule
				.getPropertyName()))
			if (rule instanceof IntRule)
				setMinimum((IntRule) rule);
			else
				setMinimum(null);
		else if (ScaleEditPart.INCREMENT_PROPERTY_NAME.equals(rule
				.getPropertyName()))
			if (rule instanceof IntRule)
				setIncrement((IntRule) rule);
			else
				setIncrement(null);
		else if (ScaleEditPart.PAGEINCREMENT_PROPERTY_NAME.equals(rule
				.getPropertyName()))
			if (rule instanceof IntRule)
				setPageIncrement((IntRule) rule);
			else
				setPageIncrement(null);

		else
			super.updateStyleRule(rule);
	}

	protected void setMaximum(IntRule rule) {
		int currentValue = ((org.eclipse.swt.widgets.Scale) getSWTControl())
				.getMaximum();
		if (rule == null && currentValue != 100)
			((org.eclipse.swt.widgets.Scale) getSWTControl()).setMaximum(100);
		else
			((org.eclipse.swt.widgets.Scale) getSWTControl()).setMaximum(rule
					.getValue());
	}

	protected void setMinimum(IntRule rule) {
		int currentValue = ((org.eclipse.swt.widgets.Scale) getSWTControl())
				.getMinimum();
		if (rule == null && currentValue != 0)
			((org.eclipse.swt.widgets.Scale) getSWTControl()).setMinimum(0);
		else
			((org.eclipse.swt.widgets.Scale) getSWTControl()).setMinimum(rule
					.getValue());
	}

	protected void setIncrement(IntRule rule) {
		int currentValue = ((org.eclipse.swt.widgets.Scale) getSWTControl())
				.getIncrement();
		if (rule == null && currentValue != 1)
			((org.eclipse.swt.widgets.Scale) getSWTControl()).setIncrement(1);
		else
			((org.eclipse.swt.widgets.Scale) getSWTControl()).setIncrement(rule
					.getValue());
	}

	protected void setPageIncrement(IntRule rule) {
		int currentValue = ((org.eclipse.swt.widgets.Scale) getSWTControl())
				.getIncrement();
		if (rule == null && currentValue != 10)
			((org.eclipse.swt.widgets.Scale) getSWTControl()).setPageIncrement(10);
		else
			((org.eclipse.swt.widgets.Scale) getSWTControl()).setPageIncrement(rule
					.getValue());
	}

}
