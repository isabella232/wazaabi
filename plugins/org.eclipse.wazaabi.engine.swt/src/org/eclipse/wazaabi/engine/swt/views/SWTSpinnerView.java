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
import org.eclipse.wazaabi.engine.core.editparts.SpinnerEditPart;
import org.eclipse.wazaabi.engine.core.views.SpinnerView;
import org.eclipse.wazaabi.mm.core.styles.BooleanRule;
import org.eclipse.wazaabi.mm.core.styles.IntRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.widgets.Spinner;
import org.eclipse.wazaabi.mm.swt.descriptors.SWTDescriptorsPackage;

public class SWTSpinnerView extends SWTControlView implements SpinnerView {

private SelectionListener selectionListener = new SelectionListener() {
		public void widgetSelected(SelectionEvent e) {
			int newValue = ((org.eclipse.swt.widgets.Spinner) e.widget)
					.getSelection();
			if (((Spinner) getHost().getModel()).getValue() != newValue)
				((Spinner) getHost().getModel()).setValue(newValue);
		}

		public void widgetDefaultSelected(SelectionEvent e) {
		}
	};

	protected SelectionListener getSelectionListener() {
		return this.selectionListener;
	}

	public EClass getWidgetViewEClass() {
		return SWTDescriptorsPackage.Literals.SPINNER;
	}

	@Override
	public boolean needReCreateWidgetView(StyleRule rule) {
		//org.eclipse.swt.widgets.Widget widget = getSWTWidget();
		if (rule instanceof BooleanRule
				&& SpinnerEditPart.BORDER_PROPERTY_NAME.equals(rule
						.getPropertyName()))
			return false;
		else
			return super.needReCreateWidgetView(rule);
	}

	@Override
	protected int computeSWTCreationStyle(StyleRule rule) {
		final String propertyName = rule.getPropertyName();
		if (rule instanceof BooleanRule
				&& SpinnerEditPart.BORDER_PROPERTY_NAME.equals(propertyName))
			return SWT.None;
		return super.computeSWTCreationStyle(rule);
	}

	protected Widget createSWTWidget(Widget parent, int swtStyle, int index) {
		int style = computeSWTCreationStyle(getHost());

		final org.eclipse.swt.widgets.Spinner spinner = new org.eclipse.swt.widgets.Spinner(
				(org.eclipse.swt.widgets.Composite) parent, style);
		if (getSelectionListener() != null)
			spinner.addSelectionListener(getSelectionListener());

		return checkParentLayout((Composite) parent, spinner);
	}

	public void setValue(int value) {
		((org.eclipse.swt.widgets.Spinner) getSWTWidget()).setSelection(value);
	}

	public int getValue() {
		return ((org.eclipse.swt.widgets.Spinner) getSWTWidget()).getSelection();
	}

	// FIXME : rename this ugly named method !!
	protected void widgetDisposed() {
		super.widgetDisposed();
		if (getSWTControl() != null && !getSWTControl().isDisposed()
				&& getSelectionListener() != null)
			((org.eclipse.swt.widgets.Spinner) getSWTControl())
					.removeSelectionListener(getSelectionListener());
	}

	@Override
	public void updateStyleRule(StyleRule rule) {
		if (rule == null)
			return;
		if (SpinnerEditPart.MAXIMUM_PROPERTY_NAME.equals(rule.getPropertyName()))
			if (rule instanceof IntRule)
				setMaximum((IntRule) rule);
			else
				setMaximum(null);
		else if (SpinnerEditPart.MINIMUM_PROPERTY_NAME.equals(rule
				.getPropertyName()))
			if (rule instanceof IntRule)
				setMinimum((IntRule) rule);
			else
				setMinimum(null);
		else if (SpinnerEditPart.INCREMENT_PROPERTY_NAME.equals(rule.getPropertyName()))
			if (rule instanceof IntRule)
				setIncrement((IntRule) rule);
			else
				setIncrement(null);
		else if (SpinnerEditPart.DIGITS_PROPERTY_NAME.equals(rule.getPropertyName()))
			if (rule instanceof IntRule)
				setDigits((IntRule) rule);
			else
				setDigits(null);
		else if (SpinnerEditPart.TEXTLIMIT_PROPERTY_NAME.equals(rule.getPropertyName()))
			if (rule instanceof IntRule)
				setTextLimit((IntRule) rule);
			else
				setTextLimit(null);
		
		else
			super.updateStyleRule(rule);
	}

	protected void setMaximum(IntRule rule) {
		int currentValue = ((org.eclipse.swt.widgets.Spinner) getSWTControl())
				.getMaximum();
		if (rule == null && currentValue != 100)
			((org.eclipse.swt.widgets.Spinner) getSWTControl()).setMaximum(100);
		else
			((org.eclipse.swt.widgets.Spinner) getSWTControl()).setMaximum(rule
					.getValue());
	}

	protected void setMinimum(IntRule rule) {
		int currentValue = ((org.eclipse.swt.widgets.Spinner) getSWTControl())
				.getMinimum();
		if (rule == null && currentValue != 0)
			((org.eclipse.swt.widgets.Spinner) getSWTControl()).setMinimum(0);
		else
			((org.eclipse.swt.widgets.Spinner) getSWTControl()).setMinimum(rule
					.getValue());
	}

	protected void setIncrement(IntRule rule) {
		int currentValue = ((org.eclipse.swt.widgets.Spinner) getSWTControl())
				.getIncrement();
		if (rule == null && currentValue != 1)
			((org.eclipse.swt.widgets.Spinner) getSWTControl()).setIncrement(1);
		else
			((org.eclipse.swt.widgets.Spinner) getSWTControl()).setIncrement(rule
				.getValue());
	}

	protected void setDigits(IntRule rule) {
		int currentValue = ((org.eclipse.swt.widgets.Spinner) getSWTControl())
				.getDigits();
		if (rule == null && currentValue != 0)
			((org.eclipse.swt.widgets.Spinner) getSWTControl()).setDigits(0);
		else
			((org.eclipse.swt.widgets.Spinner) getSWTControl()).setDigits(rule
				.getValue());
	}


	protected void setTextLimit(IntRule rule) {
		int currentValue = ((org.eclipse.swt.widgets.Spinner) getSWTControl()).getTextLimit();
		if (rule == null && currentValue != 0)
			((org.eclipse.swt.widgets.Spinner) getSWTControl()).setDigits(100);
		else
			((org.eclipse.swt.widgets.Spinner) getSWTControl()).setTextLimit(rule
					.getValue());
	}
}
