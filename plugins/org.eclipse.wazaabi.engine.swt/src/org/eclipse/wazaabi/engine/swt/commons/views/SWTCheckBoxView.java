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
import org.eclipse.swt.widgets.Widget;
import org.eclipse.wazaabi.engine.core.editparts.WidgetEditPart;
import org.eclipse.wazaabi.engine.core.views.CheckBoxView;
import org.eclipse.wazaabi.mm.core.widgets.CheckBox;
import org.eclipse.wazaabi.mm.swt.descriptors.SWTDescriptorsPackage;

public class SWTCheckBoxView extends AbstractSWTButtonView implements
		CheckBoxView {

	private SelectionListener selectionListener = new SelectionListener() {

		public void widgetSelected(SelectionEvent e) {
			setUserSelection();
		}

		public void widgetDefaultSelected(SelectionEvent e) {
			setUserSelection();
		}
	};

	public SelectionListener getSelectionListener() {
		return selectionListener;
	}

	protected void setUserSelection() {
		boolean selection = ((org.eclipse.swt.widgets.Button) getSWTWidget())
				.getSelection();
		if (selection != ((CheckBox) getHost().getModel()).isSelected())
			((CheckBox) getHost().getModel()).setSelected(selection);
	}

	@Override
	protected Widget createSWTWidget(Widget parent, int swtStyle, int index) {
		Widget w = super.createSWTWidget(parent, swtStyle, index);
		if (w instanceof org.eclipse.swt.widgets.Button)
			((org.eclipse.swt.widgets.Button) w)
					.addSelectionListener(getSelectionListener());
		return w;
	}

	public EClass getWidgetViewEClass() {
		return SWTDescriptorsPackage.Literals.CHECK_BOX;
	}

	@Override
	protected int computeSWTCreationStyle(WidgetEditPart editPart) {
		return super.computeSWTCreationStyle(editPart) | SWT.CHECK;
	}

	public void setSelected(boolean selected) {
		((org.eclipse.swt.widgets.Button) getSWTWidget())
				.setSelection(selected);
	}

	public boolean isSelected() {
		return ((org.eclipse.swt.widgets.Button) getSWTWidget()).getSelection();
	}

}
