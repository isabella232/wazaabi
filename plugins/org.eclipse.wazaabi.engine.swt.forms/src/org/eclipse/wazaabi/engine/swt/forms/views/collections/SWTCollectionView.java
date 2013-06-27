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

package org.eclipse.wazaabi.engine.swt.forms.views.collections;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.wazaabi.engine.swt.forms.views.SWTContainerView;

public class SWTCollectionView
		extends
		org.eclipse.wazaabi.engine.swt.commons.views.collections.SWTCollectionView {

	private final SWTContainerView containingForm;

	public SWTCollectionView(SWTContainerView containingForm) {
		this.containingForm = containingForm;
	}

	/**
	 * private for avoiding the use of this constructor
	 */
	@SuppressWarnings("unused")
	private SWTCollectionView() {
		this.containingForm = null;
	}

	@Override
	protected Composite createLayoutHolder(Composite parent, int style) {
		if (containingForm == null || containingForm.getFormToolkit() == null)
			return super.createLayoutHolder(parent, style);
		return containingForm.getFormToolkit().createComposite(
				(org.eclipse.swt.widgets.Composite) parent, SWT.NONE);
	}

	@Override
	protected Combo createCombo(Composite parent, int style) {
		return super.createCombo(parent, style);
	}

	@Override
	protected Table createTable(Composite parent, int style) {
		if (containingForm == null || containingForm.getFormToolkit() == null)
			return super.createTable(parent, style);
		return containingForm.getFormToolkit().createTable(parent, style);
	}

	@Override
	protected Tree createTree(Composite parent, int style) {
		if (containingForm == null || containingForm.getFormToolkit() == null)
			return super.createTree(parent, style);
		return containingForm.getFormToolkit().createTree(parent, style);
	}

}
