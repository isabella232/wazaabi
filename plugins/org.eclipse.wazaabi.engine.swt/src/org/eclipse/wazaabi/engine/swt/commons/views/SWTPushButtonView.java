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
import org.eclipse.wazaabi.engine.core.editparts.WidgetEditPart;
import org.eclipse.wazaabi.engine.core.views.PushButtonView;
import org.eclipse.wazaabi.mm.swt.descriptors.SWTDescriptorsPackage;

public class SWTPushButtonView extends AbstractSWTButtonView implements
		PushButtonView {

	public EClass getWidgetViewEClass() {
		return SWTDescriptorsPackage.Literals.PUSH_BUTTON;
	}

	@Override
	protected int computeSWTCreationStyle(WidgetEditPart editPart) {
		return super.computeSWTCreationStyle(editPart) | SWT.PUSH;
	}

}
