/*******************************************************************************
 * Copyright (c) 2012 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.ui.editparts;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.swt.graphics.Image;
import org.eclipse.wazaabi.ide.ui.internal.Activator;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class EventHandlerTreeEditPart extends AbstractTreeEditPart {

	@SuppressWarnings("rawtypes")
	@Override
	protected List getModelChildren() {
		List<EObject> kids = new ArrayList<EObject>();
		kids.addAll(((EventHandler) getModel()).getParameters());
		kids.addAll(((EventHandler) getModel()).getEvents());
		return kids;
	}

	protected Image getImage() {
		return Activator.getDefault().getImageRegistry()
				.get(((EObject) getModel()).eClass().getName());
	}

	@Override
	protected String getText() {
		String text = ((EventHandler) getModel()).getUri();
		return text != null ? text : "";
	}

}
