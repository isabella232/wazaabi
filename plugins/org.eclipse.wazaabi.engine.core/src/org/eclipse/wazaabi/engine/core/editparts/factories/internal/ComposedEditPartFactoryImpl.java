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

package org.eclipse.wazaabi.engine.core.editparts.factories.internal;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.wazaabi.engine.core.editparts.factories.ComposedEditPartFactory;
import org.eclipse.wazaabi.engine.core.editparts.factories.EditPartFactory;
import org.eclipse.wazaabi.engine.core.gef.EditPart;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ComposedEditPartFactoryImpl implements ComposedEditPartFactory {

	final static Logger logger = LoggerFactory
			.getLogger(ComposedEditPartFactoryImpl.class);

	private Collection<EditPartFactory> editPartFactories = new ArrayList<EditPartFactory>();

	// TODO : add some synchronized ???

	public void addEditPartFactory(EditPartFactory editPartFactory) {
		logger.info("Adding {}", editPartFactory);
		editPartFactories.add(editPartFactory);
	}

	public void removeEditPartFactory(EditPartFactory editPartFactory) {
		logger.info("Removing {}", editPartFactory);
		editPartFactories.remove(editPartFactory);
	}

	public EditPart createEditPart(EditPart context, Object model) {
		for (EditPartFactory editPartFactory : editPartFactories) {
			EditPart editPart = editPartFactory.createEditPart(context, model);
			if (editPart != null)
				return editPart;
		}
		return null;
	}

}
