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

package org.eclipse.wazaabi.engine.swt.forms.editparts;

import org.eclipse.wazaabi.engine.core.editparts.factories.CoreEditPartFactory;
import org.eclipse.wazaabi.engine.core.gef.EditPart;
import org.eclipse.wazaabi.engine.swt.forms.views.SWTFormsUtils;
import org.eclipse.wazaabi.mm.core.widgets.Container;

public class SWTFormsEditPartFactory extends CoreEditPartFactory {

	// private final Logger logger = LoggerFactory
	// .getLogger(CoreEditPartFactory.class);

	public static final String EDITPART_FACTORY_ID = SWTFormsEditPartFactory.class
			.getName();

	@Override
	protected EditPart getPartForElement(Object modelElement) {

		if (modelElement instanceof Container
				&& SWTFormsUtils
						.ancestorOrSelfIsAForm((Container) modelElement))
			return new ContainerEditPart();
		return super.getPartForElement(modelElement);
	}

	@Override
	public String getFactoryID() {
		return EDITPART_FACTORY_ID;
	}

}
