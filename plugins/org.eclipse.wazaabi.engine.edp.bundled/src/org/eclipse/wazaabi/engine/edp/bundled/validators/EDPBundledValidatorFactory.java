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

package org.eclipse.wazaabi.engine.edp.bundled.validators;

import org.eclipse.wazaabi.engine.edp.validators.BundledValidatorFactory;

public class EDPBundledValidatorFactory implements BundledValidatorFactory {

	public static final String FACTORY_ID = EDPBundledValidatorFactory.class
			.getName();

	public boolean isFactoryFor(Object callingContext, Object model, Object creationHint) {
		if (model instanceof String)
			return true;
		return false;
	}

	public String getFactoryID() {
		return FACTORY_ID;
	}

	@Override
	public Object createComponent(Object callingContext, Object model,
			Object creationHint) {
		if ("bundledIsIntValidator".equals(model))
			return new BundledIsIntValidator();
		else if ("bundledIsStringValidator".equals(model))
			return new BundledIsStringValidator();
		else if ("bundledSourceTargetSizesValidator".equals(model))
			return new BundledSourceTargetSizesValidator();
		else if ("testBundledBasicValidator".equals(model))
			return new TestBundledBasicValidator();
		return null;
	}

}