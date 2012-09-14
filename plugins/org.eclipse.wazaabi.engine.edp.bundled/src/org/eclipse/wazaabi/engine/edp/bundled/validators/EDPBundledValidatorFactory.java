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

import org.eclipse.wazaabi.engine.edp.validators.BundledValidator;
import org.eclipse.wazaabi.engine.edp.validators.BundledValidatorFactory;



public class EDPBundledValidatorFactory implements BundledValidatorFactory {

	public boolean isFactoryFor(Object context, Object source) {
		if (source instanceof String)
			return true;
		return false;
	}

	public String getFactoryID() {
		return getClass().getName();
	}

	
	public BundledValidator createBundledValidator(Object context, String id) {
		if("bundledIsIntValidator".equals(id))
			return new BundledIsIntValidator();
		else if ("bundledIsStringValidator".equals(id))
			return new BundledIsStringValidator();
		else if ("bundledSourceTargetSizesValidator".equals(id))
			return new BundledSourceTargetSizesValidator();
		else if("testBundledBasicValidator".equals(id))
			return new TestBundledBasicValidator();
		
//		if ("bundledHelloStringValidator".equals(id))
//			return new BundledHelloStringValidator();
//		else if ("bundledIntToStringValidator".equals(id))
//			return new BundledIntToStringValidator();
//		else if ("bundledStringToIntValidator".equals(id))
//			return new BundledStringToIntValidator();
		return null;
	}

}