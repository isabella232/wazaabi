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

import java.util.List;

import org.eclipse.wazaabi.engine.edp.EDP;
import org.eclipse.wazaabi.engine.edp.validators.BundledValidator;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class BundledSourceTargetSizesValidator implements BundledValidator {

	public boolean validate(EventDispatcher eventDispatcher,
			EventHandler eventHandler) {
		Object source = eventDispatcher.get(EDP.VALUE_SOURCE_KEY);
		Object target = eventDispatcher.get(EDP.VALUE_TARGET_KEY);
		if (source instanceof List) {
			if (target instanceof List)
				if (((List<?>) source).size() == ((List<?>) target).size())
					System.out
							.println("Bundled validator executed: source and target sizes ok");
			return true;
		} else {
			return false;
		}

	}

	public String errorMessage() {
		return "Input error: source and target are not same size";
	}

	public void dispose() {
		System.out.println("disposing " + getClass().getName());
	}

	public boolean isValidatorFor(Class<?> input) {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean isDisposed() {
		// TODO Auto-generated method stub
		return false;
	}

}
