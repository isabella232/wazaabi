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

import org.eclipse.wazaabi.engine.edp.EDP;
import org.eclipse.wazaabi.engine.edp.validators.BundledValidator;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class BundledIsStringValidator implements BundledValidator {

	public boolean isValid(EventDispatcher eventDispatcher, EventHandler eventHandler) {
		Object input = eventDispatcher.get(EDP.VALUE_SOURCE_KEY);
		return (input instanceof Integer);
	}

	public boolean isValidatorFor(Class<?> input) {
		if (input == Integer.class)
			return true;
		return false;

	}

	public boolean isDisposed() {
		return false;
	}

	public void dispose() {
	}

	public String getErrorMessage() {
		return null;
	}

}
