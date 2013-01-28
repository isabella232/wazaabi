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
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class TestBundledBasicValidator implements BundledValidator {

	boolean disposed = true;

	public TestBundledBasicValidator() {
		System.out.println("creating " + getClass().getName());
		disposed = false;
	}

	public boolean isValid(EventDispatcher a, EventHandler b) {
		System.out.println("Bundled validation ongoing");
			return true;
	}

	public void dispose() {
		System.out.println("disposing " + getClass().getName());
		disposed = true;
	}

	public boolean isDisposed() {
		return disposed;
	}

	public boolean isValidatorFor(Class<?> input) {
		// TODO Auto-generated method stub
		return false;
	}

	public String getErrorMessage() {
		return null;
	}

}
