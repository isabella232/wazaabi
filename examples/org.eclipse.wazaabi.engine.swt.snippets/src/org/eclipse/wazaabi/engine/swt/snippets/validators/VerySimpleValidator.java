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

package org.eclipse.wazaabi.engine.swt.snippets.validators;

import org.eclipse.wazaabi.mm.core.widgets.TextComponent;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class VerySimpleValidator {

	public VerySimpleValidator() {
		System.out.println("creating " + getClass().getName());
	}

	public boolean isValid(TextComponent eventDispatcher,
			EventHandler eventHandler) {
		if ("Hello World".equals(eventDispatcher.getText()))
			return true;
		return false;
	}

//	public String getErrorMessage() {
//		return "Input error: please type 'hello'";
//	}

	public void dispose() {
		System.out.println("disposing " + getClass().getName());
	}
}
