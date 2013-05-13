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

package org.eclipse.wazaabi.engine.swt.snippets.validators;

import org.eclipse.wazaabi.mm.core.widgets.TextComponent;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Less5CharsValidator {

	private static final Logger logger = LoggerFactory
			.getLogger(Less5CharsValidator.class);

	public Less5CharsValidator() {
		logger.debug("creating {}", getClass().getName());
	}

	public boolean isValid(TextComponent eventDispatcher,
			EventHandler eventHandler) {
		// Object source = eventDispatcher.get(EDP.VALUE_SOURCE_KEY);
		// Object target = eventDispatcher.get(EDP.VALUE_TARGET_KEY);
		logger.debug("isValid()={}", eventDispatcher.getText().length() < 5);
		return eventDispatcher.getText().length() < 5;

	}

	public String getErrorMessage() {
		return "Content length must be less than 5 characters";
	}

	public void dispose() {
		System.out.println("disposing " + getClass().getName());
	}
}
