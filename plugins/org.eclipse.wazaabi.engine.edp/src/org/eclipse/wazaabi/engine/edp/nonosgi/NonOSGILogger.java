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

package org.eclipse.wazaabi.engine.edp.nonosgi;

import org.eclipse.wazaabi.engine.edp.Logger;

public class NonOSGILogger {

	public static void log(String name, int level, String message,
			Throwable exception) {
		java.util.logging.Logger logger = java.util.logging.Logger
				.getLogger(name);
		if (logger != null)
			switch (level) {
			case Logger.DEBUG:
				logger.log(java.util.logging.Level.ALL, message, exception);
				break;
			case Logger.ERROR:
				logger.log(java.util.logging.Level.SEVERE, message, exception);
				break;
			case Logger.INFO:
				logger.log(java.util.logging.Level.INFO, message, exception);
				break;
			case Logger.WARNING:
				logger.log(java.util.logging.Level.WARNING, message, exception);
				break;
			}
	}

}
