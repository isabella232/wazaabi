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

package org.eclipse.wazaabi.engine.edp;

import org.eclipse.wazaabi.engine.edp.internal.osgi.Activator;
import org.eclipse.wazaabi.engine.edp.nonosgi.NonOSGILogger;

public class Logger {

	public static final int ERROR = 1;
	public static final int WARNING = 2;
	public static final int INFO = 3;
	public static final int DEBUG = 4;

	public static void log(String name, int level, String message) {
		log(name, level, message, null);
	}

	public static void log(String name, int level, String message,
			Throwable exception) {
		if (Activator.getDefault() != null)
			Activator.log(name, level, message, exception);
		else
			NonOSGILogger.log(name, level, message, exception);
	}
}
