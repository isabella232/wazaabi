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

package org.eclipse.wazaabi.engine.locationpaths.runtime;

import java.io.StringReader;

import org.eclipse.wazaabi.engine.edp.PathException;
import org.eclipse.wazaabi.engine.locationpaths.model.LocationPath;
import org.eclipse.wazaabi.engine.locationpaths.parser.ParseException;
import org.eclipse.wazaabi.engine.locationpaths.parser.TokenMgrError;

public class LocationParser {

	private static org.eclipse.wazaabi.engine.locationpaths.parser.LocationParser parser = new org.eclipse.wazaabi.engine.locationpaths.parser.LocationParser(
			new StringReader("")); //$NON-NLS-1$

	public static LocationPath parse(String locationPath) {
		synchronized (parser) {
			LocationPath parsedLocationPath = null;
			try {
				parser.ReInit(new StringReader(locationPath));
				parsedLocationPath = parser.LocationPath();
			} catch (TokenMgrError e) {
				throw new PathException(e.getMessage());
			} catch (ParseException e) {
				throw new PathException("Invalid location path: '"
						+ locationPath
						+ "'. Syntax error "
						+ describePosition(locationPath,
								e.currentToken.beginColumn));
			}
			return parsedLocationPath;
		}
	}

	/**
	 * Describes a parse position.
	 * 
	 * @param expression
	 *            to parse
	 * @param position
	 *            parse position
	 * @return String
	 */
	private static String describePosition(String expression, int position) {
		if (position <= 0) {
			return "at the beginning of the expression";
		}
		if (position >= expression.length()) {
			return "- expression incomplete";
		}
		return "after: '" + expression.substring(0, position) + "'";
	}

}
