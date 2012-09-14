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

import java.util.List;

import org.eclipse.emf.ecore.EClassifier;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.wazaabi.engine.locationpaths.model.LiteralExpression;

public class QueryContextResolver {

	private static final String ECLASSIFIER_FUNTION_NAME = "eClassifier"; //$NON-NLS-1$

	public static Object resolveInitialContext(String functionName, List args) {
		if (ECLASSIFIER_FUNTION_NAME.equals(functionName) && args != null
				&& args.size() == 2 && args.get(0) instanceof LiteralExpression
				&& args.get(1) instanceof LiteralExpression) {
			return getEClassifier(((LiteralExpression) args.get(0)).getValue(),
					((LiteralExpression) args.get(1)).getValue());
		}
		return null;
	}

	private static EClassifier getEClassifier(String ePackageNsURI,
			String eClassifierName) {

		if (ePackageNsURI != null && !"".equals(ePackageNsURI)) { //$NON-NLS-1$
			EPackage ePackage = EPackage.Registry.INSTANCE
					.getEPackage(ePackageNsURI);
			if (ePackage != null)
				return ePackage.getEClassifier(eClassifierName);
		}
		return null;
	}
}
