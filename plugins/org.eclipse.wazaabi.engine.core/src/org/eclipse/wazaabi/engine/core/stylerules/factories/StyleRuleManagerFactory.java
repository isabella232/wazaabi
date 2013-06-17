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

package org.eclipse.wazaabi.engine.core.stylerules.factories;

import org.eclipse.wazaabi.engine.edp.ComponentFactory;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;

public interface StyleRuleManagerFactory extends ComponentFactory {

	public void platformSpecificRefresh(Object context, StyleRule rule);

	public void platformSpecificUpdate(Object context, StyleRule rule);

	/**
	 * Converts this style rule data into a platform specific object. For
	 * instance, a specific layout rule will be converted into a SWT layout
	 * object.
	 * 
	 * @param rule
	 *            A rule to convert
	 * 
	 * @param context
	 *            An Object representing the platform specific context where the
	 *            style rule check is processed.
	 * 
	 * @return An platform specific object if the conversion succeeded, null
	 *         otherwise.
	 */
	public Object convertIntoPlatformSpecificObject(Object context,
			StyleRule rule);

}
