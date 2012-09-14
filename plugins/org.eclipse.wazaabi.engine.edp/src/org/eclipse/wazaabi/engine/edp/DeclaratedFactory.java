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

public interface DeclaratedFactory {

	/**
	 * Returns true if this factory is a factory for this source in the given
	 * context.
	 * 
	 * @param context
	 * @param object
	 * @return
	 */
	public boolean isFactoryFor(Object context, Object source);

	public String getFactoryID();

}
