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

package org.eclipse.wazaabi.engine.edp.executables;

import org.eclipse.wazaabi.engine.edp.coderesolution.ExecutableAdapter;
import org.eclipse.wazaabi.mm.edp.handlers.Executable;

public interface ComposedExecutableAdapterFactory extends ExecutableAdapterFactory {
	public void addExecutableAdapterFactory(ExecutableAdapterFactory factory);

	public void removeExecutableAdapterFactory(ExecutableAdapterFactory factory);

	public ExecutableAdapter createExecutableAdapter(
			Object context,
			Executable executable);

}
