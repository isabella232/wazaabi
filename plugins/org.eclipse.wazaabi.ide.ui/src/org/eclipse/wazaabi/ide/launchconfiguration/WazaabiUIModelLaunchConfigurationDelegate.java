/*******************************************************************************
 * Copyright (c) 2013 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.launchconfiguration;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.pde.launching.OSGiLaunchConfigurationDelegate;

public class WazaabiUIModelLaunchConfigurationDelegate extends
		OSGiLaunchConfigurationDelegate {

	public static final String ID_WAZAABI_APPLICATION = "org.eclipse.wazaabi.launching.wazaabiApplication"; //$NON-NLS-1$

	public void launch(ILaunchConfiguration configuration, String mode,
			ILaunch launch, IProgressMonitor monitor) throws CoreException {
		//launch.addDebugTarget(new WazaabiDebugTarget());
		super.launch(configuration, mode, launch, monitor);

	}

}
