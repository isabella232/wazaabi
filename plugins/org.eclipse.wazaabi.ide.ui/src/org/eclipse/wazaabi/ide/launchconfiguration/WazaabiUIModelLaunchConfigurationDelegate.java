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
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.jdt.launching.SocketUtil;
import org.eclipse.pde.launching.OSGiLaunchConfigurationDelegate;
import org.eclipse.wazaabi.ide.debug.model.DisplayProcess;
import org.eclipse.wazaabi.ide.debug.model.WazaabiDebugTarget;

public class WazaabiUIModelLaunchConfigurationDelegate extends
		OSGiLaunchConfigurationDelegate {

	public static final String ID_WAZAABI_APPLICATION = "org.eclipse.wazaabi.launching.wazaabiApplication"; //$NON-NLS-1$

	public static final String DEBUG_PORT = "debugPort"; //$NON-NLS-1$

	public void launch(ILaunchConfiguration configuration, String mode,
			ILaunch launch, IProgressMonitor monitor) throws CoreException {

		if (mode.equals(ILaunchManager.DEBUG_MODE)) {
			int requestPort = SocketUtil.findFreePort();
			ILaunchConfigurationWorkingCopy wc = configuration.getWorkingCopy();

			String vmArgs = wc.getAttribute(
					IJavaLaunchConfigurationConstants.ATTR_VM_ARGUMENTS,
					(String) null);

			if (vmArgs != null) {
				String pattern = "(-D" + DEBUG_PORT + "=\\d+)"; //$NON-NLS-1$  //$NON-NLS-2$
				vmArgs = vmArgs.replaceAll(pattern, ""); //$NON-NLS-1$ 
			}

			wc.setAttribute(
					IJavaLaunchConfigurationConstants.ATTR_VM_ARGUMENTS, vmArgs
							+ " -D" + DEBUG_PORT + "=" //$NON-NLS-1$  //$NON-NLS-2$
							+ Integer.toString(requestPort));
			configuration = wc.doSave();

			IProcess process = new DisplayProcess(launch);
			launch.addDebugTarget(new WazaabiDebugTarget(launch, process,
					requestPort));
		}
		super.launch(configuration, mode, launch, monitor);
	}

}
