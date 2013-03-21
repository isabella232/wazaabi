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
		//
		// List<String> commandList = new ArrayList<String>();
		// // SocketUtil.findFreePort()
		//
		// // IProcess process= newProcess(launch, p,
		// renderProcessLabel(cmdLine), getDefaultProcessMap());
		//
		//
		//
		// // if in debug mode, add debug arguments - i.e. '-debug requestPort
		// // eventPort'
		// int requestPort = -1;
		// int eventPort = -1;
		// if (mode.equals(ILaunchManager.DEBUG_MODE)) {
		// requestPort = findFreePort();
		// eventPort = findFreePort();
		// if (requestPort == -1 || eventPort == -1) {
		// // abort("Unable to find free port", null);
		// }
		// commandList.add("-debug");
		// commandList.add("" + requestPort);
		// commandList.add("" + eventPort);
		// }
		// //
		// // String[] commandLine = (String[]) commandList
		// // .toArray(new String[commandList.size()]);
		// //
		// // Process process = DebugPlugin.exec(commandLine, null);
		// // IProcess p = DebugPlugin.newProcess(launch, process,
		// // configuration.getName());
		// //
		// // // if in debug mode, create a debug target
		// // if (mode.equals(ILaunchManager.DEBUG_MODE)) {
		// // IDebugTarget target = new WazaabiDebugTarget(
		// // configuration.getName(), launch, p, requestPort, eventPort);
		// // launch.addDebugTarget(target);
		// // }
		// configuration.

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
