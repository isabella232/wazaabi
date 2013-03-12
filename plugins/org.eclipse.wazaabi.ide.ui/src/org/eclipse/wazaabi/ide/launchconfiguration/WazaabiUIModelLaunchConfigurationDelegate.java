package org.eclipse.wazaabi.ide.launchconfiguration;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.pde.launching.OSGiLaunchConfigurationDelegate;

public class WazaabiUIModelLaunchConfigurationDelegate extends
		OSGiLaunchConfigurationDelegate {

	public void launch(ILaunchConfiguration configuration, String mode,
			ILaunch launch, IProgressMonitor monitor) throws CoreException {
		super.launch(configuration, mode, launch, monitor);
		//
		// IVMRunner runner = vm.getVMRunner(mode);
		//
		// String[] classpath = getClasspath(configuration);
		//
		// VMRunnerConfiguration runConfig = new VMRunnerConfiguration(
		// "org.eclipse.wazaabi.ide.launchconfiguration.WazaabiModelDisplay",
		// classpath);
		//
		//
		// // Bootpath
		// // String[] bootpath = getBootpath(configuration);
		// // runConfig.setBootClassPath(bootpath);
		//
		// // Launch the configuration
		// // this.fCurrentLaunchConfiguration = configuration;
		// runner.run(runConfig, launch, monitor);

	}

}
