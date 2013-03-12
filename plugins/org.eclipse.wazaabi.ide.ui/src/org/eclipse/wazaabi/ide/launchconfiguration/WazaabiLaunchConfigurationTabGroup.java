package org.eclipse.wazaabi.ide.launchconfiguration;

import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.ui.AbstractLaunchConfigurationTabGroup;
import org.eclipse.debug.ui.CommonTab;
import org.eclipse.debug.ui.EnvironmentTab;
import org.eclipse.debug.ui.ILaunchConfigurationDialog;
import org.eclipse.debug.ui.ILaunchConfigurationTab;
import org.eclipse.jdt.debug.ui.launchConfigurations.JavaArgumentsTab;
import org.eclipse.pde.internal.launching.PDELaunchingPlugin;
import org.eclipse.pde.internal.launching.launcher.OSGiFrameworkManager;
import org.eclipse.pde.ui.launcher.BundlesTab;
import org.eclipse.pde.ui.launcher.OSGiSettingsTab;
import org.eclipse.pde.ui.launcher.TracingTab;

@SuppressWarnings("restriction")
public class WazaabiLaunchConfigurationTabGroup extends
		AbstractLaunchConfigurationTabGroup {

	public void createTabs(ILaunchConfigurationDialog dialog, String mode) {
		ILaunchConfigurationTab[] tabs = new ILaunchConfigurationTab[] {
				new BundlesTab(), new JavaArgumentsTab(),
				new OSGiSettingsTab(), new TracingTab(), new EnvironmentTab(),
				new CommonTab() };
		setTabs(tabs);
	}

	public void setDefaults(ILaunchConfigurationWorkingCopy configuration) {
		super.setDefaults(configuration);
		OSGiFrameworkManager manager = PDELaunchingPlugin.getDefault()
				.getOSGiFrameworkManager();
		manager.getDefaultInitializer().initialize(configuration);
	}

}
