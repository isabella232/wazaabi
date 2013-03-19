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

package org.eclipse.wazaabi.ide.ui.editors.actions;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.gef.TreeEditPart;
import org.eclipse.gef.ui.actions.SelectionAction;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.pde.core.plugin.IPluginModelBase;
import org.eclipse.pde.core.plugin.PluginRegistry;
import org.eclipse.pde.internal.core.DependencyManager;
import org.eclipse.pde.internal.launching.PDELaunchingPlugin;
import org.eclipse.pde.launching.IPDELauncherConstants;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.wazaabi.ide.launchconfiguration.WazaabiUIModelLaunchConfigurationDelegate;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.osgi.framework.Version;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class RunInSeparateWindow extends SelectionAction {
	private final static Logger logger = LoggerFactory
			.getLogger(RunInSeparateWindow.class);

	public static final String ID = "RunInSeparateWindow"; //$NON-NLS-1$
	public static final String BOOTSTART_PLUGIN_NAME = "org.eclipse.wazaabi.debug.ui"; //$NON-NLS-1$

	public RunInSeparateWindow(IWorkbenchPart part) {
		super(part);
		setLazyEnablementCalculation(false);
	}

	protected void init() {
		setText("Run In Separate Window...");
		setToolTipText("RunInSeparateWindow");
		setId(ID);
		// ImageDescriptor icon = AbstractUIPlugin.imageDescriptorFromPlugin(
		// "TutoGEF", "icons/rename-icon.png");
		// if (icon != null)
		// setImageDescriptor(icon);
		setEnabled(false);
	}

	@Override
	protected boolean calculateEnabled() {
		List<?> selectedObjects = getSelectedObjects();
		if (selectedObjects.size() == 1
				&& selectedObjects.get(0) instanceof TreeEditPart) {
			TreeEditPart tep = (TreeEditPart) getSelectedObjects().get(0);
			if (tep.getModel() instanceof AbstractComponent)
				return true;
		}
		return false;
	}

	public void run() {
		AbstractComponent rootModel = (AbstractComponent) EcoreUtil
				.copy((AbstractComponent) ((TreeEditPart) getSelectedObjects()
						.get(0)).getModel());
		lauchSeparateViewer(rootModel);
		Thread t = new WaitAndSendRootModelThread(rootModel, "localhost", 8080);
		t.start();
	}

	@SuppressWarnings("restriction")
	public void lauchSeparateViewer(AbstractComponent rootModel) {
		ILaunchManager manager = DebugPlugin.getDefault().getLaunchManager();

		ILaunchConfigurationType type = manager
				.getLaunchConfigurationType(WazaabiUIModelLaunchConfigurationDelegate.ID_WAZAABI_APPLICATION);
		try {
			ILaunchConfiguration[] configurations = manager
					.getLaunchConfigurations(type);
			for (int i = 0; i < configurations.length; i++) {
				ILaunchConfiguration configuration = configurations[i];
				if (configuration.getName().equals("Start Wazaabi")) {
					configuration.delete();
					break;
				}
			}
			ILaunchConfigurationWorkingCopy wc = type.newInstance(null,
					"Start Wazaabi");
			PDELaunchingPlugin.getDefault().getOSGiFrameworkManager()
					.getDefaultInitializer().initialize(wc);

			String vmArgs = wc.getAttribute(
					IJavaLaunchConfigurationConstants.ATTR_VM_ARGUMENTS, "");
			vmArgs += " -DdisplayService -Dorg.osgi.service.http.port=8080";
			wc.setAttribute(
					IJavaLaunchConfigurationConstants.ATTR_VM_ARGUMENTS, vmArgs);

			List<IPluginModelBase> requiredPlugins = new ArrayList<IPluginModelBase>();

			IPluginModelBase wazaabiServicePlugin = getLastVersionOfPluginModelBase(BOOTSTART_PLUGIN_NAME);
			if (wazaabiServicePlugin == null)
				logger.error("{} not found", BOOTSTART_PLUGIN_NAME);
			else {
				requiredPlugins.add(wazaabiServicePlugin);
				requiredPlugins.addAll(getPluginsRequiredByModel(rootModel));
			}
			initializePluginsList(requiredPlugins, wc);
			wc.setAttribute(IPDELauncherConstants.AUTOMATIC_ADD, false);
			ILaunchConfiguration configuration = wc.doSave();

			DebugUITools.launch(configuration, ILaunchManager.DEBUG_MODE);

			System.out.println("launched");
		} catch (CoreException e) {
			e.printStackTrace();
		}
	}

	private final static String PREFIX = "platform:/plugin/"; //$NON-NLS-1$

	protected List<IPluginModelBase> getPluginsRequiredByModel(
			AbstractComponent rootModel) {
		List<IPluginModelBase> requiredPlugins = new ArrayList<IPluginModelBase>();
		TreeIterator<EObject> treeIterator = EcoreUtil.getAllContents(
				rootModel, true);
		List<String> pluginSymbolicNames = new ArrayList<String>();
		while (treeIterator.hasNext()) {
			EObject object = treeIterator.next();
			if (object.eClass().equals(
					EDPHandlersPackage.Literals.EVENT_HANDLER)) {
				EventHandler eh = (EventHandler) object;
				if (eh.getUri() != null && !eh.getUri().isEmpty()
						&& eh.getUri().startsWith(PREFIX)) {
					String pluginSymbolicName = eh.getUri().substring(
							PREFIX.length());
					int idx = pluginSymbolicName.indexOf('/');
					if (idx != -1) {
						pluginSymbolicName = pluginSymbolicName.substring(0,
								idx);
						if (!pluginSymbolicNames.contains(pluginSymbolicName))
							pluginSymbolicNames.add(pluginSymbolicName);
					}
				}
			}
		}
		for (String pluginSymbolicName : pluginSymbolicNames) {
			IPluginModelBase requiredPlugin = getLastVersionOfPluginModelBase(pluginSymbolicName);
			if (requiredPlugin != null
					&& !requiredPlugins.contains(requiredPlugins)) {
				requiredPlugins.add(requiredPlugin);
				logger.debug("Added {} to the launch configuration",
						pluginSymbolicName);
			} else
				logger.error("Unable to find {}", pluginSymbolicName);

		}
		return requiredPlugins;
	}

	protected IPluginModelBase getLastVersionOfPluginModelBase(
			String bundleSymbolicName) {

		IPluginModelBase pluginModelBase = getLastVersionOfPluginModelBase(
				bundleSymbolicName, PluginRegistry.getWorkspaceModels());
		if (pluginModelBase != null)
			return pluginModelBase;
		if (pluginModelBase == null)
			pluginModelBase = getLastVersionOfPluginModelBase(
					bundleSymbolicName, PluginRegistry.getExternalModels());

		return pluginModelBase;
	}

	protected IPluginModelBase getLastVersionOfPluginModelBase(
			String bundleSymbolicName, IPluginModelBase[] models) {
		if (bundleSymbolicName == null || bundleSymbolicName.isEmpty())
			return null;
		IPluginModelBase result = null;
		for (IPluginModelBase pluginModelBase : models) {
			if (pluginModelBase.getBundleDescription() != null
					&& bundleSymbolicName.equals(pluginModelBase
							.getBundleDescription().getSymbolicName()))
				if (result == null)
					result = pluginModelBase;
				else {
					Version resultVersion = result.getBundleDescription() != null ? result
							.getBundleDescription().getVersion() : null;
					Version currentVersion = pluginModelBase
							.getBundleDescription().getVersion();
					if (resultVersion == null) {
						if (currentVersion != null)
							result = pluginModelBase;
					} else {
						if (currentVersion != null
								&& resultVersion.compareTo(currentVersion) < 1)
							result = pluginModelBase;
					}

				}
		}
		return result;
	}

	@SuppressWarnings("restriction")
	protected void initializePluginsList(
			List<IPluginModelBase> pluginModelBases,
			ILaunchConfigurationWorkingCopy wc) {
		if (pluginModelBases == null || pluginModelBases.isEmpty())
			return;
		StringBuffer wsplugins = new StringBuffer();
		StringBuffer explugins = new StringBuffer();

		Set<String> plugins = new TreeSet<String>();
		for (IPluginModelBase pluginModelBase : pluginModelBases) {

			// exclude "org.eclipse.ui.workbench.compatibility" - it is only
			// needed
			// for pre-3.0 bundles
			@SuppressWarnings("unchecked")
			Set<String> intermediateResult = (Set<String>) DependencyManager
					.getSelfAndDependencies(
							pluginModelBase,
							new String[] { "org.eclipse.ui.workbench.compatibility" }); //$NON-NLS-1$
			plugins.addAll(intermediateResult);

		}

		for (String id : plugins) {
			IPluginModelBase model = PluginRegistry.findModel(id);
			if (model == null || !model.isEnabled())
				continue;
			if (model.getUnderlyingResource() == null) {
				appendPlugin(explugins, model);
			} else {
				appendPlugin(wsplugins, model);
			}
		}

		wc.setAttribute(IPDELauncherConstants.WORKSPACE_BUNDLES,
				wsplugins.toString());
		wc.setAttribute(IPDELauncherConstants.TARGET_BUNDLES,
				explugins.toString());
	}

	private void appendPlugin(StringBuffer buffer, IPluginModelBase model) {
		if (buffer.length() > 0)
			buffer.append(',');
		buffer.append(model.getPluginBase().getId());
	}
}