package org.eclipse.wazaabi.ide.ui.editors.actions;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.InetSocketAddress;
import java.net.MalformedURLException;
import java.net.ProtocolException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.URL;
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
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.ecore.xmi.XMIResource;
import org.eclipse.emf.ecore.xmi.impl.XMIResourceImpl;
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
		setText("RunInSeparateWindow...");
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
		// try {
		// Socket socket0 = new Socket("localhost", 10000);
		// socket0.getOutputStream().write("open\r\n".getBytes());
		// socket0.close();
		// Socket socket = new Socket("localhost", 10000);
		// XMIResource r = new XMIResourceImpl();
		// r.getContents().add(rootModel);
		// r.save(socket.getOutputStream(), null);
		// socket.close();
		// } catch (IOException e) {
		// e.printStackTrace();
		// }
		// getWorkbenchPart().getSite().getShell().getDisplay()
		// .syncExec(new Runnable() {
		//
		// public void run() {
		// test();
		//
		// }
		// });
		// test();
		Thread t = new Thread() {

			protected boolean isServerListening(String server, int port,
					int sleepingDelay, int nbrLoops) {
				boolean isServerListening = false;
				for (int i = 0; i < nbrLoops; i++) {
					final Socket sock = new Socket();
					try {
						sock.connect(new InetSocketAddress(server, port), 500);
						if (sock.isConnected())
							isServerListening = true;
					} catch (IOException e) {
						e.printStackTrace();
					} finally {
						try {
							sock.close();
						} catch (IOException e) {
						}
					}
					if (isServerListening)
						break;
					try {
						sleep(sleepingDelay);
					} catch (InterruptedException e) {
						e.printStackTrace();
					}
				}
				return isServerListening;
			}

			public void run() {
				AbstractComponent rootModel = (AbstractComponent) EcoreUtil
						.copy((AbstractComponent) ((TreeEditPart) getSelectedObjects()
								.get(0)).getModel());

				String server = "localhost";
				int port = 8080;
				if (isServerListening("localhost", 8080, 500, 30)) {
					sendAbstractComponent(rootModel, server, port);
				}
			}

			protected void sendAbstractComponent(AbstractComponent rootModel,
					String server, int port) {
				URL url = null;
				try {
					url = new URL("http://" + server + ":" + port + "/simple");
				} catch (MalformedURLException ex) {
					// NOTHING TO DO HERE
				}
				HttpURLConnection urlConn = null;
				try {
					// URL connection channel.
					urlConn = (HttpURLConnection) url.openConnection();
				} catch (IOException ex) {
					// Logger.getLogger(TestHTTPClient.class.getName()).log(
					// Level.SEVERE, null, ex);
				}

				// Let the run-time system (RTS) know that we want input.
				urlConn.setDoInput(true);

				// Let the RTS know that we want to do output.
				urlConn.setDoOutput(true);

				// No caching, we want the real thing.
				urlConn.setUseCaches(false);

				try {
					urlConn.setRequestMethod("POST");
				} catch (ProtocolException ex) {
					ex.printStackTrace();
					// Logger.getLogger(TestHTTPClient.class.getName()).log(
					// Level.SEVERE, null, ex);
				}

				try {
					urlConn.connect();
				} catch (IOException ex) {
					ex.printStackTrace();
					// Logger.getLogger(TestHTTPClient.class.getName()).log(
					// Level.SEVERE, null, ex);
				}

				DataOutputStream output = null;
				DataInputStream input = null;

				try {
					output = new DataOutputStream(urlConn.getOutputStream());
				} catch (IOException ex) {
					// Logger.getLogger(TestHTTPClient.class.getName()).log(Level.SEVERE,
					// null, ex);
				}

				// Specify the content type if needed.
				// urlConn.setRequestProperty("Content-Type",
				// "application/x-www-form-urlencoded");

				// Construct the POST data.
				// String content = "name=" + URLEncoder.encode("Greg")
				// + "&email="
				// + URLEncoder.encode("greg at code dot geek dot sh");

				XMIResource r = new XMIResourceImpl();
				r.getContents().add(rootModel);

				// Send the request data.
				try {
					r.save(output, null);
					output.flush();
					output.close();
				} catch (IOException ex) {
					// Logger.getLogger(TestHTTPClient.class.getName()).log(Level.SEVERE,
					// null, ex);
				}
				String str = null;
				try {
					input = new DataInputStream(urlConn.getInputStream());
					while (null != ((str = input.readLine()))) {
						System.out.println(str);
					}
					input.close();
				} catch (IOException ex) {
					// Logger.getLogger(TestHTTPClient.class.getName()).log(Level.SEVERE,
					// null, ex);
					ex.printStackTrace();
				}
			}
		};
		t.start();

	}

	public static int findFreePort() {
		int port;
		try {
			ServerSocket socket = new ServerSocket(0);
			port = socket.getLocalPort();
			socket.close();
		} catch (Exception e) {
			port = -1;
		}
		return port;
	}

	@SuppressWarnings("restriction")
	public void test() {
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
			vmArgs += " -DdisplayService.port=10000";
			wc.setAttribute(
					IJavaLaunchConfigurationConstants.ATTR_VM_ARGUMENTS, vmArgs);

			List<IPluginModelBase> requiredPlugins = new ArrayList<IPluginModelBase>();

			IPluginModelBase wazaabiServicePlugin = getLastVersionOfPluginModelBase(BOOTSTART_PLUGIN_NAME);
			if (wazaabiServicePlugin == null)
				logger.error("{} not found", BOOTSTART_PLUGIN_NAME);
			else {
				requiredPlugins.add(wazaabiServicePlugin);
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