package org.eclipse.wazaabi.ide.ui.editors.actions;

import java.io.IOException;
import java.net.Socket;
import java.util.List;

import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.ecore.xmi.XMIResource;
import org.eclipse.emf.ecore.xmi.impl.XMIResourceImpl;
import org.eclipse.gef.TreeEditPart;
import org.eclipse.gef.ui.actions.SelectionAction;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;

public class RunInSeparateWindow extends SelectionAction {

	public static final String ID = "RunInSeparateWindow"; //$NON-NLS-1$

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
		AbstractComponent rootModel = (AbstractComponent) EcoreUtil
				.copy((AbstractComponent) ((TreeEditPart) getSelectedObjects()
						.get(0)).getModel());
		try {
			Socket socket0 = new Socket("localhost", 10000);
			socket0.getOutputStream().write("open\r\n".getBytes());
			socket0.close();
			Socket socket = new Socket("localhost", 10000);
			XMIResource r = new XMIResourceImpl();
			r.getContents().add(rootModel);
			r.save(socket.getOutputStream(), null);
			socket.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

}