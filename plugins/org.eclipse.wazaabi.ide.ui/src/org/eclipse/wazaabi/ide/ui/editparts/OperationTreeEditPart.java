package org.eclipse.wazaabi.ide.ui.editparts;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.swt.graphics.Image;
import org.eclipse.wazaabi.ide.ui.internal.Activator;
import org.eclipse.wazaabi.mm.edp.handlers.Operation;

public class OperationTreeEditPart extends AbstractTreeEditPart {

	protected Image getImage() {
		return Activator.getDefault().getImageRegistry()
				.get(((EObject) getModel()).eClass().getName());
	}

	@Override
	protected String getText() {
		String text = ((Operation) getModel()).getUri();
		if (text == null)
			return "";
		int idx = text.lastIndexOf('.');
		if (idx != -1)
			text = text.substring(idx + 1);
		return text;
	}
}
