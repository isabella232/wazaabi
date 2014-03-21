package org.eclipse.wazaabi.ide.propertysheets.forms.viewers;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.wazaabi.ide.propertysheets.PropertySection;
import org.eclipse.wazaabi.ide.propertysheets.TargetChangeListener;

public class FormBasedPropertyTableViewer implements PropertySection {

	private Composite container;

	public void addTargetChangeListener(TargetChangeListener listener) {
		// TODO Auto-generated method stub

	}

	public void removeTargetChangeListener(TargetChangeListener listener) {
		// TODO Auto-generated method stub

	}

	public void refresh() {
		// TODO Auto-generated method stub

	}

	public void setInput(Object input) {
		// TODO Auto-generated method stub

	}

	public void createControls(Composite parent) {
		container = new Composite(parent, SWT.NONE);
	}

	public void dispose() {
		if (container != null && !container.isDisposed())
			container.dispose();
	}

	public String getLabel() {
		return "Properties"; //$NON-NLS-1$
	}

	public Control getControl() {
		return container;
	}

}
