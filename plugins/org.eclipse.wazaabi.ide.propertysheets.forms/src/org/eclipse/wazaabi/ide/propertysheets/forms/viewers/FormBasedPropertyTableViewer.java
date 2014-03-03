package org.eclipse.wazaabi.ide.propertysheets.forms.viewers;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.wazaabi.ide.propertysheets.viewers.PropertySection;
import org.eclipse.wazaabi.ide.propertysheets.viewers.TargetChangeListener;

public class FormBasedPropertyTableViewer implements PropertySection {

	private Composite container;

	@Override
	public void addTargetChangeListener(TargetChangeListener listener) {
		// TODO Auto-generated method stub

	}

	@Override
	public void removeTargetChangeListener(TargetChangeListener listener) {
		// TODO Auto-generated method stub

	}

	@Override
	public void refresh() {
		// TODO Auto-generated method stub

	}

	@Override
	public void setInput(Object input) {
		// TODO Auto-generated method stub

	}

	@Override
	public void createControls(Composite parent) {
		container = new Composite(parent, SWT.NONE);
	}

	@Override
	public void dispose() {
		if (container != null && !container.isDisposed())
			container.dispose();
	}

	@Override
	public String getLabel() {
		return "Properties"; //$NON-NLS-1$
	}

	@Override
	public Control getControl() {
		return container;
	}

}
