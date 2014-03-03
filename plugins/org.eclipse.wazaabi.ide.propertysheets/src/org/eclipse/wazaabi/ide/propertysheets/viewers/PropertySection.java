package org.eclipse.wazaabi.ide.propertysheets.viewers;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

public interface PropertySection {

	void refresh();

	void setInput(Object input);

	public void createControls(Composite parent);

	public void dispose();

	public String getLabel();
	
	public Control getControl();

}
