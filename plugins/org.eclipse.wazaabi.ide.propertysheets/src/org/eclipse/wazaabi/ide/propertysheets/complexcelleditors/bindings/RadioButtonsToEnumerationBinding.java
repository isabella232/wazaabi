package org.eclipse.wazaabi.ide.propertysheets.complexcelleditors.bindings;

import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.wazaabi.ide.propertysheets.viewer.TargetChangeListener;

public class RadioButtonsToEnumerationBinding extends AbstractBinding {

	public static final String ENUMERATION_VALUE_KEY = "EnumerationValueKey";

	@Override
	protected void addListeners(final Control control) {

		if (control instanceof Composite)
			for (Control child : ((Composite) control).getChildren())
				if (child instanceof Button
						&& child.getData(ENUMERATION_VALUE_KEY) != null)
					((Button) child)
							.addSelectionListener(new SelectionAdapter() {
								@Override
								public void widgetSelected(SelectionEvent e) {
									Object newValue = e.widget
											.getData(ENUMERATION_VALUE_KEY);
									Object domainValue = getDomainValue(control);
									if (newValue.equals(domainValue))
										return;
									TargetChangeListener listener = getTargetChangeListener(control);
									if (listener != null)
										listener.targetModified(
												getDomainObject(control),
												getEStructuralFeature(control),
												-1, domainValue, newValue);
								}
							});
	}

	@Override
	protected Object convertToExpectedValue(Object value) {
		return null;
	}

	@Override
	public void refresh(Control control) {
		Object domainValue = getDomainValue(control);
		if (domainValue == null)
			return;
		if (control instanceof Composite) {
			for (Control child : ((Composite) control).getChildren())
				if (child instanceof Button)
					((Button) child).setSelection(domainValue.equals(child
							.getData(ENUMERATION_VALUE_KEY)));
		}

	}
}
