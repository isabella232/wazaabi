package org.eclipse.wazaabi.ide.propertysheets.forms.complexcelleditors.events;

import java.util.List;

import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;
import org.eclipse.wazaabi.ide.propertysheets.TargetChangeListener;
import org.eclipse.wazaabi.ide.propertysheets.complexcelleditors.bindings.TextToStringBinding;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersFactory;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage;
import org.eclipse.wazaabi.mm.edp.handlers.Parameter;
import org.eclipse.wazaabi.mm.edp.handlers.StringParameter;

public class TextToFirstStringParameterBinding extends TextToStringBinding {

	private final String name;

	public TextToFirstStringParameterBinding(String name) {
		this.name = name;
	}

	public final String getName() {
		return name;
	}

	@Override
	protected Object convertToExpectedValue(Object value) {
		if ("".equals(value)) //$NON-NLS-1$
			return null;
		return value;
	}

	@Override
	public void refresh(Control control) {
		StringParameter parameter = getFirstStringParameter(control);
		if (parameter != null && parameter.eContainer() != null)
			((Text) control).setText(parameter != null ? parameter.getValue()
					: ""); //$NON-NLS-1$
		else
			((Text) control).setText("");//$NON-NLS-1$
	}

	@SuppressWarnings("unchecked")
	protected StringParameter getFirstStringParameter(Control control) {
		for (Parameter parameter : (List<Parameter>) getDomainValue(control))
			if (getName().equals(parameter.getName())
					&& parameter instanceof StringParameter)
				return (StringParameter) parameter;
		return null;
	}

	@Override
	protected void applyChanges(Control control) {
		String newStringValue = ((Text) control).getText();

		String errorMessage = getErrorMessage(newStringValue);
		StringParameter parameter = getFirstStringParameter(control);
		Object domainValue = parameter != null ? parameter.getValue() : null;

		if (errorMessage == null) {
			String newValue = (String) convertToExpectedValue(newStringValue);
			if (newValue == null) {
				if (domainValue == null)
					return;
			} else if (newValue.equals(domainValue))
				return;
			TargetChangeListener listener = getTargetChangeListener(control);
			if (listener != null) {
				if (newValue == null) {
					listener.targetRemoved(getDomainObject(control), parameter);
				} else if (parameter != null) {
					listener.targetModified(
							parameter,
							EDPHandlersPackage.Literals.STRING_PARAMETER__VALUE,
							-1, domainValue, newValue);
				} else {
					StringParameter newParamater = EDPHandlersFactory.eINSTANCE
							.createStringParameter();
					newParamater.setName(getName());
					newParamater.setValue(newValue);
					listener.targetAdded(getDomainObject(control),
							newParamater, -1);
				}
			}
		} else
			; // TODO

	}

}
