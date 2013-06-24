package org.eclipse.wazaabi.engine.swt.forms.views;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.wazaabi.mm.swt.descriptors.SWTDescriptorsPackage;

public class SWTContainerView extends
		org.eclipse.wazaabi.engine.swt.commons.views.SWTContainerView {

	private FormToolkit formToolkit;

	public EClass getWidgetViewEClass() {
		return SWTDescriptorsPackage.Literals.COMPOSITE;
	}

	protected Widget createSWTWidget(Widget parent, int swtStyle, int index) {
		// for (StyleRule rule : ((StyledElement) getHost().getModel())
		// .getStyleRules()) {
		// if (rule instanceof BarLayoutRuleImpl
		// && ContainerEditPart.LAYOUT_PROPERTY_NAME.equals(rule
		// .getPropertyName())) {

		formToolkit = new FormToolkit(parent.getDisplay());
		Form form = formToolkit
				.createForm((org.eclipse.swt.widgets.Composite) parent);
		form.setText("Hello, Eclipse Forms");
		return form;
	}

	protected FormToolkit getFormToolkit() {
		return formToolkit;
	}

	@Override
	protected void widgetDisposed() {
		super.widgetDisposed();
		if (formToolkit != null)
			formToolkit.dispose();
	}

	@Override
	public Widget getContentPane() {
		return ((Form) getSWTWidget()).getBody();
	}

}