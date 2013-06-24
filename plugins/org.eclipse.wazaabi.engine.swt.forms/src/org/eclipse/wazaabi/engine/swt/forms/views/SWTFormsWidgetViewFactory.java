package org.eclipse.wazaabi.engine.swt.forms.views;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.wazaabi.engine.core.gef.EditPart;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;

public class SWTFormsWidgetViewFactory extends
		org.eclipse.wazaabi.engine.swt.commons.views.SWTWidgetViewFactory {

	// private final Logger logger = LoggerFactory
	// .getLogger(SWTWidgetViewFactory.class);

	public static final String FACTORY_ID = SWTFormsWidgetViewFactory.class
			.getName();

	@Override
	public Object createComponent(Object callingContext, Object model,
			Object creationHint) {
		if (model instanceof EditPart
				&& ((EditPart) model).getModel() instanceof EObject) {
			EClass eClass = ((EObject) ((EditPart) model).getModel()).eClass();

			if (eClass == CoreWidgetsPackage.Literals.TEXT_COMPONENT)
				return new SWTTextComponentView();
			if (eClass == CoreWidgetsPackage.Literals.CONTAINER)
				return new SWTContainerView();
			
			return super.createComponent(callingContext, model, creationHint);
		}
		return null;
	}

	@Override
	public String getFactoryID() {
		return FACTORY_ID;
	}

}