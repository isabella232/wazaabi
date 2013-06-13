package org.eclipse.wazaabi.engine.core;

import org.eclipse.wazaabi.engine.core.annotations.factories.AnnotationManagerFactory;
import org.eclipse.wazaabi.engine.core.annotations.managers.AnnotationManager;
import org.eclipse.wazaabi.engine.core.editparts.factories.EditPartFactory;
import org.eclipse.wazaabi.engine.core.gef.EditPart;
import org.eclipse.wazaabi.engine.core.views.WidgetView;
import org.eclipse.wazaabi.engine.core.views.factories.WidgetViewFactory;
import org.eclipse.wazaabi.engine.edp.EDPFactoryImpl;

public class CoreRegistry extends EDPFactoryImpl {

	@Override
	protected Class<?> getServiceInterfacerFor(Class<?> returnedType) {
		if (EditPart.class.equals(returnedType))
			return EditPartFactory.class;
		if (WidgetView.class.equals(returnedType))
			return WidgetViewFactory.class;
		if (AnnotationManager.class.equals(returnedType))
			return AnnotationManagerFactory.class;
		return super.getServiceInterfacerFor(returnedType);
	}

}
