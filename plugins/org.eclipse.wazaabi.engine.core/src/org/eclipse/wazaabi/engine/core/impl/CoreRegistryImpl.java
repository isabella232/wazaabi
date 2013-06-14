package org.eclipse.wazaabi.engine.core.impl;

import org.eclipse.wazaabi.engine.core.annotations.factories.AnnotationManagerFactory;
import org.eclipse.wazaabi.engine.core.annotations.managers.AnnotationManager;
import org.eclipse.wazaabi.engine.core.celleditors.factories.CellEditorFactory;
import org.eclipse.wazaabi.engine.core.editparts.AbstractWidgetEditPart.StyleRuleManager;
import org.eclipse.wazaabi.engine.core.editparts.factories.EditPartFactory;
import org.eclipse.wazaabi.engine.core.gef.EditPart;
import org.eclipse.wazaabi.engine.core.stylerules.factories.StyleRuleManagerFactory;
import org.eclipse.wazaabi.engine.core.views.WidgetView;
import org.eclipse.wazaabi.engine.core.views.factories.WidgetViewFactory;
import org.eclipse.wazaabi.engine.edp.impl.EDPRegistryImpl;
import org.eclipse.wazaabi.mm.core.extras.CellEditor;

public class CoreRegistryImpl extends EDPRegistryImpl {

	@Override
	protected Class<?> getServiceInterfacerFor(Class<?> returnedType) {
		if (EditPart.class.equals(returnedType))
			return EditPartFactory.class;
		if (WidgetView.class.equals(returnedType))
			return WidgetViewFactory.class;
		if (AnnotationManager.class.equals(returnedType))
			return AnnotationManagerFactory.class;
		if (StyleRuleManager.class.equals(returnedType))
			return StyleRuleManagerFactory.class;
		if (CellEditor.class.equals(returnedType))
			return CellEditorFactory.class;
		return super.getServiceInterfacerFor(returnedType);
	}

}
