package org.eclipse.wazaabi.engine.swt.forms.views;

import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.wazaabi.engine.core.editparts.ContainerEditPart;
import org.eclipse.wazaabi.engine.core.gef.EditPart;

public class SWTTextComponentView extends
		org.eclipse.wazaabi.engine.swt.commons.views.SWTTextComponentView {

	protected FormToolkit getFormToolkit() {
		EditPart parent = null;

		while ((parent = getHost().getParent()) != null) {
			if (parent instanceof ContainerEditPart
					&& ((ContainerEditPart) parent).getWidgetView() instanceof SWTContainerView)
				return ((SWTContainerView) ((ContainerEditPart) parent)
						.getWidgetView()).getFormToolkit();
		}
		return null;
	}

	protected Widget createSWTWidget(Widget parent, int swtStyle, int index) {
		FormToolkit formToolkit = getFormToolkit();
		if (formToolkit == null)
			return null;
		final Text text = formToolkit.createText(
				(org.eclipse.swt.widgets.Composite) parent, null,
				computeSWTCreationStyle(getHost()));
		if (getModifyListener() != null)
			text.addModifyListener(getModifyListener());
		return text;
	}

}
