package test123.handlers;

import org.eclipse.e4.ui.model.application.ui.basic.MPart;
import org.eclipse.e4.ui.workbench.modeling.ESelectionService;
import org.eclipse.e4.ui.workbench.modeling.ISelectionListener;
import org.eclipse.wazaabi.engine.core.CoreUtils;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class DetailsInitializeHandler {

	protected class InternalSelectionListener implements ISelectionListener {
		private final EventDispatcher eventDispatcher;

		public InternalSelectionListener(final EventDispatcher eventDispatcher) {
			this.eventDispatcher = eventDispatcher;
		}

		@Override
		public void selectionChanged(MPart part, Object selection) {
			eventDispatcher.set("input", selection);
			CoreUtils.deepRefresh((AbstractComponent) eventDispatcher);
		}

	};

	public void execute(EventDispatcher eventDispatcher,
			EventHandler eventHandler, Event event) {
		ESelectionService selectionService = (ESelectionService) eventDispatcher
				.get("SelectionService");
		if (selectionService != null) {
			selectionService
					.addSelectionListener(new InternalSelectionListener(
							eventDispatcher));
		}
	}
}