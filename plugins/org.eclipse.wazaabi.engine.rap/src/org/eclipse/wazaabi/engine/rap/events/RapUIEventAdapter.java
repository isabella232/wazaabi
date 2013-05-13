package org.eclipse.wazaabi.engine.rap.events;

import org.eclipse.wazaabi.engine.swt.commons.events.AbstractSWTUIEventAdapter;
import org.eclipse.wazaabi.mm.edp.events.Event;

public class RapUIEventAdapter extends AbstractSWTUIEventAdapter {

	@Override
	protected int getSWTEvent(Event event) {
		return RapEventUtils.getSWTEvent(event);
	}

}
