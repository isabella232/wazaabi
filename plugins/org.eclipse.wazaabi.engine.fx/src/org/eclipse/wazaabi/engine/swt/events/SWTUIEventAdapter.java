package org.eclipse.wazaabi.engine.swt.events;

import org.eclipse.wazaabi.engine.swt.commons.events.AbstractSWTUIEventAdapter;
import org.eclipse.wazaabi.mm.edp.events.Event;

public class SWTUIEventAdapter extends AbstractSWTUIEventAdapter {

	@Override
	protected int getSWTEvent(Event event) {
		return SWTEventUtils.getSWTEvent(event);
	}

}
