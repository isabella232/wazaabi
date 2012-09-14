/*******************************************************************************
 * Copyright (c) 2008 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.engine.core;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.wazaabi.engine.core.editparts.AbstractWidgetEditPart;
import org.eclipse.wazaabi.engine.edp.adapters.EventHandlerAdapter;
import org.eclipse.wazaabi.engine.edp.exceptions.OperationAborted;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.events.impl.EventImpl;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class CoreUtils {

	public static final String CORE_UI_REFRESH_EVENT_ID = "core:ui:refresh"; //$NON-NLS-1$

	public static final Event CORE_UI_REFRESH_EVENT = new EventImpl() {

		@Override
		public String getId() {
			return CORE_UI_REFRESH_EVENT_ID;
		}

	};

	public static void throwEvent(EventDispatcher dispatcher, Event event) {
		if (dispatcher == null || event == null || "".equals(event.getId())) //$NON-NLS-1$
			return;
		for (EventHandler eventHandler : dispatcher.getHandlers())
			for (Event listenedEvent : eventHandler.getEvents())
				if (event.getId().equals(listenedEvent.getId()))
					for (Adapter adapter : eventHandler.eAdapters())
						if (adapter instanceof EventHandlerAdapter)
							try {
								((EventHandlerAdapter) adapter).trigger(event);
							} catch (OperationAborted e) {
								// TODO Auto-generated catch block
								e.printStackTrace();
								System.err.println(e.getErrorMessage());
							}

	}

	/**
	 * Sends a refresh UI event to the given widget
	 * 
	 * @param widget
	 */
	public static void refresh(org.eclipse.wazaabi.mm.core.widgets.Widget widget) {
		for (Adapter adapter : widget.eAdapters())
			if (adapter instanceof AbstractWidgetEditPart)
				((AbstractWidgetEditPart) adapter).forceRefreshEvent();
	}

	/**
	 * Send a refresh event to this component and all its descendants.
	 * 
	 * @param widget
	 */
	public static void deepRefresh(
			org.eclipse.wazaabi.mm.core.widgets.Widget widget) {
		for (Adapter adapter : widget.eAdapters())
			if (adapter instanceof AbstractWidgetEditPart)
				((AbstractWidgetEditPart) adapter).forceRefreshEvent();
		if (widget instanceof Container)
			for (AbstractComponent component : ((Container) widget)
					.getChildren())
				deepRefresh(component);
	}

}
