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
import org.eclipse.wazaabi.engine.core.editparts.CollectionEditPart;
import org.eclipse.wazaabi.engine.edp.EDPUtils;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.core.widgets.Collection;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.events.impl.EventImpl;

public class CoreUtils {

	public static final String CTRL_KEY = "CTRL"; //$NON-NLS-1$
	public static final String SHIFT_KEY = "SHIFT"; //$NON-NLS-1$
	public static final String ALT_KEY = "ALT"; //$NON-NLS-1$
	public static final String CHARACTER_KEY = "character"; //$NON-NLS-1$
	public static final String CORE_UI_REFRESH_EVENT_ID = "core:ui:refresh"; //$NON-NLS-1$

	public static final Event CORE_UI_REFRESH_EVENT = new EventImpl() {

		@Override
		public String getId() {
			return CORE_UI_REFRESH_EVENT_ID;
		}

	};

	/**
	 * This method throws a given event to the dispatcher.
	 * 
	 * @param dispatcher
	 * @param event
	 * 
	 * @Deprecated use {@link EDPUtils#throwEvent(EventDispatcher, Event)}
	 *             instead
	 */
	public static void throwEvent(EventDispatcher dispatcher, Event event) {
		EDPUtils.throwEvent(dispatcher, event);
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

	public static void refreshContent(Collection collection) {
		if (collection == null)
			return;
		for (Adapter adapter : collection.eAdapters())
			if (adapter instanceof CollectionEditPart)
				((CollectionEditPart) adapter).refresh();
	}

	public static void refreshContent(Collection collection, Object element) {
		// TODO : not implemented yet
		refreshContent(collection);
	}

	public static boolean isAltPressed(Event event) {
		if (event != null && event.get(ALT_KEY) == Boolean.TRUE)
			return true;
		return false;
	}

	public static boolean isCtrlPressed(Event event) {
		if (event != null && event.get(CTRL_KEY) == Boolean.TRUE)
			return true;
		return false;
	}

	public static boolean isShiftPressed(Event event) {
		if (event != null && event.get(SHIFT_KEY) == Boolean.TRUE)
			return true;
		return false;
	}

	public static char getCharacter(Event event) {
		if (event != null)
			return ((Character) event.get(CHARACTER_KEY));
		return '\u0000';
	}
}
