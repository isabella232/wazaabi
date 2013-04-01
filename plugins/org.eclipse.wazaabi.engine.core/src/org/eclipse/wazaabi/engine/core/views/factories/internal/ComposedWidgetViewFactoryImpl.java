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

package org.eclipse.wazaabi.engine.core.views.factories.internal;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.wazaabi.engine.core.editparts.WidgetEditPart;
import org.eclipse.wazaabi.engine.core.views.WidgetView;
import org.eclipse.wazaabi.engine.core.views.factories.ComposedWidgetViewFactory;
import org.eclipse.wazaabi.engine.core.views.factories.WidgetViewFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ComposedWidgetViewFactoryImpl implements ComposedWidgetViewFactory {

	List<WidgetViewFactory> widgetViewFactories = new ArrayList<WidgetViewFactory>();
	final static Logger logger = LoggerFactory
			.getLogger(ComposedWidgetViewFactoryImpl.class);

	public WidgetView createWidgetView(WidgetEditPart editPart,
			Object creationHint) {
		if (editPart == null)
			return null;
		List<WidgetViewFactory> filteredWidgetViewFactoris = getWidgetViewFactoriesFor(editPart
				.getViewer());
		for (WidgetViewFactory widgetViewFactory : filteredWidgetViewFactoris) {
			Object widgetView = widgetViewFactory.createWidgetView(editPart,
					creationHint);
			if (widgetView instanceof WidgetView)
				return (WidgetView) widgetView;
		}

		return null;
	}

	public void addWidgetViewFactory(WidgetViewFactory widgetViewFactory) {
		widgetViewFactories.add(widgetViewFactory);
		logger.debug("Added {}", widgetViewFactory);
	}

	public void removeWidgetViewFactory(WidgetViewFactory widgetViewFactory) {
		widgetViewFactories.remove(widgetViewFactory);
		logger.debug("Removed {}", widgetViewFactory);
	}

	public boolean isFactoryFor(Object type) {
		for (WidgetViewFactory widgetViewFactory : widgetViewFactories)
			if (widgetViewFactory.isFactoryFor(type))
				return true;
		return false;
	}

	/**
	 * Returns a list of WidgetViewFactory compatible with the given type (a
	 * platform specific viewer).
	 */
	// TODO : the returned list could be computed once and updated each time an
	// addition or removal is made.
	protected List<WidgetViewFactory> getWidgetViewFactoriesFor(Object type) {
		List<WidgetViewFactory> result = new ArrayList<WidgetViewFactory>();
		for (WidgetViewFactory widgetViewFactory : widgetViewFactories)
			if (widgetViewFactory.isFactoryFor(type))
				result.add(widgetViewFactory);
		return result;
	}
}
