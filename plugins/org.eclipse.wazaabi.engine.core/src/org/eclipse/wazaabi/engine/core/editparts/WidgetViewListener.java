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

package org.eclipse.wazaabi.engine.core.editparts;

import org.eclipse.wazaabi.engine.core.views.WidgetView;

public interface WidgetViewListener {

	public static final int VIEW_REPAINTED = 1;
	public static final int VIEW_VALIDATED = 2;
	public static final int VIEW_REVALIDATED = 4;
	public static final int VIEW_DISPOSED = 8;

	public void viewChanged(WidgetView widgetView, int changeType);
}
