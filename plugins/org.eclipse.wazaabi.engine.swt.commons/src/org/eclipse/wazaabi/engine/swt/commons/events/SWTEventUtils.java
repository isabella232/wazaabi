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

package org.eclipse.wazaabi.engine.swt.commons.events;

import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.swt.SWT;

public class SWTEventUtils {

	/**
	 * Given an Event, returns the corresponding int as defined in SWT.java.
	 * 
	 * @see org.eclipse.swt.SWT
	 * 
	 * @param id
	 *            An non null Event
	 * @return
	 */

	// TODO : temporary, we need a central location for event declaration and
	// event context's content definition
	public static int getSWTEvent(Event event) {
		if (event == null)
			return SWT.NONE;
		final String id = event.getId();
		if (id == null || "".equals(id)) //$NON-NLS-1$
			return SWT.None;
		if (!id.startsWith("core:ui:")) //$NON-NLS-1$
			return SWT.NONE;
		final String id2 = id.substring(8);

		if ("focus:in".equals(id2))
			return SWT.FocusIn;
		if ("focus:out".equals(id2))
			return SWT.FocusOut;

		if ("mouse:down".equals(id2))
			return SWT.MouseDown;
		if ("mouse:up".equals(id2))
			return SWT.MouseUp;
		if ("mouse:double:click".equals(id2))
			return SWT.MouseDoubleClick;
		if ("mouse:enter".equals(id2))
			return SWT.MouseEnter;
		if ("mouse:exit".equals(id2))
			return SWT.MouseExit;
		if ("mouse:hover".equals(id2))
			return SWT.MouseHover;
		if ("mouse:wheel".equals(id2))
			return SWT.MouseWheel;
		if ("mouse:move".equals(id2))
			return SWT.MouseMove;
		if ("selection".equals(id2))
			return SWT.Selection;
		if ("default:selection".equals(id2))
			return SWT.DefaultSelection;
		if ("text:modify".equals(id2))
			return SWT.Modify;
		if ("key:down".equals(id2))
			return SWT.KeyDown;
		if ("key:up".equals(id2))
			return SWT.KeyUp;
		return SWT.None;
	}

}
