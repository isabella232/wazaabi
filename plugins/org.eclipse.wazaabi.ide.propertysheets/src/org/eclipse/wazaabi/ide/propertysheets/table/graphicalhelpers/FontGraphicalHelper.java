/*******************************************************************************
 * Copyright (c) 2014 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.propertysheets.table.graphicalhelpers;

import org.eclipse.swt.widgets.Event;

public class FontGraphicalHelper extends AbstractGraphicalHelper {

//	private static Image CHECKED_IMAGE = null;
//	private static Image UNCHECKED_IMAGE = null;

	public FontGraphicalHelper() {
//		CHECKED_IMAGE = new Image(Display.getCurrent(), getClass()
//				.getClassLoader().getResourceAsStream("checked.gif"));
//		UNCHECKED_IMAGE = new Image(Display.getCurrent(), getClass()
//				.getClassLoader().getResourceAsStream("unchecked.gif"));
	}

	@Override
	public void paint(Event event, Object element, int columnIndex) {
//		Image image = UNCHECKED_IMAGE;
//		if (((BooleanRule) element).isValue())
//			image = CHECKED_IMAGE;
//		Rectangle bounds = ((TableItem) event.item).getBounds(columnIndex);
//		int x = bounds.x + bounds.width / 2 - image.getBounds().width / 2;
//		int y = bounds.y + bounds.height / 2 - image.getBounds().height / 2;
//		event.gc.drawImage(image, x, y);
	}

}
