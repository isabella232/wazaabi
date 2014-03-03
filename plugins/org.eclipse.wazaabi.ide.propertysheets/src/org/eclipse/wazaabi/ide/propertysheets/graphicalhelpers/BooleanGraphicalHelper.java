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

package org.eclipse.wazaabi.ide.propertysheets.graphicalhelpers;

import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.wazaabi.ide.propertysheets.ImageUtils;
import org.eclipse.wazaabi.mm.core.styles.BooleanRule;

public class BooleanGraphicalHelper extends AbstractGraphicalHelper {

	private static Image CHECKED_IMAGE = null;
	private static Image UNCHECKED_IMAGE = null;

	public BooleanGraphicalHelper() {
		CHECKED_IMAGE = new Image(Display.getCurrent(),
				ImageUtils.getImageData("icons/checked.gif",
						BooleanGraphicalHelper.class));
		UNCHECKED_IMAGE = new Image(Display.getCurrent(),
				ImageUtils.getImageData("icons/unchecked.gif",
						BooleanGraphicalHelper.class));
	}

	@Override
	public void paint(Event event, Object element, int columnIndex) {
		Image image = UNCHECKED_IMAGE;
		if (((BooleanRule) element).isValue())
			image = CHECKED_IMAGE;
		Rectangle bounds = ((TableItem) event.item).getBounds(columnIndex);
		int x = bounds.x + bounds.width / 2 - image.getBounds().width / 2;
		int y = bounds.y + bounds.height / 2 - image.getBounds().height / 2;
		event.gc.drawImage(image, x, y);
	}

}
