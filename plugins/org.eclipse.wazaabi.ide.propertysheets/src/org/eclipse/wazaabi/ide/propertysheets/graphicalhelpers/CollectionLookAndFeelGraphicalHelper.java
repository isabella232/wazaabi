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

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeel;
import org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeelRule;

public class CollectionLookAndFeelGraphicalHelper extends AbstractGraphicalHelper {

	@Override
	public void erase(Event event, Object element, int columnIndex) {
		// int itemIndex = table.indexOf((TableItem) event.item);
		// int leftX = (LOWS[itemIndex] - SCALE_MIN) * clientWidth /
		// SCALE_RANGE;
		// int rightX = (HIGHS[itemIndex] - SCALE_MIN) * clientWidth /
		// SCALE_RANGE;
		// GC gc = event.gc;
		// Rectangle clipping = gc.getClipping();
		// clipping.x = leftX;
		// clipping.width = rightX - leftX;
		// gc.setClipping(clipping);
		// Color oldForeground = gc.getForeground();
		// Color oldBackground = gc.getBackground();
		// gc.setForeground(blue);
		// gc.setBackground(white);
		// gc.fillGradientRectangle(event.x, event.y, event.width / 2,
		// event.height, false);
		// gc.setForeground(white);
		// gc.setBackground(red);
		// gc.fillGradientRectangle(event.x + event.width / 2, event.y,
		// event.width / 2, event.height, false);
		// gc.setForeground(oldForeground);
		// gc.setBackground(oldBackground);
		event.detail &= ~SWT.BACKGROUND;
		event.detail &= ~SWT.HOT;
	}

	@Override
	public void paint(Event event, Object element, int columnIndex) {
		Rectangle bounds = ((TableItem) event.item).getBounds(columnIndex);
		LookAndFeel lookAndFeel = ((LookAndFeelRule) element).getValue();
		Point point = event.gc.stringExtent(lookAndFeel.getLiteral());
		int x = bounds.x + X_OFFSET;
		int y = bounds.y + bounds.height / 2 - point.y / 2;
		event.gc.drawText(lookAndFeel.getLiteral(), x, y);
	}

}
