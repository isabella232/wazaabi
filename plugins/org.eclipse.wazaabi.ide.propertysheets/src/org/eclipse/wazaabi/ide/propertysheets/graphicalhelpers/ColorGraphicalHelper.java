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
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.wazaabi.mm.core.styles.ColorRule;

public class ColorGraphicalHelper extends AbstractGraphicalHelper {

	@Override
	public void erase(Event event, Object element, int columnIndex) {
//		int itemIndex = table.indexOf((TableItem) event.item);
//		int leftX = (LOWS[itemIndex] - SCALE_MIN) * clientWidth / SCALE_RANGE;
//		int rightX = (HIGHS[itemIndex] - SCALE_MIN) * clientWidth / SCALE_RANGE;
//		GC gc = event.gc;
//		Rectangle clipping = gc.getClipping();
//		clipping.x = leftX;
//		clipping.width = rightX - leftX;
//		gc.setClipping(clipping);
//		Color oldForeground = gc.getForeground();
//		Color oldBackground = gc.getBackground();
//		gc.setForeground(blue);
//		gc.setBackground(white);
//		gc.fillGradientRectangle(event.x, event.y, event.width / 2,
//				event.height, false);
//		gc.setForeground(white);
//		gc.setBackground(red);
//		gc.fillGradientRectangle(event.x + event.width / 2, event.y,
//				event.width / 2, event.height, false);
//		gc.setForeground(oldForeground);
//		gc.setBackground(oldBackground);
		event.detail &= ~SWT.BACKGROUND;
		event.detail &= ~SWT.HOT;
	}
	
	@Override
	public void paint(Event event, Object element, int columnIndex) {
		Rectangle bounds = ((TableItem) event.item).getBounds(columnIndex);

		String text = ((ColorRule) element).getRed() + ","
				+ ((ColorRule) element).getGreen() + ","
				+ ((ColorRule) element).getBlue();
		Image image = new Image(event.display, bounds.height, bounds.height);
		Color color = new Color(event.display, new RGB(
				((ColorRule) element).getRed(),
				((ColorRule) element).getGreen(),
				((ColorRule) element).getBlue()));

		GC gc = new GC(image);
		gc.setBackground(color);
		gc.fillRectangle(image.getBounds());
		color.dispose();
		gc.dispose();
		Point point = event.gc.stringExtent(text);
		int x = bounds.x + 3;
		int y = bounds.y + bounds.height / 2 - point.y / 2;
		event.gc.drawText(text, x, y);
		event.gc.drawImage(image, x + point.x + 5, image.getBounds().height);
		image.dispose();
	}
}
