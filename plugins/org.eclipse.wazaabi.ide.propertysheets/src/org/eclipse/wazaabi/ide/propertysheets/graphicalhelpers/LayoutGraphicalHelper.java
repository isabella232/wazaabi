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

import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.wazaabi.ide.propertysheets.descriptors.StyleRuleDescriptor.PlaceHolderRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;

public class LayoutGraphicalHelper extends AbstractGraphicalHelper {

	@Override
	public void paint(Event event, Object element, int columnIndex) {
		if (element instanceof PlaceHolderRule)
			return;
		Rectangle bounds = ((TableItem) event.item).getBounds(columnIndex);

		String text = ((StyleRule) element).eClass().getName();
		Point point = event.gc.stringExtent(text);

		int x = bounds.x + 3;
		int y = bounds.y + bounds.height / 2 - point.y / 2;
		event.gc.drawText(text, x, y);
	}

}
