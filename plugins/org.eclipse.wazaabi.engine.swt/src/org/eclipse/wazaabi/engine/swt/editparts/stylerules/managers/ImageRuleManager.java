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

package org.eclipse.wazaabi.engine.swt.editparts.stylerules.managers;

import java.io.IOException;
import java.io.InputStream;

import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.wazaabi.engine.core.stylerules.managers.StringRuleManager;
import org.eclipse.wazaabi.engine.edp.EDPSingletons;
import org.eclipse.wazaabi.engine.swt.views.SWTWidgetView;
import org.eclipse.wazaabi.mm.core.styles.ImageRule;

public class ImageRuleManager extends StringRuleManager {

	public static Image convertToPlatformSpecificObject(Object widgetView,
			ImageRule rule) {
		if (rule == null || rule.getValue() == null
				|| "".equals(rule.getValue())) //$NON-NLS-1$
			return null;
		return convertToPlatformSpecificObject(widgetView, rule.getValue());
	}
	
	public static Image convertToPlatformSpecificObject(Object widgetView,
			String imageFile) {
		if (!(widgetView instanceof SWTWidgetView))
			return null;
	
		final Widget widget = (Widget) ((SWTWidgetView) widgetView)
				.getSWTWidget();
		if (widget == null || widget.isDisposed())
			return null;

		try {
			InputStream in = EDPSingletons.getComposedCodeLocator().getResourceInputStream(imageFile);
			if (in != null) {
				ImageData imageData = new ImageData(in);
				in.close();
				return new Image(widget.getDisplay(), imageData);
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
		return null;
	}

}
