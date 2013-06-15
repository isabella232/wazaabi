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

package org.eclipse.wazaabi.engine.swt.commons.editparts.stylerules.managers;

import java.io.IOException;
import java.io.InputStream;

import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.wazaabi.engine.core.gef.EditPartViewer;
import org.eclipse.wazaabi.engine.core.stylerules.managers.StringRuleManager;
import org.eclipse.wazaabi.engine.edp.EDPUtils;
import org.eclipse.wazaabi.engine.edp.coderesolution.ICodeLocator;
import org.eclipse.wazaabi.engine.swt.commons.views.SWTWidgetView;
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
			EditPartViewer viewer = ((SWTWidgetView) widgetView).getHost()
					.getViewer();
			String baseURI = viewer.getCodeLocatorBaseUri();
			if (baseURI != null && baseURI.length() != 0)
				imageFile = EDPUtils.normalizeURI(baseURI, imageFile);
			ICodeLocator codeLocator = (ICodeLocator) viewer.getFactoryFor(
					null, imageFile, null, ICodeLocator.class);
			if (codeLocator != null) {
				InputStream in = codeLocator.getResourceInputStream(imageFile);
				if (in != null) {
					ImageData imageData = new ImageData(in);
					in.close();
					return new Image(widget.getDisplay(), imageData);
				}
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
		return null;
	}

}
