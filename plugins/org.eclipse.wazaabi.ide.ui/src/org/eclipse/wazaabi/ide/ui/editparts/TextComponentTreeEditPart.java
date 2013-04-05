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

package org.eclipse.wazaabi.ide.ui.editparts;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.widgets.Event;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.handlers.Binding;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.wazaabi.mm.edp.handlers.Parameter;
import org.eclipse.wazaabi.mm.edp.handlers.StringParameter;

public class TextComponentTreeEditPart extends AbstractComponentTreeEditPart {

	protected String getExtendedInfo() {
		String model2UIBindingSource = getFirstModel2UIBindingSource((EventDispatcher) getModel());
		String UI2ModelBindingTarget = getFirstUI2ModelBindingTarget((EventDispatcher) getModel());
		String inputVariableName = getInputVariableName();
		if (inputVariableName != null && !inputVariableName.isEmpty()) {
			if (model2UIBindingSource != null
					&& model2UIBindingSource.equals(UI2ModelBindingTarget))
				return model2UIBindingSource
						.substring(("$" + inputVariableName + "/").length());
		}
		return super.getExtendedInfo();
	}

	private String getStringParamaterValue(EventHandler eventHandler,
			String paramName) {
		if (paramName == null || "".equals(paramName)) //$NON-NLS-1$
			return null;
		for (Parameter param : eventHandler.getParameters()) {
			if (paramName.equals(param.getName())) //$NON-NLS-1$
				return ((StringParameter) param).getValue();
		}
		return null;
	}

	private String getFirstModel2UIBindingSource(EventDispatcher dispatcher) {
		for (EventHandler eventHandler : dispatcher.getHandlers()) {
			if (eventHandler instanceof Binding) {
				String target = getStringParamaterValue(eventHandler, "target");
				if (target != null && "@text".equals(target))
					return getStringParamaterValue(eventHandler, "source");
			}
		}
		return null;
	}

	private String getFirstUI2ModelBindingTarget(EventDispatcher dispatcher) {
		for (EventHandler eventHandler : dispatcher.getHandlers()) {
			if (eventHandler instanceof Binding) {
				String source = getStringParamaterValue(eventHandler, "source");
				if (source != null && "@text".equals(source))
					return getStringParamaterValue(eventHandler, "target");
			}
		}
		return null;
	}

	public void measureWidget(Event event) {

	}

	public void eraseWidget(Event event) {

	}

	public void paintWidget(Event event) {
		if (event.index == 1) {

			FontData fontData = event.gc.getFont().getFontData()[0];
			fontData.setStyle(SWT.BOLD);
			Font font = new Font(event.display, fontData);
			event.gc.setFont(font);

			/* center column 1 vertically */
			// int yOffset = 0;
			// if (event.index == 1) {
			// Point size = event.gc.textExtent(text);
			// yOffset = Math.max(0, (event.height - size.y) / 2);
			// }
			event.gc.drawText(getExtendedInfo() != null ? getExtendedInfo()
					: "", event.x /* + TEXT_MARGIN */, event.y
			/* + yOffset */, true);
			font.dispose();
		}
	}
}