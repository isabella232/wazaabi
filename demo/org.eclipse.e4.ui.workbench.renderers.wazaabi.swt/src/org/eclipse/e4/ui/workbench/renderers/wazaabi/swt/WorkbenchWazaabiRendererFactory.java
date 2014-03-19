/*******************************************************************************
 * Copyright (c) 2013 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.eclipse.e4.ui.workbench.renderers.wazaabi.swt;

import org.eclipse.e4.ui.internal.workbench.swt.AbstractPartRenderer;
import org.eclipse.e4.ui.model.application.ui.MUIElement;
import org.eclipse.e4.ui.workbench.renderers.wazaabi.model.wazaabiE4.WazaabiPart;

@SuppressWarnings("restriction")
public class WorkbenchWazaabiRendererFactory extends
		org.eclipse.e4.ui.workbench.renderers.swt.WorkbenchRendererFactory {

	private WazaabiPartRenderer wazaabiPartRenderer = null;

	@Override
	public AbstractPartRenderer getRenderer(MUIElement uiElement, Object parent) {
		if (uiElement instanceof WazaabiPart) {
			if (wazaabiPartRenderer == null) {
				wazaabiPartRenderer = new WazaabiPartRenderer();
				initRenderer(wazaabiPartRenderer);
			}
			return wazaabiPartRenderer;
		}
		return super.getRenderer(uiElement, parent);
	}

}
