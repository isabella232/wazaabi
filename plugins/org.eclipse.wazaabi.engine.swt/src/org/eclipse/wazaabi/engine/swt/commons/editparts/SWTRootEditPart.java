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

package org.eclipse.wazaabi.engine.swt.editparts;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.wazaabi.engine.core.editparts.WidgetEditPart;
import org.eclipse.wazaabi.engine.core.viewers.AbstractWidgetRootEditPart;
import org.eclipse.wazaabi.engine.core.views.WidgetView;
import org.eclipse.wazaabi.engine.swt.viewers.AbstractSWTViewer;
import org.eclipse.wazaabi.engine.swt.views.SWTControlView;

public class SWTRootEditPart extends AbstractWidgetRootEditPart {

	private SWTControlView viewerParentControlWidgetView = new SWTControlView() {

		protected Widget createSWTWidget(Widget parent, int swtStyle, int index) {
			throw new UnsupportedOperationException();
			// the SWT widget is the viewer parent's control
		}

		public Widget getSWTWidget() {
			if (SWTRootEditPart.this.getViewer() != null)
				return ((AbstractSWTViewer) SWTRootEditPart.this.getViewer())
						.getParent();
			return null;
		}

		@Override
		public EClass getWidgetViewEClass() {
			return null;
		}

		@Override
		protected void initPlatformPropertyDescriptors() {
			// Since we do not create this SWTWidget...
		}

		@Override
		public WidgetEditPart getHost() {
			return SWTRootEditPart.this;
		}

		@Override
		protected boolean isValidationRoot() {
			return true;
		}

	};

	public WidgetView getWidgetView() {
		return viewerParentControlWidgetView;
	}

}
