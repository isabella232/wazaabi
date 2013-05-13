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

package org.eclipse.wazaabi.engine.swt.tests;

import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.CoolItem;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.wazaabi.engine.core.editparts.AbstractWidgetEditPart;
import org.eclipse.wazaabi.engine.core.views.WidgetView;
import org.eclipse.wazaabi.engine.swt.commons.viewers.AbstractSWTViewer;
import org.eclipse.wazaabi.engine.swt.commons.views.SWTWidgetView;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.core.widgets.Widget;

public class SWTUtils {

	/**
	 * Returns the SWT widget corresponding to the given model for the given
	 * Viewer.
	 * 
	 * @param viewer
	 * @param modelControl
	 * @return
	 */
	public static org.eclipse.swt.widgets.Widget getWidget(
			AbstractSWTViewer viewer,
			Widget modelControl) {
		AbstractWidgetEditPart controlEditPart = (AbstractWidgetEditPart) viewer
				.getEditPartRegistry().get(modelControl);
		if (controlEditPart != null) {
			return (org.eclipse.swt.widgets.Widget) (((SWTWidgetView) controlEditPart
					.getWidgetView()).getSWTWidget());
		}
		return null;
	}
	
	/**
	 * Returns the WidgetView corresponding to the given model for the given
	 * Viewer.
	 * 
	 * @param viewer
	 * @param modelControl
	 * @return
	 */
	
	public static WidgetView getWidgetView(
			AbstractSWTViewer viewer, 
			Widget modelControl) {
		AbstractWidgetEditPart controlEditPart = (AbstractWidgetEditPart) viewer
				.getEditPartRegistry().get(modelControl);
		if (controlEditPart != null) {
			return (WidgetView) (((SWTWidgetView) controlEditPart
					.getWidgetView()));
		}
		return null;
	}

	/**
	 * Returns whether the model (a Control) has same bounds in the viewer than
	 * the given swt control.
	 * 
	 * @param viewer
	 *            The viewer where the model is displayed
	 * @param wazComponent
	 *            the (model) control
	 * @param swtControl
	 *            the swt control
	 * @return true if the bounds are equal
	 */
	public static boolean haveSameBounds( AbstractSWTViewer viewer, AbstractComponent wazComponent,
			Control swtControl) {

		org.eclipse.swt.widgets.Widget wazWidget = getWidget(viewer, wazComponent);

		if (wazWidget instanceof ToolItem)
			wazWidget = ((ToolItem)wazWidget).getControl();
		if (wazWidget instanceof CoolItem)
			wazWidget = ((CoolItem)wazWidget).getControl();
		
		assert wazWidget instanceof Control;
		
		Rectangle swtBounds = swtControl.getBounds();
		if (swtControl instanceof Composite) {
			swtBounds = ((Composite)swtControl).getClientArea();
		}
		
		Rectangle wazBounds = ((Control) wazWidget).getBounds();
		
		System.out.println("SWT bounds : " + swtBounds);
		System.out.println("Wazaabi bounds : " + wazBounds);

		return swtBounds.equals(wazBounds);
	}
	
	public static boolean haveSameWidth( AbstractSWTViewer viewer, AbstractComponent wazComponent,
			Control swtControl) {

		org.eclipse.swt.widgets.Widget wazWidget = getWidget(viewer, wazComponent);

		if (wazWidget instanceof ToolItem)
			wazWidget = ((ToolItem)wazWidget).getControl();
		if (wazWidget instanceof CoolItem)
			wazWidget = ((CoolItem)wazWidget).getControl();
		
		assert wazWidget instanceof Control;
		
		Rectangle swtBounds = swtControl.getBounds();
		if (swtControl instanceof Composite) {
			swtBounds = ((Composite)swtControl).getClientArea();
		}
		
		Rectangle wazBounds = ((Control) wazWidget).getBounds();
		
		System.out.println("SWT bounds : " + swtBounds);
		System.out.println("Wazaabi bounds : " + wazBounds);

		if (swtBounds.width == wazBounds.width)
			return true;
		else
			return false;
	}

	public static boolean compareHierarchy(Control swtControl1,	Control swtControl2) {

		if (!swtControl1.getClass().equals(swtControl2.getClass()))
			return false;

		if (swtControl1 instanceof org.eclipse.swt.widgets.Composite) {
			for (int i = 0; i < ((org.eclipse.swt.widgets.Composite) swtControl1)
					.getChildren().length; i++) {
				if (!compareHierarchy(
						((org.eclipse.swt.widgets.Composite) swtControl1)
								.getChildren()[i],
						((org.eclipse.swt.widgets.Composite) swtControl2)
								.getChildren()[i]))
					return false;
			}
		}
		return compareControls(swtControl1, swtControl2);
	}

	public static boolean compareControls(
			org.eclipse.swt.widgets.Control swtControl1,
			org.eclipse.swt.widgets.Control swtControl2) {
		if (!swtControl1.getClass().equals(swtControl2.getClass()))
			return false;

		// only shells are allowed to have different location
		if (!(swtControl1 instanceof org.eclipse.swt.widgets.Shell)) {
			if (!swtControl1.getLocation().equals(swtControl2.getLocation())) {
				System.err.println("Different locations: " + swtControl1 + " "
						+ swtControl2);
				return false;
			}
		}
		if (!swtControl1.getSize().equals(swtControl2.getSize())) {
			System.err.println("Different sizes: " + swtControl1 + " "
					+ swtControl2);
			return false;
		}
		if (swtControl1 instanceof org.eclipse.swt.widgets.Button
				&& !compareButtons(
						(org.eclipse.swt.widgets.Button) swtControl1,
						(org.eclipse.swt.widgets.Button) swtControl2))
			return false;
		return true;
	}

	public static boolean compareButtons(
			org.eclipse.swt.widgets.Button swtButton1,
			org.eclipse.swt.widgets.Button swtButton2) {
		if (!swtButton1.getText().equals(swtButton2.getText())) {
			System.err.println("Different texts: " + swtButton1 + " "
					+ swtButton2);
			return false;
		}
		return true;
	}

}