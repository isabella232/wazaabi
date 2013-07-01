/*******************************************************************************
 * Copyright (c) 2013 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.engine.swt.forms.views;

import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.wazaabi.engine.core.editparts.AbstractComponentEditPart;
import org.eclipse.wazaabi.engine.core.editparts.ContainerEditPart;
import org.eclipse.wazaabi.engine.core.editparts.WidgetEditPart;
import org.eclipse.wazaabi.engine.core.gef.EditPart;
import org.eclipse.wazaabi.engine.swt.commons.views.SWTWidgetView;
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;
import org.eclipse.wazaabi.mm.core.widgets.Widget;

public class SWTFormsUtils {

	/**
	 * Looks up into containment hierarchy of the given EditPart and returns the
	 * SWTContainer which owns (or will own) a {@link Form} widget if exists.
	 * Returns null otherwise.
	 * 
	 * @param host
	 *            A non null <code>EditPart</code>
	 * @return A SWTContainer if found, null otherwise
	 */
	public static SWTContainerView getSWTContainer(WidgetEditPart host) {
		EditPart parent = host;
		while ((parent = parent.getParent()) != null)
			if (parent instanceof ContainerEditPart
					&& ((ContainerEditPart) parent).getWidgetView() instanceof SWTContainerView)
				return (SWTContainerView) ((ContainerEditPart) parent)
						.getWidgetView();
		return null;
	}

	/**
	 * Returns true only if this host is a child of a container whose WidgetView
	 * is a {@link Form}
	 * 
	 * @param host
	 * @return
	 */
	public static boolean isDirectChildOfForm(WidgetEditPart host) {
		if (host != null
				&& host.getParent() instanceof ContainerEditPart
				&& ((SWTWidgetView) ((ContainerEditPart) host.getParent())
						.getWidgetView()).getSWTWidget() instanceof Form)
			return true;
		return false;
	}

	/**
	 * Returns true if this widget is either a Form (a Container whose style
	 * 'look-and-feel'='form') or is contained by a Form.
	 * 
	 * @param widget
	 * @return
	 */
	public static boolean ancestorOrSelfIsAForm(Widget widget) {
		if (widget == null)
			return false;
		if (widget.eClass() == CoreWidgetsPackage.Literals.CONTAINER)
			if (isAForm((Container) widget))
				return true;
		if (widget.eContainer() instanceof Widget)
			return ancestorOrSelfIsAForm((Widget) widget.eContainer());
		return false;
	}

	protected static boolean isAForm(Container container) {
		if (container != null) {
			for (StyleRule rule : container.getStyleRules())
				if (AbstractComponentEditPart.LOOK_AND_FEEL.equals(rule
						.getPropertyName()) && rule instanceof StringRule)
					return SWTContainerView.FORM_STYLE
							.equals(((StringRule) rule).getValue());
		}
		return false;
	}
}
