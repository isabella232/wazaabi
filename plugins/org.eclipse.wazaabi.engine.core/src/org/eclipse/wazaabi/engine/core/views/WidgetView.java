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

package org.eclipse.wazaabi.engine.core.views;

import java.util.HashMap;
import java.util.List;

import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.engine.core.editparts.WidgetEditPart;
import org.eclipse.wazaabi.engine.core.editparts.WidgetViewListener;
import org.eclipse.wazaabi.engine.core.editparts.stylerules.StylePropertyDescriptor;

public interface WidgetView {

	public WidgetView getParent();

	public void addWidgetViewListener(WidgetViewListener listener);

	public void removeWidgetViewListener(WidgetViewListener listener);

	/**
	 * Indicates to all WidgetViewListener that this widgets has been repainted.
	 */
	public void fireWidgetViewRepainted();

	/**
	 * Indicates to all WidgetViewListener that this widgets has been validated.
	 */
	public void fireWidgetViewValidated();

	/**
	 * Attaches the given WidgetEditPart to this WidgetView. SetHost(null) MUST
	 * be called when a WidgetView is no longer attached to a WidgetEditPart. It
	 * is important since all resources attached to the underlying platform
	 * widget will be disposed.
	 * 
	 * @param host
	 */
	public void setHost(WidgetEditPart host);

	public void add(WidgetView view, int index);

	public void remove(WidgetView view);

	/**
	 * Invalidates this WidgetView and revalidates() its parent. If a WidgetView
	 * does not have a parent, it will request a validation (layout).
	 */
	public void revalidate();

	/**
	 * Invalidates this WidgetView. If a WidgetView is tagged as invalid, it is
	 * tagged as dirty for the layout of its container.
	 */
	public void invalidate();

	/**
	 * Sets this figure to be valid if <i>value</i> is <code>true</code> and
	 * invalid otherwise.
	 * 
	 * @param value
	 *            The valid value
	 */
	public void setValid(boolean value);

	/**
	 * Causes this WidgetView to layout itself, as well as its children, if it
	 * is a Container.
	 */
	public void validate();

	/**
	 * Returns whether WidgetView needs to be re created because this style rule
	 * has changed.
	 * 
	 * @param styleRule
	 *            the style rule to observe, can be null.
	 * @return true if this widget view needs to be re created because the given
	 *         style rule has changed.
	 */

	public boolean needReCreateWidgetView(StyleRule styleRule);

	/**
	 * Returns whether WidgetView needs to be re created because at least one of
	 * the property taken into account at creation time has changed.
	 * 
	 * @return true if this widget view needs to be recreated.
	 */
	public boolean needReCreateWidgetView(List<StyleRule> styleRules);

	/**
	 * Sets this style property (platform specific of not) using the given
	 * parameters. This method is expected to be called only if the property is
	 * not used at widget's creation/construction phase.
	 * 
	 * @param rule
	 *            TODO
	 * @param newValue
	 *            can be null, the value encoded as a String, the method is
	 *            expected to convert it into if necessary.
	 * 
	 */
	public void updateStyleRule(StyleRule rule);

	public void updateSameStyleRules(List<StyleRule> rules);

	/**
	 * Called after this WidgetView is added to its parent.
	 */
	public void addNotify();

	/**
	 * Called before this WidgetView is removed from its parent.
	 */
	public void removeNotify();

	public HashMap<String, StylePropertyDescriptor> getPlatformSpecificStylePropertyDescriptors();

	public void processPostControlCreation();

}
