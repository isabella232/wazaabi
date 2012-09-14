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

public interface MenuComponentView extends WidgetView {
	/**
	 * Sets the text value of the MenuComponent and tags as invalid the widget if
	 * necessary.
	 * 
	 * @param value
	 */
	public void setText(String text);

	/**
	 * Returns the text value of the MenuComponent 
	 * 
	 * @return
	 */
	public String getText();
	
	/**
	 * Sets the enabled value of the MenuComponent and tags as invalid the widget if
	 * necessary.
	 * 
	 * @param value
	 */
	
	public void setEnabled(boolean enabled);

	/**
	 * Returns the enabled value of the MenuComponent and tags as invalid the widget if
	 * necessary.
	 * 
	 * @param value
	 */
	public boolean isEnabled();
	
	
	

}
