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


public interface SliderView extends AbstractComponentView {

	/**
	 * Sets the value of the slider and tags as invalid the widget if
	 * necessary.
	 * 
	 * @param value
	 */
	public void setValue(int value);

	/**
	 * Returns the value of the slider
	 * 
	 * @return
	 */
	public int getValue();

}
