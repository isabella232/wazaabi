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

package org.eclipse.wazaabi.engine.core.editparts;

public abstract class AbstractComponentEditPart extends AbstractWidgetEditPart {

	public static final String BACKGROUND_COLOR_PROPERTY_NAME = "background-color"; //$NON-NLS-1$
	public static final String FOREGROUND_COLOR_PROPERTY_NAME = "foreground-color"; //$NON-NLS-1$
	public static final String FONT_PROPERTY_NAME = "font"; //$NON-NLS-1$
	public static final String TOOLTIP_TEXT_PROPERTY_NAME = "tooltip-text"; //$NON-NLS-1$ 
	public static final String ERROR_TEXT_PROPERTY_NAME = "error-text"; //$NON-NLS-1$ 
	public static final String DIRECTION_PROPERTY_NAME = "direction"; //$NON-NLS-1$ 
	// TODO : layout-data is not a platform specific rule ???
	public static final String LAYOUT_DATA_PROPERTY_NAME = "layout-data"; //$NON-NLS-1$

	public static final String ENABLED_PROPERTY_NAME = "enabled"; //$NON-NLS-1$
	public static final String VISIBLE_PROPERTY_NAME = "visible"; //$NON-NLS-1$
	public static final String ORIENTATION_PROPERTY_NAME = "orientation"; //$NON-NLS-1$
	public static final String TITLE_VALUE_PROPERTY_NAME = "title-value"; //$NON-NLS-1$
	public static final String TITLE_BORDER_PROPERTY_NAME = "title-border"; //$NON-NLS-1$
	public static final String BORDER_PROPERTY_NAME = "border"; //$NON-NLS-1$
	public static final String TAB_INDEX_PROPERTY_NAME = "tab-index"; //$NON-NLS-1$
	public static final String LOOK_AND_FEEL = "look-and-feel"; //$NON-NLS-1$

	protected void refreshFeaturesAndStyles() {
		refreshUniqueStyleRule(BACKGROUND_COLOR_PROPERTY_NAME);
		refreshUniqueStyleRule(FOREGROUND_COLOR_PROPERTY_NAME);
		refreshUniqueStyleRule(FONT_PROPERTY_NAME);
		refreshUniqueStyleRule(TOOLTIP_TEXT_PROPERTY_NAME);
		refreshUniqueStyleRule(ERROR_TEXT_PROPERTY_NAME);
		refreshUniqueStyleRule(DIRECTION_PROPERTY_NAME);
		refreshUniqueStyleRule(LAYOUT_DATA_PROPERTY_NAME);
		refreshUniqueStyleRule(ENABLED_PROPERTY_NAME);
		refreshUniqueStyleRule(VISIBLE_PROPERTY_NAME);
		refreshUniqueStyleRule(BORDER_PROPERTY_NAME);
		refreshUniqueStyleRule(TAB_INDEX_PROPERTY_NAME);
	}

}
