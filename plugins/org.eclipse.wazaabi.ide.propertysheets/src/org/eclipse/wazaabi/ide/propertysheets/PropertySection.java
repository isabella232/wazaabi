/*******************************************************************************
 * Copyright (c) 2014 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.propertysheets;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

public interface PropertySection extends TargetChangeService {

	void refresh();

	void setInput(Object input);

	public void createControls(Composite parent);

	public void dispose();

	public String getLabel();

	public Control getControl();

}
