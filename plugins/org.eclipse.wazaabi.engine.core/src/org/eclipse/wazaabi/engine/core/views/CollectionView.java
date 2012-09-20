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

import java.util.List;

public interface CollectionView extends AbstractComponentView {

	public void setInput(Object input);

	public void refresh();

	public void setSelection(List<Object> newSelection);

	public void setHeaderVisible(boolean show);

	public void setShowHorizontalLines(boolean show);

}
