/*******************************************************************************
 * Copyright (c) 2012 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.engine.core.celleditors.factories;

import org.eclipse.wazaabi.mm.core.extras.CellEditor;

public interface CellEditorFactory {

	public Object createCellEditor(CellEditor cellEditor,
			Object creationHint);
	
	public boolean isFactoryFor (CellEditor cellEditor);

}