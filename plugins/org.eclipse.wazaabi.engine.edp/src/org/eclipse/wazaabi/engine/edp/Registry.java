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

package org.eclipse.wazaabi.engine.edp;

import org.eclipse.wazaabi.engine.edp.coderesolution.ICodeLocator;
import org.eclipse.wazaabi.engine.edp.locationpaths.IPointersEvaluator;

public interface Registry {

	public void addPointersEvaluator(IPointersEvaluator pointersEvaluator);

	public void removePointersEvaluator(IPointersEvaluator pointersEvaluator);

	public IPointersEvaluator getPointersEvaluator(String id);

	public IPointersEvaluator getDefaultPointersEvaluator();

//	public void addCodeLocator(ICodeLocator codeLocator);
//
//	public void removeCodeLocator(ICodeLocator codeLocator);
}
