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

package org.eclipse.wazaabi.engine.edp.adapters;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.wazaabi.engine.edp.locationpaths.IPointersEvaluator;

public interface EventDispatcherAdapter extends Adapter {

	public IPointersEvaluator getPointersEvaluator();

	public void lock(String id);

	public void unlock(String id);

	public boolean isLocked(String id);

	public String getCodeLocatorBaseUri();

}
