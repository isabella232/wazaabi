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

package org.eclipse.wazaabi.engine.edp.coderesolution;

import org.eclipse.emf.common.notify.Adapter;

public interface DeferredAdapter extends Adapter {

	public AbstractCodeDescriptor getCodeDescriptor();

	public void dispose();

	public String getCodeLocatorBaseUri();

	public void setCodeLocatorBaseUri(String newBaseUri);

}