/*******************************************************************************
 * Copyright (c) 2013 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.engine.rap.viewers;

import org.eclipse.wazaabi.engine.swt.commons.editparts.SWTRootEditPart;
import org.eclipse.wazaabi.engine.swt.commons.viewers.AbstractCompatibilityToolkit;
import org.eclipse.wazaabi.engine.swt.commons.viewers.AbstractSWTControlViewer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * 
 * 
 * @author Olivier Moises
 * 
 */
public class RapControlViewer extends AbstractSWTControlViewer {

	final static Logger logger = LoggerFactory
			.getLogger(RapControlViewer.class);

	private final static AbstractCompatibilityToolkit abstractCompatibilityToolkit = new RapCompatibilityToolkit();

	public RapControlViewer(org.eclipse.swt.widgets.Composite parent,
			SWTRootEditPart rootEditPart) {
		super(parent);
		setRootEditPart(rootEditPart);
	}

	public RapControlViewer(org.eclipse.swt.widgets.Composite parent) {
		this(parent, new SWTRootEditPart());
	}

	@Override
	public AbstractCompatibilityToolkit getAbstractCompatibilityToolkit() {
		return abstractCompatibilityToolkit;
	}

}
