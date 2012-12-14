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

package org.eclipse.wazaabi.engine.core.tests.nonosgi;

import org.eclipse.wazaabi.engine.edp.adapters.ActionAdapterImpl;
import org.eclipse.wazaabi.engine.edp.adapters.SequenceAdapterImpl;
import org.eclipse.wazaabi.mm.edp.handlers.Action;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersFactory;
import org.eclipse.wazaabi.mm.edp.handlers.Sequence;
import org.junit.Assert;
import org.junit.Test;

public class TestSequenceAdapter extends AbstractTestExecutableAdapter {
	
	private Sequence sequence;
	
	@Override
	public void before() {
		sequence = EDPHandlersFactory.eINSTANCE.createSequence();
		SequenceAdapterImpl sequenceAdapter = new SequenceAdapterImpl();
		sequence.eAdapters().add(sequenceAdapter);
		super.before();
	}
	
	@Test
	public void testAddActionAdapterToModel() {
		

		
		Action action = EDPHandlersFactory.eINSTANCE.createAction();
		action.setUri(BASIC_ACTION_HANDLER);
		sequence.getExecutables().add(action);
		
		Assert.assertTrue(action.eAdapters().get(0) instanceof ActionAdapterImpl);

	}

}
