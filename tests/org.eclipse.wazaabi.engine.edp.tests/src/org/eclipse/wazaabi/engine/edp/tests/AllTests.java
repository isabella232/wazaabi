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

package org.eclipse.wazaabi.engine.edp.tests;


import org.eclipse.wazaabi.engine.core.tests.nonosgi.TestActionAdapter;
import org.eclipse.wazaabi.engine.core.tests.nonosgi.TestConditionAdapter;
import org.eclipse.wazaabi.engine.core.tests.nonosgi.TestEventHandlerAdapter;
import org.eclipse.wazaabi.engine.core.tests.nonosgi.TestPropertyChanged;
import org.eclipse.wazaabi.engine.core.tests.nonosgi.TestSequenceAdapter;
import org.eclipse.wazaabi.engine.core.tests.nonosgi.TestValidatorAdapter;
import org.eclipse.wazaabi.engine.core.tests.nonosgi.eventdispatchers.UnitTestEventDispatcherAdapter;
import org.eclipse.wazaabi.engine.core.tests.nonosgi.eventdispatchers.UnitTestEventHandlerAdapter;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

@RunWith(Suite.class)
@SuiteClasses(value = {
		TestActionAdapter.class,
		TestConditionAdapter.class,
		TestSequenceAdapter.class,
		TestEventHandlerAdapter.class,
		TestPropertyChanged.class,
		TestValidatorAdapter.class,
		TestPropertyChanged.class,
		UnitTestEventDispatcherAdapter.class,
		UnitTestEventHandlerAdapter.class
})

public class AllTests {

}
