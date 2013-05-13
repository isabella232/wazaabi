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

import org.eclipse.wazaabi.engine.edp.nonosgi.EDPHelper;
import org.eclipse.wazaabi.locationpaths.nonosgi.LocationPathsHelper;
import org.eclipse.wazaabi.locator.urn.java.nonosgi.URNJavaLocatorHelper;
import org.junit.After;
import org.junit.Before;

public class AbstractTestExecutableAdapter {
	
	//public static final String JAVA_URN_TEST_URI = "urn:java:org.eclipse.wazaabi.test.testbundle.handlers.SecondTestHandler"; //$NON-NLS-1$ 
	//public static final String PLUGIN_PLATFORM_TEST_URI = "platform:/plugin/org.eclipse.wazaabi.test.testbundle/org.eclipse.wazaabi.test.testbundle.handlers.FirstTestHandler"; //$NON-NLS-1$
	
	
	public static final String BASIC_ACTION_HANDLER = "urn:java:org.eclipse.wazaabi.engine.edp.tests.handlers.BasicActionHandler"; //$NON-NLS-1$ 
	public static final String BAD_ACTION_HANDLER = "urn:java:org.eclipse.wazaabi.engine.edp.tests.handlers.BadActionHandler"; //$NON-NLS-1$ 

	
	@After
	public void tearDown() throws Exception {
	}
	
	@Before
	public void before() {
		EDPHelper.init();
		URNJavaLocatorHelper.init();
		LocationPathsHelper.init();
	}

}
