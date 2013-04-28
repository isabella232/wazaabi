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

package org.eclipse.wazaabi.locationpaths.tests;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.wazaabi.locationpaths.model.Pointer;
import org.eclipse.wazaabi.locationpaths.runtime.Evaluator;
import org.eclipse.wazaabi.locationpaths.runtime.LocationSelector;
import org.junit.Test;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

public class TestOneStepOnListOfObjects extends AbstractTest {

	@Test
	public void testStarOnListOfEObject() {
		String path = "*"; //$NON-NLS-1$
		System.out.println("testing \"" + path + "\""); //$NON-NLS-1$ $NON-NLS-2$
		List<EObject> context = new ArrayList<EObject>();
		context.addAll(getTestEPackage().eContents());

		List<Pointer<?>> result = LocationSelector.select(context, path);

		assertEquals(result.size(), 3);
		List<?> objects = Evaluator.evaluate(result.get(0));
		assertNotNull(objects);
		assertEquals(1, objects.size());
		assertEquals(getTestEPackage().getEClassifiers().get(0), objects.get(0));

		objects = Evaluator.evaluate(result.get(1));
		assertNotNull(objects);
		assertEquals(1, objects.size());
		assertEquals(getTestEPackage().getEClassifiers().get(1), objects.get(0));
	}

	@Test
	public void testStarAndPredicateOnListOfEObject() {
		String path = "*[1]"; //$NON-NLS-1$
		System.out.println("testing \"" + path + "\""); //$NON-NLS-1$ $NON-NLS-2$
		List<EObject> context = new ArrayList<EObject>();
		context.addAll(getTestEPackage().eContents());

		List<Pointer<?>> result = LocationSelector.select(context, path);

		assertEquals(result.size(), 1);
		List<?> objects = Evaluator.evaluate(result.get(0));
		assertNotNull(objects);
		assertEquals(1, objects.size());
		assertEquals(getTestEPackage().getEClassifiers().get(1), objects.get(0));

	}

	@Test
	public void testNameTestOnListOfEObject() {
		String path = "EPackage"; //$NON-NLS-1$
		System.out.println("testing \"" + path + "\""); //$NON-NLS-1$ $NON-NLS-2$
		List<EObject> context = new ArrayList<EObject>();
		context.addAll(getTestEPackage().eContents());

		List<Pointer<?>> result = LocationSelector.select(context, path);

		assertEquals(result.size(), 1);
		List<?> objects = Evaluator.evaluate(result.get(0));
		assertNotNull(objects);
		assertEquals(1, objects.size());
		assertEquals(getTestEPackage().eContents().get(2), objects.get(0));

	}
}
