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

package org.eclipse.wazaabi.locationpaths.tests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EcorePackage;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.wazaabi.locationpaths.model.Pointer;
import org.eclipse.wazaabi.locationpaths.runtime.Evaluator;
import org.eclipse.wazaabi.locationpaths.runtime.LocationSelector;
import org.junit.Before;
import org.junit.Test;

public class TestOneStep extends AbstractTest {

	public static class EObjectWithData extends EObjectImpl {

		private HashMap<String, Object> data = new HashMap<String, Object>();

		public Object get(String key) {
			return data.get(key);
		}

		public void set(String key, Object value) {
			data.put(key, value);
		}
	};

	@Before
	public void setUp() throws Exception {
		//LocationPathsHelper.init();
	}

	@Test
	public void testInitialContextWithEClassifier() {
		String path = "eClassifier(\"" + EcorePackage.eINSTANCE.getNsURI() + "\", \"" + EcorePackage.Literals.EPACKAGE.getName() + "\")/@name"; //$NON-NLS-1$
		System.out.println("testing \"" + path + "\""); //$NON-NLS-1$ $NON-NLS-2$

		List<Pointer<?>> result = LocationSelector.select(getTestEPackage(),
				path);
		assertNotNull(result);
		assertEquals(1, result.size());

		List<?> objects = Evaluator.evaluate(result.get(0));
		assertNotNull(objects);
		assertEquals(1, objects.size());
		assertEquals(EcorePackage.Literals.EPACKAGE.getName(), objects.get(0));
	}

	@Test
	public void testAbbreviatedAttributeAxis() {
		String path = "@name"; //$NON-NLS-1$
		System.out.println("testing \"" + path + "\""); //$NON-NLS-1$ $NON-NLS-2$

		List<Pointer<?>> result = LocationSelector.select(getTestEPackage(),
				path);
		assertNotNull(result);
		assertEquals(1, result.size());

		List<?> objects = Evaluator.evaluate(result.get(0));
		assertNotNull(objects);
		assertEquals(1, objects.size());
		assertEquals(ROOT_EPACKAGE_NAME, objects.get(0));
	}

	@Test
	public void testAttributeAxis() {
		String path = "attribute::name"; //$NON-NLS-1$
		System.out.println("testing \"" + path + "\""); //$NON-NLS-1$ $NON-NLS-2$

		List<Pointer<?>> result = LocationSelector.select(getTestEPackage(),
				path);
		assertNotNull(result);
		assertEquals(1, result.size());

		List<?> objects = Evaluator.evaluate(result.get(0));
		assertNotNull(objects);
		assertEquals(1, objects.size());
		assertEquals(ROOT_EPACKAGE_NAME, objects.get(0));
	}

	@Test
	public void testAbbreviatedReferenceAxis() {
		String path = "&eClassifiers"; //$NON-NLS-1$
		System.out.println("testing \"" + path + "\""); //$NON-NLS-1$ $NON-NLS-2$

		List<Pointer<?>> result = LocationSelector.select(getTestEPackage(),
				path);

		assertNotNull(result);

		// we expect on list of EMFPointers
		assertEquals(1, result.size());

		List<?> objects = Evaluator.evaluate(result.get(0));
		assertNotNull(objects);
		assertEquals(getTestEPackage().getEClassifiers().size(), objects.size());

		for (int i = 0; i < getTestEPackage().getEClassifiers().size(); i++)
			assertEquals(getTestEPackage().getEClassifiers().get(i),
					objects.get(i));
	}

	@Test
	public void testReferenceAxis() {
		String path = "reference::eClassifiers"; //$NON-NLS-1$
		System.out.println("testing \"" + path + "\""); //$NON-NLS-1$ $NON-NLS-2$

		List<Pointer<?>> result = LocationSelector.select(getTestEPackage(),
				path);
		assertNotNull(result);

		// we expect on list of EMFPointers
		assertEquals(1, result.size());

		List<?> objects = Evaluator.evaluate(result.get(0));
		assertNotNull(objects);
		assertEquals(getTestEPackage().getEClassifiers().size(), objects.size());

		for (int i = 0; i < getTestEPackage().getEClassifiers().size(); i++)
			assertEquals(getTestEPackage().getEClassifiers().get(i),
					objects.get(i));

	}

	@Test
	public void testAbbreviatedReferencePathWithPredicateEquality() {
		String path = "&eClassifiers[ @name =  \"" + getTestEPackage().getEClassifiers().get(1).getName() + "\"]"; //$NON-NLS-1$
		System.out.println("testing \"" + path + "\""); //$NON-NLS-1$ $NON-NLS-2$

		List<Pointer<?>> result = LocationSelector.select(getTestEPackage(),
				path);
		assertNotNull(result);

		// we expect on list of EMFPointers
		assertEquals(1, result.size());

		List<?> objects = Evaluator.evaluate(result.get(0));
		assertNotNull(objects);
		assertEquals(1, objects.size());

		assertEquals(getTestEPackage().getEClassifiers().get(1), objects.get(0));

	}

	@Test
	public void testReferencePathWithPredicateEquality() {
		String path = "reference::eClassifiers[ @name =  \"" + getTestEPackage().getEClassifiers().get(1).getName() + "\"]"; //$NON-NLS-1$
		System.out.println("testing \"" + path + "\""); //$NON-NLS-1$ $NON-NLS-2$

		List<Pointer<?>> result = LocationSelector.select(getTestEPackage(),
				path);
		assertNotNull(result);

		// we expect on list of EMFPointers
		assertEquals(1, result.size());

		List<?> objects = Evaluator.evaluate(result.get(0));
		assertNotNull(objects);
		assertEquals(1, objects.size());

		assertEquals(getTestEPackage().getEClassifiers().get(1), objects.get(0));

	}

	@Test
	public void testAbbreviatedChildAxis() {
		String path = "EPackage"; //$NON-NLS-1$
		System.out.println("testing \"" + path + "\""); //$NON-NLS-1$ $NON-NLS-2$

		List<Pointer<?>> result = LocationSelector.select(getTestEPackage(),
				path);
		assertNotNull(result);

		// we expect on list of EMFPointers
		assertEquals(1, result.size());

		List<?> objects = Evaluator.evaluate(result.get(0));
		assertNotNull(objects);
		assertEquals(getTestEPackage().getESubpackages().size(), objects.size());

		for (int i = 0; i < getTestEPackage().getESubpackages().size(); i++)
			assertEquals(getTestEPackage().getESubpackages().get(i),
					objects.get(i));
	}

	@Test
	public void testChildAxis() {
		String path = "child::EPackage"; //$NON-NLS-1$
		System.out.println("testing \"" + path + "\""); //$NON-NLS-1$ $NON-NLS-2$

		List<Pointer<?>> result = LocationSelector.select(getTestEPackage(),
				path);
		assertNotNull(result);

		// we expect on list of EMFPointers
		assertEquals(1, result.size());

		List<?> objects = Evaluator.evaluate(result.get(0));
		assertNotNull(objects);
		assertEquals(getTestEPackage().getESubpackages().size(), objects.size());

		for (int i = 0; i < getTestEPackage().getESubpackages().size(); i++)
			assertEquals(getTestEPackage().getESubpackages().get(i),
					objects.get(i));
	}

	@Test
	public void testAbreviatedParentAxis() {
		String path = ".."; //$NON-NLS-1$
		System.out.println("testing \"" + path + "\""); //$NON-NLS-1$ $NON-NLS-2$

		List<Pointer<?>> result = LocationSelector.select(getTestEPackage()
				.getEClassifiers().get(0), path);
		assertNotNull(result);

		// we expect on list of EMFPointers
		assertEquals(1, result.size());

		List<?> objects = Evaluator.evaluate(result.get(0));
		assertNotNull(objects);
		assertEquals(1, objects.size());

		assertEquals(getTestEPackage(), objects.get(0));
	}

	@Test
	public void testParentAxis() {
		String path = "parent::node()"; //$NON-NLS-1$
		System.out.println("testing \"" + path + "\""); //$NON-NLS-1$ $NON-NLS-2$

		List<Pointer<?>> result = LocationSelector.select(getTestEPackage()
				.getEClassifiers().get(0), path);
		assertNotNull(result);

		// we expect on list of EMFPointers
		assertEquals(1, result.size());

		List<?> objects = Evaluator.evaluate(result.get(0));
		assertNotNull(objects);
		assertEquals(1, objects.size());

		assertEquals(getTestEPackage(), objects.get(0));
	}

	@Test
	public void testAbreviatedSelfAxis() {
		String path = "."; //$NON-NLS-1$
		System.out.println("testing \"" + path + "\""); //$NON-NLS-1$ $NON-NLS-2$

		List<Pointer<?>> result = LocationSelector.select(getTestEPackage(),
				path);
		assertNotNull(result);

		// we expect on list of EMFPointers
		assertEquals(1, result.size());

		List<?> objects = Evaluator.evaluate(result.get(0));
		assertNotNull(objects);
		assertEquals(1, objects.size());

		assertEquals(getTestEPackage(), objects.get(0));
	}

	@Test
	public void testSelfAxis() {
		String path = "self::node()"; //$NON-NLS-1$
		System.out.println("testing \"" + path + "\""); //$NON-NLS-1$ $NON-NLS-2$

		List<Pointer<?>> result = LocationSelector.select(getTestEPackage(),
				path);
		assertNotNull(result);

		// we expect on list of EMFPointers
		assertEquals(1, result.size());

		List<?> objects = Evaluator.evaluate(result.get(0));
		assertNotNull(objects);
		assertEquals(1, objects.size());

		assertEquals(getTestEPackage(), objects.get(0));
	}

	@Test
	public void testAbreviatedVariableAxis() {

		EObjectWithData eObjectWithData = new EObjectWithData();

		eObjectWithData.set("data", getTestEPackage()); //$NON-NLS-1$

		String path = "$data"; //$NON-NLS-1$
		System.out.println("testing \"" + path + "\""); //$NON-NLS-1$ $NON-NLS-2$

		List<Pointer<?>> result = LocationSelector
				.select(eObjectWithData, path);
		assertNotNull(result);

		// we expect on list of EMFPointers
		assertEquals(1, result.size());

		List<?> objects = Evaluator.evaluate(result.get(0));
		assertNotNull(objects);
		assertEquals(1, objects.size());

		assertEquals(getTestEPackage(), objects.get(0));
	}

	@Test
	public void testVariableAxis() {

		EObjectWithData eObjectWithData = new EObjectWithData();

		eObjectWithData.set("data", getTestEPackage()); //$NON-NLS-1$

		String path = "variable::data"; //$NON-NLS-1$
		System.out.println("testing \"" + path + "\""); //$NON-NLS-1$ $NON-NLS-2$

		List<Pointer<?>> result = LocationSelector
				.select(eObjectWithData, path);
		assertNotNull(result);

		// we expect on list of EMFPointers
		assertEquals(1, result.size());

		List<?> objects = Evaluator.evaluate(result.get(0));
		assertNotNull(objects);
		assertEquals(1, objects.size());

		assertEquals(getTestEPackage(), objects.get(0));
	}

	@Test
	public void testClassAxis() {

		String path = "class::node()"; //$NON-NLS-1$
		System.out.println("testing \"" + path + "\""); //$NON-NLS-1$ $NON-NLS-2$

		List<Pointer<?>> result = LocationSelector.select(getTestEPackage(),
				path);
		assertNotNull(result);

		// we expect on list of EMFPointers
		assertEquals(1, result.size());

		List<?> objects = Evaluator.evaluate(result.get(0));
		assertNotNull(objects);
		assertEquals(1, objects.size());

		assertEquals(EcorePackage.Literals.EPACKAGE, objects.get(0));
	}

	@Test
	public void testPackageAxis1() {

		String path = "package::node()"; //$NON-NLS-1$
		System.out.println("testing \"" + path + "\""); //$NON-NLS-1$ $NON-NLS-2$

		List<Pointer<?>> result = LocationSelector.select(getTestEPackage()
				.getEClassifiers().get(0), path);
		assertNotNull(result);

		// we expect on list of EMFPointers
		assertEquals(1, result.size());

		List<?> objects = Evaluator.evaluate(result.get(0));
		assertNotNull(objects);
		assertEquals(1, objects.size());

		assertEquals(getTestEPackage(), objects.get(0));
	}

	@Test
	public void testPackageAxis2() {

		String path = "package::node()"; //$NON-NLS-1$
		System.out.println("testing \"" + path + "\""); //$NON-NLS-1$ $NON-NLS-2$

		List<Pointer<?>> result = LocationSelector.select(getTestEPackage(),
				path);
		assertNotNull(result);

		// we expect on list of EMFPointers
		assertEquals(1, result.size());

		List<?> objects = Evaluator.evaluate(result.get(0));
		assertNotNull(objects);
		assertEquals(1, objects.size());

		assertEquals(EcorePackage.eINSTANCE, objects.get(0));
	}

	// DESCENDANT-OR-SELF : not implemented

	@Test
	public void testAbbreviatedReferencePathWithIndexAsPredicate() {
		String path = "&eClassifiers[1]"; //$NON-NLS-1$
		System.out.println("testing \"" + path + "\""); //$NON-NLS-1$ $NON-NLS-2$

		List<Pointer<?>> result = LocationSelector.select(getTestEPackage(),
				path);
		assertNotNull(result);

		// we expect on list of EMFPointers
		assertEquals(1, result.size());

		List<?> objects = Evaluator.evaluate(result.get(0));
		assertNotNull(objects);
		assertEquals(1, objects.size());

		assertEquals(getTestEPackage().getEClassifiers().get(1), objects.get(0));
	}

	@Test
	public void testReferencePathWithIndexAsPredicate() {
		String path = "reference::eClassifiers[1]"; //$NON-NLS-1$
		System.out.println("testing \"" + path + "\""); //$NON-NLS-1$ $NON-NLS-2$

		List<Pointer<?>> result = LocationSelector.select(getTestEPackage(),
				path);
		assertNotNull(result);

		// we expect on list of EMFPointers
		assertEquals(1, result.size());

		List<?> objects = Evaluator.evaluate(result.get(0));
		assertNotNull(objects);
		assertEquals(1, objects.size());

		assertEquals(getTestEPackage().getEClassifiers().get(1), objects.get(0));
	}

	@Test
	public void testSelfOnListOfEObject() {
		//		String path = "*"; //$NON-NLS-1$
		String path = "reference::eClassifiers[1]"; //$NON-NLS-1$
		System.out.println("testing \"" + path + "\""); //$NON-NLS-1$ $NON-NLS-2$
		List<EObject> context = new ArrayList<EObject>();
		context.addAll(getTestEPackage().getEClassifiers());

		// List<Pointer<?>> result = LocationSelector.select(context, path);

		if (true)
			return;

		// try {
		// Method privateStringMethod = Evaluator.class.getDeclaredMethod(
		// "evaluate", new Class[] { Object.class, int.class,
		// String.class });
		// privateStringMethod.setAccessible(true);
		// List<?> result = (List<?>) privateStringMethod.invoke(null,
		// new Object[] { getTestEPackage(), Axis.REFERENCE,
		// "eClassifiers" });
		// System.err.println("--------------> " + result);
		// } catch (NoSuchMethodException e) {
		// e.printStackTrace();
		// } catch (SecurityException e) {
		// e.printStackTrace();
		// } catch (IllegalAccessException e) {
		// e.printStackTrace();
		// } catch (IllegalArgumentException e) {
		// e.printStackTrace();
		// } catch (InvocationTargetException e) {
		// e.printStackTrace();
		// }

		// Evaluator.evaluate(context, Axis.SELF, "[0]");

		//
		// List<Pointer<?>> result = LocationSelector.select(context, path);
		// assertNotNull(result);
		//
		// // we expect on list of EMFPointers
		// assertEquals(1, result.size());
		//
		// List<?> objects = Evaluator.evaluate(result.get(0));
		// assertNotNull(objects);
		// assertEquals(1, objects.size());
		//
		// assertEquals(getTestEPackage().getEClassifiers().get(1),
		// objects.get(0));
	}

}
