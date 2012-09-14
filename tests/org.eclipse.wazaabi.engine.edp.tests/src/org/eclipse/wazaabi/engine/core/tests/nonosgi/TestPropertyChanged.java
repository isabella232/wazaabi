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

import org.eclipse.wazaabi.engine.core.tests.nonosgi.eventdispatchers.mockobjects.MockEventDispatcher;
import org.eclipse.wazaabi.engine.core.tests.nonosgi.eventdispatchers.mockobjects.MockEventDispatcherAdapter;
import org.eclipse.wazaabi.engine.edp.tests.model.company.CompanyFactory;
import org.eclipse.wazaabi.engine.edp.tests.model.company.TestClass;
import org.eclipse.wazaabi.mm.edp.events.EDPEventsFactory;
import org.eclipse.wazaabi.mm.edp.events.PropertyChangedEvent;
import org.eclipse.wazaabi.mm.edp.handlers.Action;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersFactory;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.junit.Test;

public class TestPropertyChanged extends AbstractTestExecutableAdapter{
	


	@Test
	public void test1() {

		TestClass testClass = CompanyFactory.eINSTANCE.createTestClass();
		testClass.setIntAttribute1(1);
		testClass.setIntAttribute2(2);
		testClass.setStringAttribute1("attribute1");
		testClass.setStringAttribute2("attribute2");

		MockEventDispatcher eventDispatcher = new MockEventDispatcher();

		// we add an entry with '$target' as key and testClass as value
		eventDispatcher.set("target", testClass);

		Action actionHandler = EDPHandlersFactory.eINSTANCE.createAction();
		actionHandler.setUri(BASIC_ACTION_HANDLER);
		
		PropertyChangedEvent propertyChanged = EDPEventsFactory.eINSTANCE.createPropertyChangedEvent();
		propertyChanged.setPath("$target/@intAttribute1");
		
		EventHandler eventHandler = EDPHandlersFactory.eINSTANCE.createEventHandler();
		eventHandler.getExecutables().add(actionHandler);

		eventDispatcher.getHandlers().add(eventHandler);
		eventHandler.getEvents().add(propertyChanged);

		MockEventDispatcherAdapter eventDispatcherAdapter = new MockEventDispatcherAdapter();
		eventDispatcher.eAdapters().add(eventDispatcherAdapter);
		testClass.setIntAttribute1(15);
		eventDispatcher.eAdapters().remove(eventDispatcherAdapter);
		eventDispatcher.eAdapters().add(eventDispatcherAdapter);
		testClass.setIntAttribute1(15);
	}

	// @SuppressWarnings("unchecked")
	// @Test
	// public void testIntToStringConverter() {
	//
	// TestClass testClass = CompanyFactory.eINSTANCE.createTestClass();
	// testClass.setIntAttribute1(1);
	// testClass.setIntAttribute2(2);
	// testClass.setStringAttribute1("4");
	// testClass.setStringAttribute2("attribute2");
	//
	// // BindingContext is abstract (at the moment)
	// BindingContext bindingContext = new BindingContextImpl() {
	// };
	// // for the tests, we add '$target' as an entry with testClass as value
	// bindingContext.set("target", testClass);
	//
	// Binding binding = BindingFactory.eINSTANCE.createBinding();
	// bindingContext.getBindings().add(binding);
	// binding.setSource("$target/@stringAttribute1");
	// binding.setTarget("$target/@intAttribute2");
	//
	// PropertyChanged propertyChanged = EdpFactory.eINSTANCE
	// .createPropertyChanged();
	// propertyChanged.setPath("$target/@intAttribute1");
	// binding.getTriggers().add(propertyChanged);
	//
	// DeferredConverter converter = BindingFactory.eINSTANCE
	// .createDeferredConverter();
	// converter.setURI(CONVERTER_URI);
	// binding.setConverter(converter);
	//
	// BindingContextAdapter bindingContextAdapter = new
	// BindingContextAdapterStub();
	// // setting the target means starting binding
	// bindingContextAdapter.setTarget(bindingContext);
	//
	// System.out.println(testClass.getStringAttribute2());
	// testClass.setIntAttribute1(10);
	// System.out.println(testClass.getIntAttribute2());
	// bindingContextAdapter.setTarget(null);
	//
	// }

	// @SuppressWarnings("unchecked")
	// @Test
	// public void test2() {
	//
	// TestClass testClass = CompanyFactory.eINSTANCE.createTestClass();
	// testClass.setIntAttribute1(1);
	// testClass.setIntAttribute2(2);
	// testClass.setStringAttribute1("4");
	// testClass.setStringAttribute2("attribute2");
	//
	// // BindingContext is abstract (at the moment)
	// BindingContext bindingContext = new BindingContextImpl() {
	// };
	// // for the tests, we add '$target' as an entry with testClass as value
	// bindingContext.set("target", testClass);
	//
	// Binding binding = BindingFactory.eINSTANCE.createBinding();
	// bindingContext.getBindings().add(binding);
	// binding.setSource("$target/@stringAttribute1");
	// binding.setTarget("$target/@intAttribute2");
	//
	// PropertyChanged propertyChanged = BindingFactory.eINSTANCE
	// .createPropertyChanged();
	// propertyChanged.setProperty("$target/@intAttribute1");
	// binding.getTriggers().add(propertyChanged);
	//
	// DeferredConverter converter = BindingFactory.eINSTANCE
	// .createDeferredConverter();
	// converter.setURI(CONVERTER_URI);
	// binding.setConverter(converter);
	//
	// BindingContextAdapter bindingContextAdapter = new
	// BindingContextAdapterStub();
	// // setting the target means starting binding
	// bindingContextAdapter.setTarget(bindingContext);
	//
	// System.out.println(testClass.getStringAttribute2());
	// testClass.setIntAttribute1(10);
	// System.out.println(testClass.getIntAttribute2());
	// bindingContextAdapter.setTarget(null);
	//
	// }

}
