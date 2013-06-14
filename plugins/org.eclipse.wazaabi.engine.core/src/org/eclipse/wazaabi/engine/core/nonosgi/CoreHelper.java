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

package org.eclipse.wazaabi.engine.core.nonosgi;

import org.eclipse.wazaabi.engine.edp.nonosgi.EDPHelper;
import org.eclipse.wazaabi.mm.core.CorePackage;

public class CoreHelper {

	private static boolean neverCalled = true;

	/**
	 * Initializes the CoreSingletons class when called from a non osgi
	 * environment. Could be called more than once.
	 */
	public static synchronized void init() {
		if (!neverCalled)
			return;
		EDPHelper.init();
		CorePackage.eINSTANCE.eClass();
//		CoreSingletons
//				.setComposedEditPartFactory(new ComposedEditPartFactoryImpl());
//		CoreSingletons
//				.setComposedWidgetViewFactory(new ComposedWidgetViewFactoryImpl());
//		CoreSingletons
//				.setComposedStyleRuleManagerFactory(new ComposedStyleRuleManagerFactoryImpl());
//		CoreSingletons
//				.setComposedAnnotationManagerFactory(new ComposedAnnotationManagerFactoryImpl());
//		CoreSingletons
//				.setComposedCellEditorFactory(new ComposedCellEditorFactoryImpl());
//
//		CoreSingletons.getComposedEditPartFactory().addEditPartFactory(
//				new CoreEditPartFactory());
//		CoreSingletons.getComposedStyleRuleManagerFactory()
//				.addStyleRuleManagerFactory(new CoreStyleRuleManagerFactory());
//		CoreSingletons
//				.getComposedAnnotationManagerFactory()
//				.addAnnotationManagerFactory(new CoreAnnotationManagerFactory());
//		EDPSingletons.getComposedEventHandlerAdapterFactory()
//				.addEventHandlerAdapterFactory(
//						new CoreEventHandlerAdapterFactory());

		neverCalled = false;
	}

}
