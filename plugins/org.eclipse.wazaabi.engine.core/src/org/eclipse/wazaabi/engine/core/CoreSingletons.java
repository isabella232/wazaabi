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

package org.eclipse.wazaabi.engine.core;

import org.eclipse.wazaabi.engine.core.annotations.factories.ComposedAnnotationManagerFactory;
import org.eclipse.wazaabi.engine.core.celleditors.factories.ComposedCellEditorFactory;
import org.eclipse.wazaabi.engine.core.editparts.factories.ComposedEditPartFactory;
import org.eclipse.wazaabi.engine.core.stylerules.factories.ComposedStyleRuleManagerFactory;
import org.eclipse.wazaabi.engine.core.views.factories.ComposedWidgetViewFactory;

/**
 * This class provides accessors to singletons required for running the
 * framework. It does not depend on the osgi framework therefore its methods are
 * able to be callable from a pure standalone java application (non osgi).
 * 
 * @author Olivier
 * 
 */
public class CoreSingletons {

	// private static AbstractUIEventAdapterFactory
	// abstractUIEventAdapterFactory = null;
	private static ComposedEditPartFactory composedEditPartFactory = null;
	// private static ComposedAdapterFactory composedAdapterFactory = null;
	private static ComposedWidgetViewFactory composedWidgetViewFactory = null;
	private static ComposedStyleRuleManagerFactory composedStyleRuleManagerFactory = null;
	private static ComposedAnnotationManagerFactory composedAnnotationManagerFactory = null;
	private static ComposedCellEditorFactory composedCellEditorFactory = null;

	// /**
	// * Returns the unique AbstractUIEventAdapterFactory instance.
	// *
	// * @return The central AbstractUIEventAdapterFactory, could be null if the
	// * start of the framework did not set it.
	// */
	// public static AbstractUIEventAdapterFactory
	// getAbstractUIEventAdapterFactory() {
	// return abstractUIEventAdapterFactory;
	// }

	/**
	 * Returns the unique ComposedEditPartFactory instance.
	 * 
	 * @return The ComposedEditPartFactory instance, could be null if the start
	 *         of the framework did not set it.
	 */
	public static ComposedEditPartFactory getComposedEditPartFactory() {
		return composedEditPartFactory;
	}

	// /**
	// * Sets the AbstractUIEventAdapterFactory unique instance, this method is
	// * supposed to be called by the framework during its initialization phase
	// * and must not be called by implementors.
	// */
	// public static void setAbstractUIEventAdapterFactory(
	// AbstractUIEventAdapterFactory abstractUIEventAdapterFactory) {
	// CoreSingletons.abstractUIEventAdapterFactory =
	// abstractUIEventAdapterFactory;
	// }

	/**
	 * Sets the ComposedEditPartFactory unique instance, this method is supposed
	 * to be called by the framework during its initialization phase and must
	 * not be called by implementors.
	 */
	public static void setComposedEditPartFactory(
			ComposedEditPartFactory composedEditPartFactory) {
		CoreSingletons.composedEditPartFactory = composedEditPartFactory;
	}

	// /**
	// * Returns the unique ComposedAdapterFactory instance.
	// *
	// * @return The ComposedAdapterFactory instance, could be null if the start
	// * of the framework did not set it.
	// */
	// public static ComposedAdapterFactory getComposedAdapterFactory() {
	// return composedAdapterFactory;
	// }

	// /**
	// * Sets the ComposedAdapterFactory unique instance, this method is
	// supposed
	// * to be called by the framework during its initialization phase and must
	// * not be called by implementors.
	// */
	// public static void setComposedAdapterFactory(
	// ComposedAdapterFactory composedAdapterFactory) {
	// CoreSingletons.composedAdapterFactory = composedAdapterFactory;
	// }

	/**
	 * Returns the unique ComposedWidgetViewFactory instance.
	 * 
	 * @return The ComposedWidgetViewFactory instance, could be null if the
	 *         start of the framework did not set it.
	 */
	public static ComposedWidgetViewFactory getComposedWidgetViewFactory() {
		return composedWidgetViewFactory;
	}

	/**
	 * Sets the ComposedWidgetViewFactory unique instance, this method is
	 * supposed to be called by the framework during its initialization phase
	 * and must not be called by implementors.
	 */
	public static void setComposedWidgetViewFactory(
			ComposedWidgetViewFactory composedWidgetViewFactory) {
		CoreSingletons.composedWidgetViewFactory = composedWidgetViewFactory;
	}

	/**
	 * Returns the unique ComposedStyleRuleManagerFactory instance.
	 * 
	 * @return The ComposedStyleRuleManagerFactory instance, could be null if
	 *         the start of the framework did not set it.
	 */
	public static ComposedStyleRuleManagerFactory getComposedStyleRuleManagerFactory() {
		return composedStyleRuleManagerFactory;
	}

	/**
	 * Sets the ComposedStyleRuleManagerFactory unique instance, this method is
	 * supposed to be called by the framework during its initialization phase
	 * and must not be called by implementors.
	 */
	public static void setComposedStyleRuleManagerFactory(
			ComposedStyleRuleManagerFactory composedStyleRuleManagerFactory) {
		CoreSingletons.composedStyleRuleManagerFactory = composedStyleRuleManagerFactory;
	}

	/**
	 * Returns the unique ComposedAnnotationManagerFactory instance.
	 * 
	 * @return The ComposedAnnotationManagerFactory instance, could be null if
	 *         the start of the framework did not set it.
	 */
	public static ComposedAnnotationManagerFactory getComposedAnnotationManagerFactory() {
		return composedAnnotationManagerFactory;
	}

	/**
	 * Sets the ComposedAnnotationManagerFactory unique instance, this method is
	 * supposed to be called by the framework during its initialization phase
	 * and must not be called by implementors.
	 */
	public static void setComposedAnnotationManagerFactory(
			ComposedAnnotationManagerFactory composedAnnotationManagerFactory) {
		CoreSingletons.composedAnnotationManagerFactory = composedAnnotationManagerFactory;
	}

	/**
	 * Returns the unique ComposedCellEditorFactory instance.
	 * 
	 * @return The ComposedCellEditorFactory instance, could be null if the
	 *         start of the framework did not set it.
	 */
	public static ComposedCellEditorFactory getComposedCellEditorFactory() {
		return composedCellEditorFactory;
	}

	/**
	 * Sets the ComposedCellEditorFactory unique instance, this method is
	 * supposed to be called by the framework during its initialization phase
	 * and must not be called by implementors.
	 */
	public static void setComposedCellEditorFactory(
			ComposedCellEditorFactory composedCellEditorFactory) {
		CoreSingletons.composedCellEditorFactory = composedCellEditorFactory;
	}
}
