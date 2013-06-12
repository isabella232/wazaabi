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

package org.eclipse.wazaabi.engine.edp;

import org.eclipse.wazaabi.engine.edp.coderesolution.ComposedCodeLocator;
import org.eclipse.wazaabi.engine.edp.converters.ComposedBundledConverterFactory;
import org.eclipse.wazaabi.engine.edp.events.ComposedEventAdapterFactory;
import org.eclipse.wazaabi.engine.edp.events.ComposedEventHandlerAdapterFactory;
import org.eclipse.wazaabi.engine.edp.executables.ComposedExecutableAdapterFactory;
import org.eclipse.wazaabi.engine.edp.validators.ComposedBundledValidatorFactory;

/**
 * This class provides accessors to singletons required for running the
 * framework. It does not depend on the osgi framework therefore its methods are
 * able to be callable from a pure standalone java application (non osgi).
 * 
 * @author Olivier
 * 
 */
public class EDPSingletons {

	private static Registry registry = null;
	private static ComposedEventHandlerAdapterFactory composedEventHandlerAdapterFactory = null;
	private static ComposedEventAdapterFactory composedEventAdapterFactory = null;
	private static ComposedCodeLocator composedCodeLocator = null;
	private static ComposedExecutableAdapterFactory composedExecutableAdapterFactory = null;
	private static ComposedBundledConverterFactory composedBundledConverterFactory = null;
	private static ComposedBundledValidatorFactory composedBundledValidatorFactory = null;

	/**
	 * Sets the Registry unique instance, this method is supposed to be called
	 * by the framework during its initialization phase and must not be called
	 * by implementors.
	 */
	public static void setRegistry(Registry registry) {
		EDPSingletons.registry = registry;
	}

	/**
	 * Returns the unique Registry instance.
	 * 
	 * @return The Registry instance, could be null if the start of the
	 *         framework did not set it.
	 */
	public static Registry getRegistry() {
		return registry;
	}

//	/**
//	 * Returns the unique ComposedEventHandlerAdapterFactory instance.
//	 * 
//	 * @return The ComposedEventHandlerAdapterFactory instance, could be null if
//	 *         the start of the framework did not set it.
//	 */
//	public static ComposedEventHandlerAdapterFactory getComposedEventHandlerAdapterFactory() {
//		return composedEventHandlerAdapterFactory;
//	}
//
//	/**
//	 * Sets the ComposedEventHandlerAdapterFactory unique instance, this method
//	 * is supposed to be called by the framework during its initialization phase
//	 * and must not be called by implementors.
//	 */
//	public static void setComposedEventHandlerAdapterFactory(
//			ComposedEventHandlerAdapterFactory composedEventHandlerAdapterFactory) {
//		EDPSingletons.composedEventHandlerAdapterFactory = composedEventHandlerAdapterFactory;
//	}

//	/**
//	 * Returns the unique ComposedEventAdapterFactory instance.
//	 * 
//	 * @return The ComposedEventAdapterFactory instance, could be null if the
//	 *         start of the framework did not set it.
//	 */
//	public static ComposedEventAdapterFactory getComposedEventAdapterFactory() {
//		return composedEventAdapterFactory;
//	}
//
//	/**
//	 * Sets the ComposedEventAdapterFactory unique instance, this method is
//	 * supposed to be called by the framework during its initialization phase
//	 * and must not be called by implementors.
//	 */
//	public static void setComposedEventAdapterFactory(
//			ComposedEventAdapterFactory composedEventAdapterFactory) {
//		EDPSingletons.composedEventAdapterFactory = composedEventAdapterFactory;
//	}
	
//	/**
//	 * Returns the unique ComposedSequenceHandlerAdapterFactory instance.
//	 * 
//	 * @return The ComposedSequenceHandlerAdapterFactory instance, could be null if
//	 *         the start of the framework did not set it.
//	 */
//	public static ComposedExecutableAdapterFactory getComposedExecutableAdapterFactory() {
//		return composedExecutableAdapterFactory;
//	}
//
//	/**
//	 * Sets the ComposedSequenceHandlerAdapterFactory unique instance, this method
//	 * is supposed to be called by the framework during its initialization phase
//	 * and must not be called by implementors.
//	 */
//	public static void setComposedExecutableAdapterFactory(
//			ComposedExecutableAdapterFactory composedExecutableAdapterFactory) {
//		EDPSingletons.composedExecutableAdapterFactory = composedExecutableAdapterFactory;
//	}
	

	public static ComposedCodeLocator getComposedCodeLocator() {
		return composedCodeLocator;
	}

	public static void setComposedCodeLocator(
			ComposedCodeLocator composedCodeLocator) {
		EDPSingletons.composedCodeLocator = composedCodeLocator;
	}
	
	/**
	 * Returns the unique ComposedBundledConverterFactory instance.
	 * 
	 * @return The ComposedBundledConverterFactory instance, could be null if
	 *         the start of the framework did not set it.
	 */
	public static ComposedBundledConverterFactory getComposedBundledConverterFactory() {
		return composedBundledConverterFactory;
	}

	/**
	 * Sets the ComposedBundledConverterFactory unique instance, this method
	 * is supposed to be called by the framework during its initialization phase
	 * and must not be called by implementors.
	 */
	public static void setComposedBundledConverterFactory(
			ComposedBundledConverterFactory composedBundledConverterFactory) {
		EDPSingletons.composedBundledConverterFactory = composedBundledConverterFactory;
	}
	
	/**
	 * Returns the unique ComposedBundledValidatorFactory instance.
	 * 
	 * @return The ComposedBundledValidatorFactory instance, could be null if
	 *         the start of the framework did not set it.
	 */
	public static ComposedBundledValidatorFactory getComposedBundledValidatorFactory() {
		return composedBundledValidatorFactory;
	}

	/**
	 * Sets the ComposedBundledvalidatorFactory unique instance, this method
	 * is supposed to be called by the framework during its initialization phase
	 * and must not be called by implementors.
	 */
	public static void setComposedBundledValidatorFactory(
			ComposedBundledValidatorFactory composedBundledValidatorFactory) {
		EDPSingletons.composedBundledValidatorFactory = composedBundledValidatorFactory;
	}

}
