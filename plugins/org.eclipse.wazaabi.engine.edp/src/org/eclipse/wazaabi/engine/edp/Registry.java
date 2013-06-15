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

package org.eclipse.wazaabi.engine.edp;

import java.util.List;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.ecore.EObject;

public interface Registry {

	public Adapter createAdapter(Object callingContext, EObject model,
			Object creationHint, Class<?> returnedType);

	public Object createComponent(Object callingContext, Object model,
			Object creationHint, Class<?> returnedType);

	public IdentifiedFactory getFactoryFor(Object callingContext, Object model,
			Object creationHint, Class<?> interfaze);

	/**
	 * Builds and returns a list of services which are implementing the given
	 * interface by looking up first into a cache and after from within
	 * registered OSGI declarative services as returned by OSGI container (if
	 * running). Each time a OSGI declarative service is discovered, it is
	 * activated and stored into the cache and will not be discovered anymore by
	 * declarative services lookup until it will be removed from the cache.
	 * 
	 * @param interfaze
	 *            the interface the services are implementing
	 * @return Always a list of services. The list if empty if no services are
	 *         found. This list is built every time the method is called.
	 */
	public List<Object> getServices(Class<?> interfaze);

	/**
	 * Registers a list of services for the given interface. This list replaces
	 * every time the previous one. If the blockOSGI is set to true, then the
	 * discovering of OSGI declarative services for this interface will be
	 * disabled until the next call to the method with a blockOSGI set to false.
	 * 
	 * @param interfaze
	 *            the interface the services are implementing
	 * @param services
	 *            A list of services. If null, it is replaced internally by an
	 *            empty list.
	 * @param blockOSGI
	 *            if true, the discovering of OSGI declarative services is
	 *            stopped for this interface until a call to the method with
	 *            blockOSGI set to false
	 */
	public void setServices(Class<?> interfaze, List<Object> services,
			boolean blockOSGI);

	public void startBatchOptimization();

	public void endBatchOptimization();

}
