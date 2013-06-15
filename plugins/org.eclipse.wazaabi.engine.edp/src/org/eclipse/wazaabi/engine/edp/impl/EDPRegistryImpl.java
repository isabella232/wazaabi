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

package org.eclipse.wazaabi.engine.edp.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.wazaabi.engine.edp.AdapterFactory;
import org.eclipse.wazaabi.engine.edp.ComponentFactory;
import org.eclipse.wazaabi.engine.edp.IdentifiedFactory;
import org.eclipse.wazaabi.engine.edp.Registry;
import org.eclipse.wazaabi.engine.edp.adapters.EventAdapter;
import org.eclipse.wazaabi.engine.edp.adapters.EventHandlerAdapter;
import org.eclipse.wazaabi.engine.edp.coderesolution.AbstractCodeDescriptor;
import org.eclipse.wazaabi.engine.edp.coderesolution.ExecutableAdapter;
import org.eclipse.wazaabi.engine.edp.coderesolution.ICodeLocator;
import org.eclipse.wazaabi.engine.edp.converters.BundledConverter;
import org.eclipse.wazaabi.engine.edp.converters.BundledConverterFactory;
import org.eclipse.wazaabi.engine.edp.events.EventAdapterFactory;
import org.eclipse.wazaabi.engine.edp.events.EventHandlerAdapterFactory;
import org.eclipse.wazaabi.engine.edp.executables.ExecutableAdapterFactory;
import org.eclipse.wazaabi.engine.edp.internal.osgi.Activator;
import org.eclipse.wazaabi.engine.edp.validators.BundledValidator;
import org.eclipse.wazaabi.engine.edp.validators.BundledValidatorFactory;
import org.osgi.framework.InvalidSyntaxException;
import org.osgi.framework.ServiceReference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class EDPRegistryImpl implements Registry {

	private final Logger logger = LoggerFactory
			.getLogger(EDPRegistryImpl.class);

	private HashMap<Class<?>, List<Object>> activatedServices = new HashMap<Class<?>, List<Object>>();
	private HashMap<Object, ServiceReference<?>> serviceToServiceReference = new HashMap<Object, ServiceReference<?>>();
	private HashMap<ServiceReference<?>, Object> serviceReferenceToService = new HashMap<ServiceReference<?>, Object>();

	public Adapter createAdapter(Object callingContext, EObject model,
			Object creationHint, Class<?> returnedType) {
		if (returnedType == null || model == null)
			return null;

		Class<?> interfaze = getServiceInterfacerFor(returnedType);
		if (interfaze == null) {
			logger.error("No factory interface found for {}", returnedType); //$NON-NLS-1$
			return null;
		}
		IdentifiedFactory f = getFactoryFor(callingContext, model, creationHint, interfaze);
		if (f instanceof AdapterFactory) {
			return ((AdapterFactory) f).createAdapter(callingContext,
					model, creationHint);
		}
		return null;
	}

	/**
	 * Given an interface, returns a service implementing this interface by
	 * looking up into OSGI declarative services if running. Once a service has
	 * been found, this service is stored in a cache.
	 * 
	 * @param interfaze
	 * @return a list of services
	 */
	public List<Object> getServices(Class<?> interfaze) {

		//		logger.debug("seeking service for {}", interfaze); //$NON-NLS-1$
		if (interfaze == null)
			return null;

		// first we lookup into activatedServices
		List<Object> services = activatedServices.get(interfaze);
		if (services == null) {
			services = new ArrayList<Object>();
			activatedServices.put(interfaze, services);
		}

		// if running from within a OSGI container
		if (Activator.getDefault().getContext() != null) {
			List<Object> declaratedServices = new ArrayList<Object>();
			try {
				for (ServiceReference<?> sr : Activator.getDefault()
						.getContext().getServiceReferences(interfaze, null)) {

					// did we already meet this service reference ?
					if (serviceReferenceToService.containsKey(sr))
						continue;

					Object service = Activator.getDefault().getContext()
							.getService(sr);
					if (service == null)
						continue;

					serviceToServiceReference.put(service, sr);
					serviceReferenceToService.put(sr, service);
					declaratedServices.add(service);

					logger.debug("Discovered : {} from OSGI DS", //$NON-NLS-1$
							service);

				}
			} catch (InvalidSyntaxException e) {
				logger.error("{}", e);
			}
			services.addAll(declaratedServices);
		}
		return services;
	}

	/**
	 * Returns the service class name of the factory returning the given
	 * returned type. Factories are registered by osgi framework using the
	 * service class name.
	 * 
	 * @param returnedType
	 * @return
	 */
	protected Class<?> getServiceInterfacerFor(Class<?> returnedType) {
		if (EventAdapter.class.equals(returnedType))
			return EventAdapterFactory.class;
		if (EventHandlerAdapter.class.equals(returnedType))
			return EventHandlerAdapterFactory.class;
		if (ExecutableAdapter.class.equals(returnedType))
			return ExecutableAdapterFactory.class;
		if (AbstractCodeDescriptor.class.equals(returnedType))
			return ICodeLocator.class;
		if (BundledConverter.class.equals(returnedType))
			return BundledConverterFactory.class;
		if (BundledValidator.class.equals(returnedType))
			return BundledValidatorFactory.class;
		return null;
	}

	public IdentifiedFactory getFactoryFor(Object callingContext, Object model,
			Object creationHint, Class<?> interfaze) {
		if (model == null || interfaze == null)
			return null;

		List<Object> services = getServices(interfaze);

		for (Object service : services)
			if (service instanceof IdentifiedFactory
					&& ((IdentifiedFactory) service).isFactoryFor(
							callingContext, model, creationHint)) {
				//				logger.debug("found DeclaratedFactory : {}", service); //$NON-NLS-1$
				return (IdentifiedFactory) service;
			}

		return null;
	}

	public Object createComponent(Object callingContext, Object model,
			Object creationHint, Class<?> returnedType) {
		if (returnedType == null || model == null)
			return null;

		Class<?> interfaze = getServiceInterfacerFor(returnedType);
		if (interfaze == null) {
			logger.error("No factory interface found for {}", returnedType); //$NON-NLS-1$
			return null;
		}
		IdentifiedFactory f = getFactoryFor(callingContext, model, creationHint, interfaze);
		if (f instanceof ComponentFactory) {
			return ((ComponentFactory) f).createComponent(
					callingContext, model, creationHint);
		}
		return null;
	}

	@Override
	public void startBatchOptimization() {
		// TODO Auto-generated method stub

	}

	@Override
	public void endBatchOptimization() {
		// TODO Auto-generated method stub

	}

	@Override
	public void setServices(Class<?> interfaze, List<Object> services,
			boolean blockOSGI) {
		// TODO NOT FINISHED
		activatedServices.put(interfaze, services != null ? services
				: new ArrayList<Object>());
	}
}
