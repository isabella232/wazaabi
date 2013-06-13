package org.eclipse.wazaabi.engine.edp;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.wazaabi.engine.edp.adapters.EventAdapter;
import org.eclipse.wazaabi.engine.edp.adapters.EventHandlerAdapter;
import org.eclipse.wazaabi.engine.edp.coderesolution.ExecutableAdapter;
import org.eclipse.wazaabi.engine.edp.events.EventAdapterFactory;
import org.eclipse.wazaabi.engine.edp.events.EventHandlerAdapterFactory;
import org.eclipse.wazaabi.engine.edp.executables.ExecutableAdapterFactory;
import org.eclipse.wazaabi.engine.edp.internal.osgi.Activator;
import org.osgi.framework.InvalidSyntaxException;
import org.osgi.framework.ServiceReference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class EDPRegistryImpl implements EDPFactory111 {

	private final Logger logger = LoggerFactory.getLogger(EDPRegistryImpl.class);

	private HashMap<Class<?>, List<Object>> activatedServices = new HashMap<Class<?>, List<Object>>();
	private HashMap<Object, ServiceReference<?>> serviceToServiceReference = new HashMap<Object, ServiceReference<?>>();
	private HashMap<ServiceReference<?>, Object> serviceReferenceToService = new HashMap<ServiceReference<?>, Object>();

	public Adapter createAdapter(Object callingContext, EObject model,
			Class<?> returnedType) {
		DeclaratedFactory f = getFactoryFor(callingContext, model, returnedType);
		if (f instanceof DeclaratedAdapterFactory) {
			return ((DeclaratedAdapterFactory) f).createAdapter(callingContext,
					model);
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

//					logger.debug("Found declarative service : {}", //$NON-NLS-1$
//							service);

				}
			} catch (InvalidSyntaxException e) {
				logger.error("{}", e);
			}
			services.addAll(declaratedServices);
			if (logger.isDebugEnabled())
				for (Object service : declaratedServices)
					logger.debug("Added {} to declarated services", service);
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
		return null;
	}

	public DeclaratedFactory getFactoryFor(Object callingContext, Object model,
			Class<?> returnedType) {
		if (model == null || returnedType == null)
			return null;

//		logger.debug("Seeking factory for {}, {}, {}", new Object[] { //$NON-NLS-1$
//				callingContext, model, returnedType });
		Class<?> interfaze = getServiceInterfacerFor(returnedType);
		if (interfaze == null) {
			logger.error("No factory interface found for {}", returnedType); //$NON-NLS-1$
			return null;
		}

		List<Object> services = getServices(interfaze);

		for (Object service : services)
			if (service instanceof DeclaratedFactory
					&& ((DeclaratedFactory) service).isFactoryFor(
							callingContext, model)) {
//				logger.debug("found DeclaratedFactory : {}", service); //$NON-NLS-1$
				return (DeclaratedFactory) service;
			}

		return null;
	}

	public Object createComponent(Object callingContext, Object model,
			Object creationHints, Class<?> returnedType) {
		DeclaratedFactory f = getFactoryFor(callingContext, model, returnedType);
		if (f instanceof DeclaratedComponentFactory) {
			return ((DeclaratedComponentFactory) f).createComponent(
					callingContext, model, creationHints);
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

}
