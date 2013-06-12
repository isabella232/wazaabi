package org.eclipse.wazaabi.engine.edp.internal.osgi;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.wazaabi.engine.edp.DeclaratedAdapterFactory;
import org.eclipse.wazaabi.engine.edp.DeclaratedFactory;
import org.eclipse.wazaabi.engine.edp.EDPFactory111;
import org.osgi.framework.InvalidSyntaxException;
import org.osgi.framework.ServiceReference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class EDPFactoryImpl implements EDPFactory111 {

	private final Logger logger = LoggerFactory.getLogger(EDPFactoryImpl.class);

	private HashMap<Class<?>, List<DeclaratedFactory>> activatedFactories = new HashMap<Class<?>, List<DeclaratedFactory>>();
	private HashMap<DeclaratedFactory, ServiceReference<?>> serviceReferences = new HashMap<DeclaratedFactory, ServiceReference<?>>();

	public Adapter createAdapter(Object callingContext, EObject model,
			Class<?> returnedType) {
		DeclaratedFactory f = getFactoryFor(callingContext, model, returnedType);
		if (f instanceof DeclaratedAdapterFactory) {
			return ((DeclaratedAdapterFactory) f).createAdapter(callingContext,
					model);
		}
		return null;
	}

	protected DeclaratedFactory getFactoryFor(Object callingContext,
			EObject model, Class<?> returnedType) {
		if (model == null || returnedType == null)
			return null;

		logger.debug("Seeking factory for {}, {}, {}", new Object[] {
				callingContext, model, returnedType });
		List<DeclaratedFactory> existingFactories = activatedFactories
				.get(returnedType);
		if (existingFactories != null) {
			for (DeclaratedFactory factory : existingFactories)
				if (factory.isFactoryFor(callingContext, model)) {
					logger.debug("Found existing factory {} for {}",
							new Object[] { factory, returnedType });
					return factory;
				}
		}

		if (Activator.getDefault().getContext() != null) {
			List<DeclaratedFactory> declaratedFactories = null;
			try {
				for (ServiceReference<?> sr : Activator.getDefault()
						.getContext()
						.getServiceReferences(returnedType.getName(), null)) {
					DeclaratedFactory declaratedFactory = (DeclaratedFactory) Activator
							.getDefault().getContext().getService(sr);
					if (declaratedFactory == null)
						continue;
					serviceReferences.put(declaratedFactory, sr);

					declaratedFactories = activatedFactories.get(returnedType);
					if (declaratedFactories == null) {
						declaratedFactories = new ArrayList<DeclaratedFactory>();
						declaratedFactories.add(declaratedFactory);
						activatedFactories.put(returnedType,
								declaratedFactories);
						logger.debug("added {} to activatedFactories",
								declaratedFactory);
					} else if (!declaratedFactories.contains(declaratedFactory)) {
						declaratedFactories.add(declaratedFactory);
						logger.debug("added {} to activatedFactories",
								declaratedFactory);
					}
				}
			} catch (InvalidSyntaxException e) {
				// CANNOT OCCUR !
			}
			if (declaratedFactories != null) {
				for (DeclaratedFactory factory : declaratedFactories)
					if (factory.isFactoryFor(callingContext, model))
						return factory;
			}
		}
		return null;
	}

	public Object createComponent(Object callingContext, Object props,
			Class<?> returnedType) {
		// TODO Auto-generated method stub
		return null;
	}
}
