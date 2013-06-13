package org.eclipse.wazaabi.engine.edp;

import java.util.List;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.ecore.EObject;

public interface EDPFactory111 {

	public Adapter createAdapter(Object callingContext, EObject model,
			Object creationHint, Class<?> returnedType);

	public Object createComponent(Object callingContext, Object model,
			Object creationHint, Class<?> returnedType);

	public DeclaratedFactory getFactoryFor(Object callingContext, Object model,
			Class<?> interfaze);

	/**
	 * Given an interface, returns a service implementing this interface by
	 * looking up into OSGI declarative services if running. Once a service has
	 * been found, this service is stored in a cache.
	 * 
	 * @param interfaze
	 * @return
	 */
	public List<Object> getServices(Class<?> interfaze);

	public void startBatchOptimization();

	public void endBatchOptimization();

	// public void setServices(Class<?> interfaze, List<Object> services);
}
