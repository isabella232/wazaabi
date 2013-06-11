package org.eclipse.wazaabi.engine.edp;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.ecore.EObject;

public interface EDPFactory {

	public Adapter createAdapter(Object callingContext, EObject model,
			Class<?> returnedType);

	public Object createComponent(Object callingContext, Object props,
			Class<?> returnedType);
}
