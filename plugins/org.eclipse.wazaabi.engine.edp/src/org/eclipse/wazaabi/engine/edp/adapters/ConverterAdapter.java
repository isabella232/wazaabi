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

package org.eclipse.wazaabi.engine.edp.adapters;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.wazaabi.engine.edp.EDP;
import org.eclipse.wazaabi.engine.edp.EDPSingletons;
import org.eclipse.wazaabi.engine.edp.converters.BundledConverter;
import org.eclipse.wazaabi.engine.edp.exceptions.OperationAborted;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.Converter;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.wazaabi.mm.edp.handlers.Executable;

public class ConverterAdapter extends ActionAdapterImpl {

	private static final MethodSignature[] METHOD_SIGNATURES = new MethodSignature[] { new MethodSignature(
			"convert", new String[] { "input" }, new Class[] { Object.class },
			Object.class) };

	private BundledConverter bundledConverter = null;

	// public ConverterAdapter() {
	// executeMethodName = "convert";
	// }

	// @Override
	// protected void registerMethods(AbstractCodeDescriptor codeDescriptor) {
	// if (((Deferred) getInnerDeferredAdapter().getTarget())
	// .eIsSet(((Deferred) getInnerDeferredAdapter().getTarget())
	// .eClass().getEStructuralFeature("uri")))
	// setExecuteMethodDescriptor(codeDescriptor.getMethodDescriptor(
	// executeMethodName, new String[] { "input" },
	//					new Class[] { Object.class }, Object.class)); //$NON-NLS-1$
	//
	// super.registerMethods(codeDescriptor);
	// }

	@Override
	public void setTarget(Notifier newTarget) {
		// We allow the converterAdapter to resolve both the OSGi DS converter
		// referenced
		// by the uri and the deferred converter.
		// At run time priority goes to the OSGi DS converter.

		// if (newTarget != null
		// && ((Executable) newTarget).eIsSet(((Executable) newTarget)
		// .eClass().getEStructuralFeature("id"))) {
		// attachBundledConverter(((Executable) newTarget).getId());
		// }
		// if (newTarget != null
		// && ((Deferred) newTarget).eIsSet(((Deferred) newTarget)
		// .eClass().getEStructuralFeature("uri")))
		// getInnerDeferredAdapter().setTarget(newTarget);

		if (((Executable) newTarget).getId() != null
				&& !((Executable) newTarget).getId().isEmpty()) {
			if (EDPSingletons.getComposedBundledConverterFactory() != null)
				bundledConverter = EDPSingletons
						.getComposedBundledConverterFactory()
						.createBundledConverter(this,
								((Executable) newTarget).getId());
			// bundledValidator = createBundledValidatorFor(((Executable)
			// newTarget)
			// .getId());
			if (bundledConverter == null)
				throw new RuntimeException("no validator found"); //$NON-NLS-1$
			// TODO : we need to log that
			// attachBundledValidator(((Executable) newTarget).getId());
		}

		super.setTarget(newTarget);
	}

	// protected void attachBundledConverter(String id) {
	// BundledConverter bundledConverter = createBundledConverterFor(id);
	// // if(bundledConverter != null)
	// setInnerBundledConverter(bundledConverter);
	// }
	//
	// protected void detachBundledConverter() {
	// // innerBundledConverter.dispose();
	// this.innerBundledConverter = null;
	// }
	//
	// public BundledConverter getInnerBundledConverter() {
	// return innerBundledConverter;
	// }
	//
	// protected void setInnerBundledConverter(BundledConverter
	// bundledConverter) {
	// this.innerBundledConverter = bundledConverter;
	// }
	//
	// protected BundledConverter createBundledConverterFor(String id) {
	// BundledConverter bundledConverter = null;
	// if (EDPSingletons.getComposedBundledConverterFactory() != null) {
	// bundledConverter = EDPSingletons
	// .getComposedBundledConverterFactory()
	// .createBundledConverter(this, id);
	// }
	// return bundledConverter;
	// }

	@Override
	public void trigger(EventDispatcher eventDispatcher,
			EventHandler eventHandler, Event event) throws OperationAborted {
		try {
			Object sourceValue = eventDispatcher.get(EDP.VALUE_SOURCE_KEY);
			Object convert = null;
			if (sourceValue instanceof List) {
				if (((List<?>) sourceValue).size() == 1) {
					convert = this.convert(((List<?>) sourceValue).get(0));
					List<Object> result = new ArrayList<Object>();
					result.add(convert);
					eventDispatcher.set(EDP.CONVERTER_RESULT_KEY, result);
				} else {
					convert = this.convert(sourceValue);
					eventDispatcher.set(EDP.CONVERTER_RESULT_KEY, convert);
				}
			}

		} catch (Exception e) {
			throw new OperationAborted(this);
		}

	}

	public Object convert(Object input) {
		Object returnedValue = null;
		if (bundledConverter != null)
			returnedValue = bundledConverter.convert(input);
		else if (getMethodDescriptor(0) != null)
			returnedValue = getCodeDescriptor().invokeMethod(
					getMethodDescriptor(0), new Object[] { input });

		return returnedValue;
	}

	@Override
	public void notifyChanged(Notification notification) {
		switch (notification.getFeatureID(Executable.class)) {
		case EDPHandlersPackage.EXECUTABLE__ID:
			switch (notification.getEventType()) {
			case Notification.SET:
//				attachBundledConverter((String) notification.getNewValue());
				break;
			// TODO this case does never happen
			case Notification.UNSET:
//				detachBundledConverter();
				break;
			}
		}
		super.notifyChanged(notification);
	}

	@Override
	public boolean isAdapterForType(Object type) {
		return type instanceof Converter;
	}

	public MethodSignature[] getMethodSignatures() {
		return METHOD_SIGNATURES;
	}
}
