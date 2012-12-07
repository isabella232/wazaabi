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

import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.wazaabi.engine.edp.EDPSingletons;
import org.eclipse.wazaabi.engine.edp.exceptions.OperationAborted;
import org.eclipse.wazaabi.engine.edp.validators.BundledValidator;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.wazaabi.mm.edp.handlers.Executable;

public class ValidatorAdapter extends AbstractOperationAdapter {

	private static final MethodSignature[] METHOD_SIGNATURES = new MethodSignature[] { new MethodSignature(
			"isValid", new String[] { "eventDispatcher", "eventHandler" },
			new Class[] { EventDispatcher.class, EventHandler.class },
			boolean.class) };

	private BundledValidator bundledValidator = null;

	// public ValidatorAdapter() {
	// executeMethodName = "isValid";
	// }

	// protected void registerMethods(AbstractCodeDescriptor codeDescriptor) {
	// if (this.getClass()
	// .toString()
	// .equalsIgnoreCase(
	// "class org.eclipse.wazaabi.engine.edp.adapters.ValidatorAdapter")) {
	// if (((Deferred) getInnerDeferredAdapter().getTarget())
	// .eIsSet(((Deferred) getInnerDeferredAdapter().getTarget())
	// .eClass().getEStructuralFeature("uri")))
	// setExecuteMethodDescriptor(codeDescriptor.getMethodDescriptor(
	// executeMethodName, new String[] { "eventDispatcher",
	// "eventHandler" }, new Class[] {
	// EventDispatcher.class, EventHandler.class },
	//						boolean.class)); //$NON-NLS-1$
	// }
	// super.registerMethods(codeDescriptor);
	// }

	@Override
	public void setTarget(Notifier newTarget) {
		// We allow the converterAdapter to resolve both the OSGi DS converter
		// referenced by the uri and the deferred converter.
		// At run time priority goes to the OSGi DS converter.

		if (((Executable) newTarget).getId() != null
				&& !((Executable) newTarget).getId().isEmpty()) {
			if (EDPSingletons.getComposedBundledValidatorFactory() != null)
				bundledValidator = EDPSingletons
						.getComposedBundledValidatorFactory()
						.createBundledValidator(this,
								((Executable) newTarget).getId());
			// bundledValidator = createBundledValidatorFor(((Executable)
			// newTarget)
			// .getId());
			if (bundledValidator == null)
				throw new RuntimeException("no validator found"); //$NON-NLS-1$
			// TODO : we need to log that
			// attachBundledValidator(((Executable) newTarget).getId());
		}

		/*
		 * else if (((Deferred) newTarget).getUri() != null && !((Deferred)
		 * newTarget).getUri().isEmpty()) {
		 * getInnerDeferredAdapter().setTarget(newTarget); }
		 */

		// if (this.getClass()
		// .toString()
		// .equalsIgnoreCase(
		// "class org.eclipse.wazaabi.engine.edp.adapters.ValidatorAdapter")) {
		//
		// if (newTarget != null
		// && ((Executable) newTarget).eIsSet(((Executable) newTarget)
		// .eClass().getEStructuralFeature("id"))) {
		// attachBundledValidator(((Executable) newTarget).getId());
		// }
		// if (newTarget != null
		// && ((Deferred) newTarget).eIsSet(((Deferred) newTarget)
		// .eClass().getEStructuralFeature("uri")))
		// getInnerDeferredAdapter().setTarget(newTarget);
		// }
		//
		super.setTarget(newTarget);
	}

	// protected void attachBundledValidator(String id) {
	// BundledValidator bundledValidator = createBundledValidatorFor(id);
	// // if(bundledConverter != null)
	// setInnerBundledValidator(bundledValidator);
	// }
	//
	// protected void detachBundledValidator() {
	// // innerBundledValidator.dispose();
	// this.innerBundledValidator = null;
	// }

	// public BundledValidator getInnerBundledValidator() {
	// return innerBundledValidator;
	// }
	//
	// protected void setInnerBundledValidator(BundledValidator
	// bundledValidator) {
	// this.innerBundledValidator = bundledValidator;
	// }

	// protected BundledValidator createBundledValidatorFor(String id) {
	// BundledValidator bundledValidator = null;
	// if (EDPSingletons.getComposedBundledValidatorFactory() != null) {
	// bundledValidator = EDPSingletons
	// .getComposedBundledValidatorFactory()
	// .createBundledValidator(this, id);
	// }
	// return bundledValidator;
	// }

	@Override
	public void trigger(EventDispatcher eventDispatcher,
			EventHandler eventHandler, Event event) throws OperationAborted {
		boolean valid = false;
		try {
			valid = this.isValid(eventDispatcher, eventHandler);
		} catch (RuntimeException e) {
			throw (OperationAborted) e.getCause();
		}
		if (!valid) {
			throw new OperationAborted(this);
		}

		// TODO put data in context
	}

	protected boolean isValid(EventDispatcher eventDispatcher,
			EventHandler eventHandler) {
		Object returnedValue = null;
		if (bundledValidator != null) {
			returnedValue = bundledValidator.validate(eventDispatcher,
					eventHandler);
		} else if (getMethodDescriptor(0) != null) {
			returnedValue = getCodeDescriptor().invokeMethod(
					getMethodDescriptor(0),
					new Object[] { eventDispatcher, eventHandler });
		}
		if (returnedValue instanceof Boolean)
			return ((Boolean) returnedValue).booleanValue();
		// {
		// if (Boolean.FALSE.equals(returnedValue))
		// return false;
		// if (Boolean.TRUE.equals(returnedValue))
		// return true;
		// }
		return true;
	}

	public MethodSignature[] getMethodSignatures() {
		return METHOD_SIGNATURES;
	}

	public String getErrorMessage() {
		return null;
	}
}
