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

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.wazaabi.engine.edp.EDPSingletons;
import org.eclipse.wazaabi.engine.edp.exceptions.OperationAborted;
import org.eclipse.wazaabi.engine.edp.validators.BundledValidator;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.wazaabi.mm.edp.handlers.Executable;
import org.eclipse.wazaabi.mm.edp.handlers.Validator;

public class ValidatorAdapter extends AbstractOperationAdapter {

	private static final MethodSignature[] METHOD_SIGNATURES = new MethodSignature[] {
			new MethodSignature("isValid", new String[] { "eventDispatcher",
					"eventHandler" }, new Class[] { EventDispatcher.class,
					EventHandler.class }, boolean.class),
			new MethodSignature("getErrorMessage", new String[] {},
					new Class[] {}, String.class) };

	private BundledValidator bundledValidator = null;
	private String bundledValidatorId = null;

	@Override
	public void setTarget(Notifier newTarget) {
		if (newTarget != null) {
			// We allow the ValidatorAdapter to resolve both the OSGi DS
			// Validator
			// referenced by it id and the deferred Validator.
			// At run time priority goes to the OSGi DS Validator.

			String validatorId = ((Validator) newTarget).getId();
			if (validatorId != null && !validatorId.isEmpty()) {
				if (EDPSingletons.getComposedBundledValidatorFactory() != null)
					bundledValidator = EDPSingletons
							.getComposedBundledValidatorFactory()
							.createBundledValidator(this, validatorId);
				if (bundledValidator == null)
					throw new RuntimeException(
							"no validator found for: " + validatorId); //$NON-NLS-1$
				// TODO : we need to log that
			}
		} else
			detachBundledValidator();

		super.setTarget(newTarget);
	}

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
			returnedValue = bundledValidator.isValid(eventDispatcher,
					eventHandler);
		} else if (getMethodDescriptor(0) != null) {
			returnedValue = getCodeDescriptor().invokeMethod(
					getMethodDescriptor(0),
					new Object[] { eventDispatcher, eventHandler });
		}
		if (returnedValue instanceof Boolean)
			return ((Boolean) returnedValue).booleanValue();
		return true;
	}

	public MethodSignature[] getMethodSignatures() {
		return METHOD_SIGNATURES;
	}

	public String getErrorMessage() {
		String errorMessage = null;
		if (bundledValidator != null)
			return bundledValidator.getErrorMessage();
		if (getMethodDescriptor(1) != null)
			errorMessage = (String) getCodeDescriptor().invokeMethod(
					getMethodDescriptor(1), new Object[] {});
		return errorMessage;
	}

	protected void attachBundledValidator(String bundleValidatorId) {
		// we don't attach the same BundledValidator
		if (bundleValidatorId != null
				&& bundleValidatorId.equals(this.bundledValidatorId)
				&& bundledValidator != null && !bundledValidator.isDisposed())
			return;
		detachBundledValidator();
		if (bundleValidatorId != null && !bundleValidatorId.isEmpty()) {
			bundledValidator = EDPSingletons
					.getComposedBundledValidatorFactory()
					.createBundledValidator(this, bundleValidatorId);
			if (bundledValidator != null)
				this.bundledValidatorId = bundleValidatorId;
			else
				this.bundledValidatorId = null;
		}

	}

	protected void detachBundledValidator() {
		if (bundledValidator != null && !bundledValidator.isDisposed()) {
			bundledValidator.dispose();
			bundledValidator = null;
			this.bundledValidatorId = null;
		}
	}

	@Override
	public void notifyChanged(Notification notification) {
		switch (notification.getFeatureID(Executable.class)) {
		case EDPHandlersPackage.EXECUTABLE__ID:
			switch (notification.getEventType()) {
			case Notification.SET:
				attachBundledValidator(notification.getNewStringValue());
				break;
			}
		}
		super.notifyChanged(notification);
	}
}
