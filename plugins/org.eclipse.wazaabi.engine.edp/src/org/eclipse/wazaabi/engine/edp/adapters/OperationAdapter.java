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
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.wazaabi.engine.edp.coderesolution.AbstractCodeDescriptor;
import org.eclipse.wazaabi.engine.edp.coderesolution.AbstractCodeDescriptor.MethodDescriptor;
import org.eclipse.wazaabi.engine.edp.coderesolution.AbstractDeferredAdapter;
import org.eclipse.wazaabi.engine.edp.coderesolution.DeferredAdapter;
import org.eclipse.wazaabi.engine.edp.coderesolution.ExecutableAdapter;
import org.eclipse.wazaabi.engine.edp.exceptions.OperationAborted;
import org.eclipse.wazaabi.engine.edp.locationpaths.IPointersEvaluator;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.Deferred;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.wazaabi.mm.edp.handlers.Operation;

public abstract class OperationAdapter extends AdapterImpl implements
		DeferredAdapter, ExecutableAdapter {

	private EventHandlerAdapter eventHandlerAdapter = null;
	private MethodDescriptor executeMethodDescriptor = null;
	private MethodDescriptor errorMethodDescriptor = null;
	protected String executeMethodName;
	
	private final AbstractDeferredAdapter innerDeferredAdapter = new AbstractDeferredAdapter() {
		@Override
		protected void registerMethods(AbstractCodeDescriptor codeDescriptor) {	
			OperationAdapter.this.registerMethods(codeDescriptor);
		}
	};
	

	protected AbstractDeferredAdapter getInnerDeferredAdapter() {
		return innerDeferredAdapter;
	}

	public void setTarget(Notifier newTarget) {
		if (!this.getClass().toString().equalsIgnoreCase("class org.eclipse.wazaabi.engine.edp.adapters.ConverterAdapter")){
			getInnerDeferredAdapter().setTarget(newTarget);
		}
		super.setTarget(newTarget);
	}
	
	public void unsetTarget(Notifier oldTarget) {
		getInnerDeferredAdapter().unsetTarget(oldTarget);
		super.unsetTarget(oldTarget);
	}
	
	public void notifyChanged(Notification notification) {
		getInnerDeferredAdapter().notifyChanged(notification);
		super.notifyChanged(notification);
	}

	public AbstractCodeDescriptor getCodeDescriptor() {
		return getInnerDeferredAdapter().getCodeDescriptor();
	}

	public void dispose() {
		getInnerDeferredAdapter().dispose();
	}


	protected MethodDescriptor getExecuteMethodDescriptor() {
		return executeMethodDescriptor;
	}
	
	protected void setExecuteMethodDescriptor(AbstractCodeDescriptor.MethodDescriptor executeMethodDescriptor) {
		this.executeMethodDescriptor = executeMethodDescriptor;
	}
	
	protected MethodDescriptor getErrorMethodDescriptor() {
		return errorMethodDescriptor;
	}
	
	protected void setErrorMethodDescriptor(AbstractCodeDescriptor.MethodDescriptor executeMethodDescriptor) {
		this.errorMethodDescriptor = executeMethodDescriptor;
	}
	
	protected void setEventHandlerAdapter(
			EventHandlerAdapter eventHandlerAdapter) {
		this.eventHandlerAdapter = eventHandlerAdapter;
	}
	
	protected EventHandlerAdapter getEventHandlerAdapter() {
		return eventHandlerAdapter;
	}
	
	public boolean isAdapterForType(Object type) {
		return type instanceof Operation;
	}
	
	/**
	 * Registers all the methods required for this DeferredAdapter given a non
	 * null <code>CodeDescriptor</code>. Implementors must override this method
	 * in order to improve performances.
	 * 
	 * @param codeDescriptor
	 *            The <code>CodeDescriptor</code>, canot be null.
	 */
	protected void registerMethods(AbstractCodeDescriptor codeDescriptor) {
		setErrorMethodDescriptor(codeDescriptor.getMethodDescriptor("errorMessage",
				new String[] {  },
				new Class[] {  }, String.class)); //$NON-NLS-1$
	}
	
	abstract public void trigger(EventDispatcher eventDispatcher, EventHandler eventHandler , Event event) throws OperationAborted; 
	
	public IPointersEvaluator getPointersEvaluator() {
		if (getEventHandlerAdapter() != null)
			return getEventHandlerAdapter().getPointersEvaluator();
		return null;
	}

	public String getErrorMessage() {
		String message = null;
		if (getExecuteMethodDescriptor() != null) {
			message = (String) getCodeDescriptor().invokeMethod(
					getErrorMethodDescriptor(),
					new Object[] {  });
			if (message != null) {
				return message;
			}
		}
		return "Operation error";
	}
	


}
