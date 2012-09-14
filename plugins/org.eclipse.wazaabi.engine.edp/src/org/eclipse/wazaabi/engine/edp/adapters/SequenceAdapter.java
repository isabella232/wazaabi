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
import java.util.Collection;
import java.util.List;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.wazaabi.engine.edp.EDPSingletons;
import org.eclipse.wazaabi.engine.edp.coderesolution.ExecutableAdapter;
import org.eclipse.wazaabi.engine.edp.exceptions.OperationAborted;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.wazaabi.mm.edp.handlers.Executable;
import org.eclipse.wazaabi.mm.edp.handlers.Sequence;

public class SequenceAdapter extends AdapterImpl implements ExecutableAdapter {
	
	private List<ExecutableAdapter> operationAdapters = new ArrayList<ExecutableAdapter>();
	
	protected List<ExecutableAdapter> getExecutableAdapters() {
		return operationAdapters;
	}
	
	
	public void trigger(EventDispatcher eventDispatcher,
			EventHandler eventHandler, Event event) {
		for (ExecutableAdapter executable : getExecutableAdapters()) {
			try {
				executable.trigger( eventDispatcher,eventHandler,  event);
			} catch (RuntimeException e) {
				//e.printStackTrace();
				if (e instanceof OperationAborted) {
					throw e;
				} else {
					throw (OperationAborted)e.getCause();					
				}
			}
		}
	}
	
	
	public boolean isAdapterForType(Object type) {
		return type instanceof Sequence;
	}
	

	public void notifyChanged(Notification notification) {
		switch (notification.getFeatureID(Sequence.class)) {
		case EDPHandlersPackage.SEQUENCE__EXECUTABLES:
			switch (notification.getEventType()) {
			case Notification.ADD:
				adaptExecutable((Executable) notification.getNewValue());
				break;
			case Notification.ADD_MANY:
				@SuppressWarnings("unchecked")
				Collection<Executable> addedExecutables = (Collection<Executable>) notification.getNewValue();
				for (Executable executable : addedExecutables)
					adaptExecutable(executable);
				break;
			case Notification.REMOVE:
				unadaptExecutable((Executable) notification.getOldValue());
				break;
			case Notification.REMOVE_MANY:
				@SuppressWarnings("unchecked")
				Collection<Executable> removedExecutables = (Collection<Executable>) notification.getOldValue();
				for (Executable executable : removedExecutables)
					unadaptExecutable(executable);
				break;
			}
			return;
		}
	}
	
	
	public void setTarget(Notifier newTarget) {
		if (newTarget == getTarget())
			return;
		if (getTarget() != null){
			for (Executable executable : ((Sequence) getTarget()).getExecutables()){
				unadaptExecutable(executable);
			}
		}
		super.setTarget(newTarget);
		if (newTarget != null){
			for (Executable executable : ((Sequence) getTarget()).getExecutables()){
				adaptExecutable(executable);
			}
		}
	}
	

	private void unadaptExecutable(Executable oldValue) {
		ExecutableAdapter toRemove = null;
		for (Adapter adapter : oldValue.eAdapters())
			if (adapter instanceof SequenceAdapter
					&& ((SequenceAdapter) adapter).getExecutableAdapters() == this) {
				toRemove = (ExecutableAdapter) adapter;
				break;
			}
		if (toRemove != null) {
			oldValue.eAdapters().remove(toRemove);
			//toRemove.setExecutableAdapter(null);
		}
		executableRemoved(oldValue);
	}

	private void adaptExecutable(Executable newValue) {
		ExecutableAdapter adapter = createExecutableAdapterFor(newValue);
		if (adapter != null){
			//adapter.setExecutableAdapter((ExecutableAdapter) this);
			newValue.eAdapters().add(adapter);
			getExecutableAdapters().add(adapter);
		}
		executableAdded(newValue);
	}
	
	private ExecutableAdapter createExecutableAdapterFor(Executable executable) {
		ExecutableAdapter executableAdapter=null;
		if(EDPSingletons.getComposedExecutableAdapterFactory() != null){
			executableAdapter = EDPSingletons.getComposedExecutableAdapterFactory().createExecutableAdapter((ExecutableAdapter)this, executable);
		}
		return executableAdapter;

	}

	private void executableAdded(Executable newValue) {
		// TODO Auto-generated method stub
	}


	private void executableRemoved(Executable oldValue) {
		// TODO Auto-generated method stub
		
	}

	
	public void setExecutableAdapter(Executable executableAdapter) {
		// TODO Auto-generated method stub
	}


	public String getErrorMessage() {
		// TODO Auto-generated method stub
		return null;
	}

	
}
