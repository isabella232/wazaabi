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

package org.eclipse.wazaabi.engine.edp.events;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.wazaabi.engine.edp.adapters.BindingAdapter;
import org.eclipse.wazaabi.engine.edp.adapters.EventHandlerAdapter;
import org.eclipse.wazaabi.mm.edp.handlers.Binding;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class EDPEventHandlerAdapterFactory implements
		EventHandlerAdapterFactory {

	public boolean isFactoryFor(Object context, Object source) {
		if (source instanceof EventHandler
				&& ((EventHandler) source).eClass().getEPackage() == EDPHandlersPackage.eINSTANCE)
			return true;
		return false;
	}

	public String getFactoryID() {
		return getClass().getName();
	}

//	public EventHandlerAdapter createEventHandlerAdapter(Object context,
//			EventHandler eventHandler) {
//		if (eventHandler != null
//				&& eventHandler.eClass().getEPackage() == EDPHandlersPackage.eINSTANCE) {
//			if (eventHandler instanceof Binding)
//				return new BindingAdapter();
//
//			else if (eventHandler instanceof EventHandler) {
//				return new EventHandlerAdapter();
//			}
//			// else if (eventHandler instanceof Sequence) {
//			// return (EventHandlerAdapter) new SequenceAdapter();
//			// }
//			// if (eventHandler instanceof Action)
//			// return new ActionAdapter();
//			// if (eventHandler instanceof Sequence)
//			// return new SequenceAdapter();
//		}
//		return null;
//	}

	@Override
	public Adapter createAdapter(Object callingContext, EObject model, Object creationHint) {
		if (model != null
				&& model.eClass().getEPackage() == EDPHandlersPackage.eINSTANCE) {
			if (model instanceof Binding)
				return new BindingAdapter();

			else if (model instanceof EventHandler) {
				return new EventHandlerAdapter();
			}
		}
		return null;
	}

}
