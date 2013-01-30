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

import java.util.List;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.util.EContentAdapter;
import org.eclipse.wazaabi.engine.edp.exceptions.OperationAborted;
import org.eclipse.wazaabi.engine.edp.locationpaths.IPointersEvaluator;
import org.eclipse.wazaabi.mm.edp.events.ContentChangedEvent;
import org.eclipse.wazaabi.mm.edp.events.Event;

public class ContentChangedEventAdapter extends AbstractPathEventAdapter {
	protected class ContentAdapter extends EContentAdapter {

		private final Notifier initialTarget;

		protected Notifier getInitialTarget() {
			return initialTarget;
		}

		protected ContentAdapter(Notifier initialTarget) {
			this.initialTarget = initialTarget;
		}

		@Override
		public void notifyChanged(Notification msg) {
			if (msg.getEventType() == Notification.REMOVING_ADAPTER)
				return;
			try {
				getEventHandlerAdapter().trigger(
						(Event) ContentChangedEventAdapter.this.getTarget());
			} catch (OperationAborted e) {
				e.printStackTrace();
				System.err.println(e.getErrorMessage());
			} catch (RuntimeException ex) {
				ex.printStackTrace();
			}
		}

		@Override
		public boolean equals(Object other) {
			if (other instanceof ContentAdapter)
				return initialTarget.equals(((ContentAdapter) other)
						.getInitialTarget());
			return super.equals(other);
		}

	};

	public boolean isAdapterForType(Object type) {
		return ContentChangedEvent.class.equals(type);
	}

	protected Adapter createAdapter(EObject target) {
		if (target == null)
			return null;
		ContentAdapter adapter = new ContentAdapter(target);
		for (Adapter existingAdapter : getAdapters())
			if (existingAdapter instanceof ContentAdapter
					&& ((ContentAdapter) existingAdapter).equals(adapter))
				return existingAdapter;
		((EObject) target).eAdapters().add(adapter);
		getAdapters().add(adapter);
		return adapter;
	}

	protected void adaptPointer(Object pointer,
			final IPointersEvaluator pointersEvalutator) {
		Object target = pointersEvalutator.getValue(pointer);
		if (target instanceof EObject)
			createAdapter((EObject) target);
		else if (target instanceof List<?>) {
			for (Object item : (List<?>) target)
				if (item instanceof EObject)
					createAdapter((EObject) item);
		}
	}

	protected void unsetTarget(Adapter adapter) {
		((ContentAdapter) adapter).unsetTarget(((ContentAdapter) adapter)
				.getInitialTarget());
		((ContentAdapter) adapter).getInitialTarget().eAdapters()
				.remove(adapter);
	}

}
