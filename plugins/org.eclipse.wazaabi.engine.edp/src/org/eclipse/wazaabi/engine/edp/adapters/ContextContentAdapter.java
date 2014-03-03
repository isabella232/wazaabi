/*******************************************************************************
 * Copyright (c) 2013 Olivier Moises
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
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.wazaabi.mm.edp.EdpPackage;

public class ContextContentAdapter extends AdapterImpl {

	@Override
	public void notifyChanged(Notification notification) {
		switch (notification
				.getFeatureID(org.eclipse.wazaabi.mm.edp.ContextContent.class)) {
		case EdpPackage.CONTEXT_CONTENT__VALUE:
			switch (notification.getEventType()) {
			case Notification.SET:
				valueChanged(notification.getOldValue(),
						notification.getNewValue());
				break;
			}
			break;
		case EdpPackage.CONTEXT_CONTENT__KEY:
			switch (notification.getEventType()) {
			case Notification.SET:
				keyChanged(notification.getOldValue(),
						notification.getNewValue());
				break;
			}
			break;
		}
	}

	protected void valueChanged(Object oldValue, Object newValue) {
		// TODO : to be continued

		// if (oldValue == newValue)
		// return;
		// List<Adapter> existingPathEventAdapters = new ArrayList<Adapter>();
		// if (oldValue instanceof Notifier) {
		// for (Adapter adapter : ((Notifier) oldValue).eAdapters())
		// if (adapter instanceof PropertyChangedEventAdapter.FeatureAdapter
		// || adapter instanceof ContentChangedEventAdapter.ContentAdapter)
		// existingPathEventAdapters.add(adapter);
		// for (Adapter adapter : existingPathEventAdapters)
		// ((Notifier) oldValue).eAdapters().remove(adapter);
		// }
		// if (newValue instanceof Notifier) {
		// for (Adapter adapter : existingPathEventAdapters)
		// ((Notifier) newValue).eAdapters().add(adapter);
		// for (Adapter adapter : ((Notifier) newValue).eAdapters())
		// if (adapter instanceof PropertyChangedEventAdapter.FeatureAdapter
		// && ((PropertyChangedEventAdapter.FeatureAdapter) adapter)
		// .getPropertyChangedEventAdapter() instanceof
		// PropertyChangedEventAdapter
		// && ((PropertyChangedEventAdapter.FeatureAdapter) adapter)
		// .getPropertyChangedEventAdapter()
		// .getEventHandlerAdapter() != null)
		//
		// // TODO : where to put this ? what if newValue is not a
		// // instance of a notifier???
		//
		// // TODO : we should detect the change between oldValue and
		// // newValue in order to not trigger for nothing
		// ((PropertyChangedEventAdapter.FeatureAdapter) adapter)
		// .getPropertyChangedEventAdapter()
		// .getEventHandlerAdapter()
		// .trigger(
		// (PropertyChangedEvent) ((PropertyChangedEventAdapter.FeatureAdapter)
		// adapter)
		// .getPropertyChangedEventAdapter()
		// .getTarget());
		// }
	}

	protected void keyChanged(Object oldKey, Object newKey) {
		System.out.println("oldKey=" + oldKey + ", newKey=" + newKey);
	}

}
