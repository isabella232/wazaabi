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

package org.eclipse.wazaabi.engine.swt.events;

import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.wazaabi.engine.core.CoreUtils;
import org.eclipse.wazaabi.engine.core.editparts.WidgetEditPart;
import org.eclipse.wazaabi.engine.edp.adapters.EventAdapter;
import org.eclipse.wazaabi.engine.edp.adapters.EventHandlerAdapter;
import org.eclipse.wazaabi.engine.edp.exceptions.OperationAborted;
import org.eclipse.wazaabi.engine.swt.views.SWTWidgetView;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.wazaabi.mm.edp.handlers.Operation;

public class SWTUIEventAdapter extends EventAdapter {

	private int currentEventType = SWT.None;

	private Listener listener = new Listener() {

		public void handleEvent(final org.eclipse.swt.widgets.Event event) {
			final EventHandlerAdapter eventHandlerAdapter = getEventHandlerAdapter();
			if (eventHandlerAdapter.getTarget() instanceof Operation
			// TODO: try to evaluate the cost of these tests
			// may be should we attach a specific listener and track changes
					&& !((Operation) eventHandlerAdapter.getTarget()).isAsync())
				event.display.syncExec(new Runnable() {
					public void run() {
						if (event.widget != null && !event.widget.isDisposed()) {
							EventDispatcher eventDispatcher = (EventDispatcher) ((EventHandler) eventHandlerAdapter
									.getTarget()).eContainer();
							try {
								eventHandlerAdapter.trigger(getAugmentedEvent(
										event, (Event) getTarget()));
								if (eventDispatcher instanceof AbstractComponent)
									((AbstractComponent) eventDispatcher)
											.setErrorText(null);
							} catch (OperationAborted e) {
								if (eventDispatcher instanceof AbstractComponent) {
									if (e.getErrorMessage() == null)
										((AbstractComponent) eventDispatcher)
												.setErrorText("\0");
									else
										((AbstractComponent) eventDispatcher)
												.setErrorText(e
														.getErrorMessage());
								}
							}
						}
					}
				});
			else
				event.display.asyncExec(new Runnable() {
					public void run() {
						if (event.widget != null && !event.widget.isDisposed()) {
							EventDispatcher eventDispatcher = (EventDispatcher) ((EventHandler) eventHandlerAdapter
									.getTarget()).eContainer();
							try {
								eventHandlerAdapter.trigger(getAugmentedEvent(
										event, (Event) getTarget()));
								if (eventDispatcher instanceof AbstractComponent)
									((AbstractComponent) eventDispatcher)
											.setErrorText(null);
							} catch (OperationAborted e) {
								if (eventDispatcher instanceof AbstractComponent) {
									if (e.getErrorMessage() == null)
										((AbstractComponent) eventDispatcher)
												.setErrorText("\0");
									else
										((AbstractComponent) eventDispatcher)
												.setErrorText(e
														.getErrorMessage());
								}
							}
						}
					}
				});
		}
	};

	protected Event getAugmentedEvent(org.eclipse.swt.widgets.Event swtEvent,
			Event event) {
		event.set(CoreUtils.CHARACTER_KEY, swtEvent.character);
		event.set(CoreUtils.ALT_KEY, (swtEvent.stateMask & SWT.ALT) != 0);
		event.set(CoreUtils.CTRL_KEY, (swtEvent.stateMask & SWT.CTRL) != 0);
		event.set(CoreUtils.SHIFT_KEY, (swtEvent.stateMask & SWT.SHIFT) != 0);
		return event;
	}

	protected Widget getSWTWidget() {
		if (getEventHandlerAdapter() != null
				&& getEventHandlerAdapter().getEventDispatcherAdapter() instanceof WidgetEditPart
				&& ((WidgetEditPart) getEventHandlerAdapter()
						.getEventDispatcherAdapter()).getWidgetView() instanceof SWTWidgetView)
			return ((SWTWidgetView) ((WidgetEditPart) getEventHandlerAdapter()
					.getEventDispatcherAdapter()).getWidgetView())
					.getSWTWidget();
		return null;
	}

	protected void unhookSWTWidget(Event event) {
		if (currentEventType == SWT.NONE)
			return;
		final Widget currentWidget = getSWTWidget();
		if (currentWidget != null && !currentWidget.isDisposed()) {
			// System.out.println("unhook " + currentWidget + " " + event);
			currentWidget.removeListener(currentEventType, listener);
		}
		currentEventType = SWT.NONE;
	}

	protected void hookSWTWidget(Event event) {
		int newEventType = SWTEventUtils.getSWTEvent(event);
		if (newEventType == currentEventType)
			return;
		currentEventType = newEventType;
		if (currentEventType == SWT.NONE)
			return;
		final Widget currentWidget = getSWTWidget();
		if (currentWidget != null && !currentWidget.isDisposed()) {
			// System.out.println("hook " + currentWidget + " " + event);
			currentWidget.addListener(currentEventType, listener);
		}
	}

	@Override
	public void setTarget(Notifier newTarget) {
		if (newTarget == null && getTarget() == null)
			return;
		if (newTarget != null && newTarget.equals(getTarget()))
			return;
		if (getTarget() != null)
			unhookSWTWidget((Event) getTarget());
		super.setTarget(newTarget);
		if (newTarget != null)
			hookSWTWidget((Event) newTarget);
	}

	protected void updateEventId(String oldId, String newId) {
		unhookSWTWidget((Event) getTarget());
		hookSWTWidget((Event) getTarget());
	}
}
