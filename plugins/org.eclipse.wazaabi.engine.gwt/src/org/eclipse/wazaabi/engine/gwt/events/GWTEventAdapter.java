/***********************************************************************************************************************
 * Copyright (c) 2008 Olivier Moises, 2014 Pavel Erofeev
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises - initial API and implementation
 *   Pavel Erofeev - rendering engine for JavaFX
***********************************************************************************************************************/

package org.eclipse.wazaabi.engine.gwt.events;

import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.wazaabi.engine.core.editparts.AbstractComponentEditPart;
import org.eclipse.wazaabi.engine.core.editparts.WidgetEditPart;
import org.eclipse.wazaabi.engine.edp.adapters.EventAdapter;
import org.eclipse.wazaabi.engine.edp.adapters.EventHandlerAdapter;
import org.eclipse.wazaabi.engine.edp.exceptions.OperationAborted;
import org.eclipse.wazaabi.engine.gwt.views.GWTWidgetView;
import org.eclipse.wazaabi.mm.edp.events.Event;

import com.google.gwt.event.dom.client.BlurEvent;
import com.google.gwt.event.dom.client.BlurHandler;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.dom.client.FocusEvent;
import com.google.gwt.event.dom.client.FocusHandler;
import com.google.gwt.event.shared.GwtEvent;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.TextBox;
import com.google.gwt.user.client.ui.Widget;


public class GWTEventAdapter extends EventAdapter {

    private class ButtonSelectionHandler implements ClickHandler {
        public void onClick(ClickEvent e) {
            System.out.println("ButtonSelectionHandler.handle");
            EventHandlerAdapter eha = getEventHandlerAdapter();
            if (eha != null && eha.getEventDispatcherAdapter() instanceof AbstractComponentEditPart) {
                try {
                    eha.trigger(getAugmentedEvent(e, (Event) getTarget()));
                } catch (OperationAborted ex) {
                    System.err.println("button selection handler failed");
                    ex.printStackTrace();
                }
            }
        }
    }

    private class FocusOutHandler implements BlurHandler {
        public void onBlur(BlurEvent event) {
            EventHandlerAdapter eha = getEventHandlerAdapter();
            if (eha != null && eha.getEventDispatcherAdapter() instanceof AbstractComponentEditPart) {
                try {
                    eha.trigger((Event) getTarget());
                } catch (OperationAborted ex) { 
                    ex.printStackTrace();
                }
            }
        }
    }

    private ButtonSelectionHandler buttonSelectionHandler = new ButtonSelectionHandler();
    private FocusOutHandler focusOutHandler = new FocusOutHandler();

    protected Event getAugmentedEvent(GwtEvent<?> gwtEvent, Event event) {
//        event.set(CoreUtils.CHARACTER_KEY, swtEvent.character);
//        event.set(CoreUtils.ALT_KEY, (swtEvent.stateMask & SWT.ALT) != 0);
//        event.set(CoreUtils.CTRL_KEY, (swtEvent.stateMask & SWT.CTRL) != 0);
//        event.set(CoreUtils.SHIFT_KEY, (swtEvent.stateMask & SWT.SHIFT) != 0);
        return event;
    }

    protected Widget getGWTWidget() {
        if (getEventHandlerAdapter() != null
                && getEventHandlerAdapter().getEventDispatcherAdapter() instanceof WidgetEditPart
                && ((WidgetEditPart) getEventHandlerAdapter()
                        .getEventDispatcherAdapter()).getWidgetView() instanceof GWTWidgetView)
            return ((GWTWidgetView) ((WidgetEditPart) getEventHandlerAdapter()
                    .getEventDispatcherAdapter()).getWidgetView())
                    .getGWTWidget();
        return null;
    }

    protected void hookGWTWidget(Event event) {
        Widget widget = getGWTWidget();
        switch (event.getId()) {
        case "core:ui:selection":
            if (widget instanceof Button)
                ((Button) widget).addClickHandler(buttonSelectionHandler);
            break;
        case "core:ui:focus:out":
            if (widget instanceof TextBox)
                ((TextBox) widget).addBlurHandler(focusOutHandler);
            break;
        }
    }

    protected void unhookFXWidget(Event event) {
        // TODO
    }

    @Override
    public void setTarget(Notifier newTarget) {
        if (newTarget == null && getTarget() == null)
            return;
        if (newTarget != null && newTarget.equals(getTarget()))
            return;
        if (getTarget() != null)
            unhookFXWidget((Event) getTarget());
        super.setTarget(newTarget);
        //log.debug("set target to {}", newTarget);
        if (newTarget != null)
            hookGWTWidget((Event) newTarget);
    }

//    protected void updateEventId(String oldId, String newId) {
//        unhookSWTWidget((Event) getTarget());
//        hookSWTWidget((Event) getTarget());
//    }
//
//    protected int getSWTEvent(Event event) {
//        return FXEventUtils.getSWTEvent(event);
//    }
//
}
