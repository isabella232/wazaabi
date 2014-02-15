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

package org.eclipse.wazaabi.engine.fx.events;

import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.event.ActionEvent;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.TextField;

import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.wazaabi.engine.core.editparts.AbstractComponentEditPart;
import org.eclipse.wazaabi.engine.core.editparts.WidgetEditPart;
import org.eclipse.wazaabi.engine.edp.adapters.EventAdapter;
import org.eclipse.wazaabi.engine.edp.adapters.EventHandlerAdapter;
import org.eclipse.wazaabi.engine.edp.exceptions.OperationAborted;
import org.eclipse.wazaabi.engine.fx.views.FXWidgetView;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class FXEventAdapter extends EventAdapter {

    private static final Logger log = LoggerFactory.getLogger(FXEventAdapter.class);

    private class ButtonSelectionHandler implements javafx.event.EventHandler<ActionEvent> {
        public void handle(ActionEvent e) {
            log.debug("ButtonSelectionHandler.handle");
            EventHandlerAdapter eha = getEventHandlerAdapter();
            if (eha != null && eha.getEventDispatcherAdapter() instanceof AbstractComponentEditPart) {
                try {
                    eha.trigger(getAugmentedEvent(e, (Event) getTarget()));
                } catch (OperationAborted ex) { 
                    log.warn("button selection handler failed", ex);
                }
            }
        }
    }

    private class FocusOutHandler implements ChangeListener<Boolean> {
        public void changed(ObservableValue<? extends Boolean> prop, Boolean oldVal, Boolean newVal) {
            if (!newVal) {
                log.debug("FocusOutHandler.handle");
                EventHandlerAdapter eha = getEventHandlerAdapter();
                if (eha != null && eha.getEventDispatcherAdapter() instanceof AbstractComponentEditPart) {
                    try {
                        eha.trigger((Event) getTarget());
                    } catch (OperationAborted ex) { 
                        log.warn("focus out handler failed", ex);
                    }
                }
            }
        }
    }
    
    private ButtonSelectionHandler buttonSelectionHandler = new ButtonSelectionHandler();
    private FocusOutHandler focusOutHandler = new FocusOutHandler();

    protected Event getAugmentedEvent(javafx.event.ActionEvent fxEvent, Event event) {
//        event.set(CoreUtils.CHARACTER_KEY, swtEvent.character);
//        event.set(CoreUtils.ALT_KEY, (swtEvent.stateMask & SWT.ALT) != 0);
//        event.set(CoreUtils.CTRL_KEY, (swtEvent.stateMask & SWT.CTRL) != 0);
//        event.set(CoreUtils.SHIFT_KEY, (swtEvent.stateMask & SWT.SHIFT) != 0);
        return event;
    }

    protected Node getFXNode() {
        if (getEventHandlerAdapter() != null
                && getEventHandlerAdapter().getEventDispatcherAdapter() instanceof WidgetEditPart
                && ((WidgetEditPart) getEventHandlerAdapter()
                        .getEventDispatcherAdapter()).getWidgetView() instanceof FXWidgetView)
            return ((FXWidgetView) ((WidgetEditPart) getEventHandlerAdapter()
                    .getEventDispatcherAdapter()).getWidgetView())
                    .getFXNode();
        return null;
    }

    protected void hookFXWidget(Event event) {
        Node node = getFXNode();
        switch (event.getId()) {
        case "core:ui:selection":
            if (node instanceof Button)
                ((Button) node).setOnAction(buttonSelectionHandler);
            break;
        case "core:ui:focus:out":
            if (node instanceof TextField)
                ((TextField) node).focusedProperty().addListener(focusOutHandler);
            break;
        }
    }

    protected void unhookFXWidget(Event event) {
        Node node = getFXNode();
        switch (event.getId()) {
        case "core:ui:selection":
            if (node instanceof Button)
                ((Button) node).setOnAction(null);
            break;
        case "core:ui:focus:out":
            if (node instanceof TextField)
                ((TextField) node).focusedProperty().removeListener(focusOutHandler);
            break;
        }
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
        log.debug("set target to {}", newTarget);
        if (newTarget != null)
            hookFXWidget((Event) newTarget);
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
