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

package org.eclipse.wazaabi.ide.ui.editparts;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.swt.widgets.TreeItem;

public abstract class AbstractTreeEditPart extends
        org.eclipse.gef.editparts.AbstractTreeEditPart implements Adapter {

    private Notifier target;

    public void activate() {
        super.activate();
        hookModel();
    }

    public void deactivate() {
        unhookModel();
        super.deactivate();
    }

    public Notifier getTarget() {
        return target;
    }

    protected void hookModel() {
        ((EObject) getModel()).eAdapters().add(this);
    }

    public boolean isAdapterForType(Object type) {
        return false;
    }

    public void setTarget(Notifier newTarget) {
        this.target = newTarget;
    }

    protected void unhookModel() {
        ((EObject) getModel()).eAdapters().remove(this);
    }

    public void notifyChanged(Notification notification) {
        refresh();
    }

    protected void setWidgetText(String text, int columnIndex) {
        if (checkTreeItem())
            ((TreeItem) getWidget()).setText(columnIndex, text);
    }
}