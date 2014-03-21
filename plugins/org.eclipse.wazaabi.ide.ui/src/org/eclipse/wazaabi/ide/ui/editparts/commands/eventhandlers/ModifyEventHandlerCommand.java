/*******************************************************************************
 * Copyright (c) 2014 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.ui.editparts.commands.eventhandlers;

import java.util.List;

import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.wazaabi.ide.ui.editparts.commands.CommandsUtils;
import org.eclipse.wazaabi.ide.ui.editparts.commands.TransactionalEditingDomainCommand;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class ModifyEventHandlerCommand extends TransactionalEditingDomainCommand {

	private EventHandler eventHandler = null;
	private Object newValue = null;
	private Object oldValue = null;
	private EStructuralFeature feature = null;

	private int index = -1;

	public ModifyEventHandlerCommand() {
		super("Modify EventHandler");
	}

	@Override
	public boolean canExecute() {
		return super.canExecute() && getEventHandler() != null
				&& getFeature() != null
				&& (getIndex() == -1 && !getFeature().isMany());
	}

	@Override
	public boolean canUndo() {
		return super.canUndo();
	}

	@Override
	protected void doExecute() {
		setOldValue(getEventHandler().eGet(feature));
		doRedo();
	}

	@Override
	protected void doRedo() {
		setValue(getNewValue());
	}

	@Override
	protected void doUndo() {
		setValue(getOldValue());
	}

	public int getIndex() {
		return index;
	}

	public Object getNewValue() {
		return newValue;
	}

	public EventHandler getEventHandler() {
		return eventHandler;
	}

	public void setIndex(int index) {
		this.index = index;
	}

	public void setNewValue(Object newValue) {
		this.newValue = newValue;
	}

	public void setEventHandler(EventHandler eventHandler) {
		this.eventHandler = eventHandler;
		setTransactionalEditingDomain(CommandsUtils.getEditingDomain(eventHandler));
	}

	public EStructuralFeature getFeature() {
		return feature;
	}

	public void setFeature(EStructuralFeature feature) {
		this.feature = feature;
	}

	protected Object getOldValue() {
		return oldValue;
	}

	protected void setOldValue(Object oldValue) {
		this.oldValue = oldValue;
	}

	@SuppressWarnings("unchecked")
	protected void setValue(Object value) {
		if (getIndex() == -1)
			getEventHandler().eSet(feature, value);
		else {
			if (getOldValue() instanceof List<?>
					&& ((List<?>) getOldValue()).size() > getIndex())
				((List<Object>) getOldValue()).set(getIndex(), value);
		}
	}
}
