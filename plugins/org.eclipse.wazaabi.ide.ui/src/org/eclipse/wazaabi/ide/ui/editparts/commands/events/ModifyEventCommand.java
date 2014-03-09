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

package org.eclipse.wazaabi.ide.ui.editparts.commands.events;

import java.util.List;

import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.wazaabi.ide.ui.editparts.commands.CommandsUtils;
import org.eclipse.wazaabi.ide.ui.editparts.commands.TransactionalEditingDomainCommand;
import org.eclipse.wazaabi.mm.edp.events.Event;

public class ModifyEventCommand extends TransactionalEditingDomainCommand {

	private Event event = null;
	private Object newValue = null;
	private Object oldValue = null;
	private EStructuralFeature feature = null;

	private int index = -1;

	public ModifyEventCommand() {
		super("Modify Event");
	}

	@Override
	public boolean canExecute() {
		return super.canExecute() && getEvent() != null && getFeature() != null
				&& (getIndex() == -1 && !getFeature().isMany());
	}

	@Override
	public boolean canUndo() {
		return super.canUndo();
	}

	@Override
	protected void doExecute() {
		setOldValue(getEvent().eGet(feature));
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

	public Event getEvent() {
		return event;
	}

	public void setIndex(int index) {
		this.index = index;
	}

	public void setNewValue(Object newValue) {
		this.newValue = newValue;
	}

	public void setEvent(Event event) {
		this.event = event;
		setTransactionalEditingDomain(CommandsUtils.getEditingDomain(event));
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
			getEvent().eSet(feature, value);
		else {
			if (getOldValue() instanceof List<?>
					&& ((List<?>) getOldValue()).size() > getIndex())
				((List<Object>) getOldValue()).set(getIndex(), value);
		}
	}
}
