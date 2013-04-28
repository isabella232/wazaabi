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

package org.eclipse.wazaabi.locationpaths.emft;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.edit.command.SetCommand;
import org.eclipse.emf.edit.domain.AdapterFactoryEditingDomain;
import org.eclipse.emf.edit.domain.EditingDomain;
import org.eclipse.emf.transaction.RecordingCommand;
import org.eclipse.emf.transaction.TransactionalEditingDomain;

public class PointersEvaluatorImpl extends
		org.eclipse.wazaabi.locationpaths.PointersEvaluatorImpl {

	protected void setFeature(final EObject target,
			final EStructuralFeature feature, final Object value) {

		EditingDomain tmpEditingDomain = AdapterFactoryEditingDomain
				.getEditingDomainFor(target);

		if (tmpEditingDomain == null && target.eResource() != null
				&& target.eResource().getResourceSet() != null)
			tmpEditingDomain = TransactionalEditingDomain.Factory.INSTANCE
					.getEditingDomain(target.eResource().getResourceSet());

		final EditingDomain editingDomain = tmpEditingDomain;

		if (editingDomain instanceof TransactionalEditingDomain) {
			editingDomain.getCommandStack().execute(
					new RecordingCommand(
							(TransactionalEditingDomain) editingDomain) {
						protected void doExecute() {
							PointersEvaluatorImpl.super.setFeature(target,
									feature, value);
						}
					});
		} else if (editingDomain != null) {
			SetCommand setCommand = new SetCommand(editingDomain, target,
					feature, value);
			editingDomain.getCommandStack().execute(setCommand);
		} else
			super.setFeature(target, feature, value);
	}

}
