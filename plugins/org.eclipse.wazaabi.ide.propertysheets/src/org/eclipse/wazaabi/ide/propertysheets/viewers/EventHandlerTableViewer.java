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

package org.eclipse.wazaabi.ide.propertysheets.viewers;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EOperation;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.wazaabi.ide.propertysheets.descriptors.AbstractDescriptorFactory;
import org.eclipse.wazaabi.ide.propertysheets.descriptors.EventHandlerDescriptorFactory;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.Condition;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.wazaabi.mm.edp.handlers.Executable;
import org.eclipse.wazaabi.mm.edp.handlers.Parameter;

public class EventHandlerTableViewer extends AbstractTableViewer {

	public final EventHandler HANDLER_FOR_INSERTION = new EventHandler() {

		public void setUri(String value) {
		}

		public String getUri() {
			return null;
		}

		public void setAsync(boolean value) {
		}

		public boolean isAsync() {
			return false;
		}

		public void setId(String value) {
		}

		public String getId() {
			return null;
		}

		public EList<Executable> getExecutables() {
			return null;
		}

		public void eSetDeliver(boolean deliver) {

		}

		public void eNotify(Notification notification) {
		}

		public boolean eDeliver() {
			return false;
		}

		public EList<Adapter> eAdapters() {
			return null;
		}

		public void eUnset(EStructuralFeature feature) {

		}

		public void eSet(EStructuralFeature feature, Object newValue) {

		}

		public Resource eResource() {
			return null;
		}

		public boolean eIsSet(EStructuralFeature feature) {
			return false;
		}

		public boolean eIsProxy() {
			return false;
		}

		public Object eInvoke(EOperation operation, EList<?> arguments)
				throws InvocationTargetException {
			return null;
		}

		public Object eGet(EStructuralFeature feature, boolean resolve) {
			return null;
		}

		public Object eGet(EStructuralFeature feature) {
			return null;
		}

		public EList<EObject> eCrossReferences() {
			return null;
		}

		public EList<EObject> eContents() {
			return null;
		}

		public EReference eContainmentFeature() {
			return null;
		}

		public EStructuralFeature eContainingFeature() {
			return null;
		}

		public EObject eContainer() {
			return null;
		}

		public EClass eClass() {
			return null;
		}

		public TreeIterator<EObject> eAllContents() {
			return null;
		}

		public EList<Parameter> getParameters() {
			return null;
		}

		public EList<Event> getEvents() {
			return null;
		}

		public EList<Condition> getConditions() {
			return null;
		}
	};

	@Override
	public String getLabel() {
		return "EventHandlers";
	}

	@Override
	protected String getLabel(EObject row) {
		if (row instanceof EventHandler)
			return ((EventHandler) row).eClass().getName();
		return ""; //$NON-NLS-1$
	}

	@Override
	protected AbstractDescriptorFactory createAbstractDescriptorFactory() {
		return new EventHandlerDescriptorFactory();
	}

	@Override
	protected EObject getBlankRow() {
		return HANDLER_FOR_INSERTION;
	}

	@Override
	protected IContentProvider getContentProvider() {
		return new EventHandlerContentProvider((EventHandler) getBlankRow());
	}

}