/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Initial Contributors:
 *     IBM Corporation - initial API and implementation
 * Modified by:
 *     Olivier Moises
 *******************************************************************************/
package org.eclipse.wazaabi.engine.core.viewers;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.wazaabi.engine.core.CoreSingletons;
import org.eclipse.wazaabi.engine.core.editparts.factories.EditPartFactory;
import org.eclipse.wazaabi.engine.core.gef.EditPart;
import org.eclipse.wazaabi.engine.core.gef.EditPartViewer;
import org.eclipse.wazaabi.engine.core.gef.RootEditPart;
import org.eclipse.wazaabi.engine.core.views.factories.WidgetViewFactory;
import org.eclipse.wazaabi.engine.edp.EDPSingletons;
import org.eclipse.wazaabi.engine.edp.coderesolution.ICodeLocator;
import org.eclipse.wazaabi.engine.edp.locationpaths.IPointersEvaluator;

/**
 * The base implementation for EditPartViewer.
 * 
 * @author hudsonr
 */
public abstract class AbstractEditPartViewer implements EditPartViewer {

	private EditPartFactory factory;
	private Map<Object, EditPart> mapIDToEditPart = new HashMap<Object, EditPart>();
	private Map mapVisualToEditPart = new HashMap();
	private Map<String, Object> properties;
	private Object control;
	private RootEditPart rootEditPart;
	private PropertyChangeSupport changeSupport;
	private WidgetViewFactory widgetViewFactory = null;

	/**
	 * Constructs the viewer and calls {@link #init()}.
	 */
	public AbstractEditPartViewer() {
		init();
	}

	/**
	 * @see EditPartViewer#addPropertyChangeListener(PropertyChangeListener)
	 */
	public void addPropertyChangeListener(PropertyChangeListener listener) {
		if (changeSupport == null)
			changeSupport = new PropertyChangeSupport(this);
		changeSupport.addPropertyChangeListener(listener);
	}

	/**
	 * @see EditPartViewer#getContents()
	 */
	public EditPart getContents() {
		return getRootEditPart().getContents();
	}

	/**
	 * @see EditPartViewer#getControl()
	 */
	public Object getControl() {
		return control;
	}

	/**
	 * @see EditPartViewer#getEditPartFactory()
	 */
	public EditPartFactory getEditPartFactory() {
		return factory;
	}

	/**
	 * @see EditPartViewer#getEditPartRegistry()
	 */
	public Map<Object, EditPart> getEditPartRegistry() {
		return mapIDToEditPart;
	}

	/**
	 * @see EditPartViewer#getProperty(String)
	 */
	public Object getProperty(String key) {
		if (properties != null)
			return properties.get(key);
		return null;
	}

	/**
	 * @see EditPartViewer#getRootEditPart()
	 */
	public RootEditPart getRootEditPart() {
		return rootEditPart;
	}

	/**
	 * @see EditPartViewer#getVisualPartMap()
	 */
	public Map getVisualPartMap() {
		return mapVisualToEditPart;
	}

	/**
	 * Called once the control has been set.
	 * 
	 * @see #unhookControl()
	 */
	protected void hookControl() {
		Object control = getControl();
		assert control != null;
		if (getRootEditPart() != null)
			getRootEditPart().activate();
	}

	/**
	 * Called from the constructor. Subclasses may extend this method.
	 */
	protected void init() {
		assert CoreSingletons.getComposedEditPartFactory() != null;
		setEditPartFactory(CoreSingletons.getComposedEditPartFactory());
	}

	/**
	 * @see EditPartViewer#removePropertyChangeListener(PropertyChangeListener)
	 */
	public void removePropertyChangeListener(PropertyChangeListener listener) {
		if (changeSupport != null) {
			changeSupport.removePropertyChangeListener(listener);
			if (changeSupport.getPropertyChangeListeners().length == 0)
				changeSupport = null;
		}
	}

	/**
	 * @see EditPartViewer#reveal(EditPart)
	 */
	public void reveal(EditPart part) {
	}

	/**
	 * @see EditPartViewer#setContents(EditPart)
	 */
	public void setContents(EditPart editpart) {
		getRootEditPart().setContents(editpart);
	}

	/**
	 * @see EditPartViewer#setContents(Object)
	 */
	public void setContents(Object contents) {
		if (getEditPartFactory() == null) {
			System.err
					.println("An EditPartFactory is required to call setContents(Object)");//$NON-NLS-1$
			assert false;
		}
		setContents(getEditPartFactory().createEditPart(null, contents));
	}

	/**
	 * @see EditPartViewer#setControl(Control)
	 */
	public void setControl(Object control) {
		if (this.control != null)
			unhookControl();
		this.control = control;
		if (control != null)
			hookControl();
	}

	/**
	 * @see EditPartViewer#setEditPartFactory(org.eclipse.wazaabi.engine.core.editparts.factories.EditPartFactory)
	 */
	public void setEditPartFactory(EditPartFactory factory) {
		this.factory = factory;
	}

	/**
	 * @see EditPartViewer#setProperty(String, Object)
	 */
	public void setProperty(String key, Object value) {
		if (properties == null)
			properties = new HashMap<String, Object>();
		Object old;
		if (value == null)
			old = properties.remove(key);
		else
			old = properties.put(key, value);

		if (changeSupport != null)
			changeSupport.firePropertyChange(key, old, value);
	}

	/**
	 * @see EditPartViewer#setRootEditPart(RootEditPart)
	 */
	public void setRootEditPart(RootEditPart editpart) {
		if (rootEditPart != null) {
			if (rootEditPart.isActive())
				rootEditPart.deactivate();
			rootEditPart.setViewer(null);
		}
		rootEditPart = editpart;
		rootEditPart.setViewer(this);
		if (getControl() != null)
			rootEditPart.activate();
	}

	/**
	 * Called when the control is being set to <code>null</code>, but before it
	 * is null.
	 */
	protected void unhookControl() {
		assert getControl() != null;
		if (getRootEditPart() != null)
			getRootEditPart().deactivate();
	}

	public WidgetViewFactory getWidgetViewFactory() {
		if (this.widgetViewFactory == null)
			this.widgetViewFactory = CoreSingletons
					.getComposedWidgetViewFactory();
		return this.widgetViewFactory;
	}

	private IPointersEvaluator pointersEvaluator = null;

	public IPointersEvaluator getPointersEvaluator() {
		if (pointersEvaluator == null)
			pointersEvaluator = EDPSingletons.getRegistry()
					.getDefaultPointersEvaluator();
		return pointersEvaluator;
	}

	public void setPointersEvaluator(IPointersEvaluator pointersEvaluator) {
		this.pointersEvaluator = pointersEvaluator;
	}

	public String getCodeLocatorBaseUri() {
		return null;
	}

	public void setCodeLocatorBaseUri(String baseUri) {
	}

}
