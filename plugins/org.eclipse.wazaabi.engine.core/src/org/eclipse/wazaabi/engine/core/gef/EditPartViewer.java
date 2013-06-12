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
package org.eclipse.wazaabi.engine.core.gef;

import java.beans.PropertyChangeListener;
import java.util.Map;

import org.eclipse.wazaabi.engine.core.editparts.factories.EditPartFactory;
import org.eclipse.wazaabi.engine.core.views.factories.WidgetViewFactory;
import org.eclipse.wazaabi.engine.edp.EDPFactory111;
import org.eclipse.wazaabi.engine.edp.locationpaths.IPointersEvaluator;

public interface EditPartViewer extends EDPFactory111 {

	// used to redirect all actions to a unique Action instance (the value)
	public static final String UNIVERSAL_ACTIONS_REDIRECTOR = "UNIVERSAL_ACTIONS_REDIRECTOR"; //$NON-NLS-1$  

	// used to redirect all focus handling to a unique one
	public static final String UNIVERSAL_FOCUS_HANDLER = "UNIVERSAL_FOCUS_HANDLER"; //$NON-NLS-1$

	/**
	 * Adds a listener to be notified of viewer property changes.
	 * 
	 * @param listener
	 *            the listener
	 */
	void addPropertyChangeListener(PropertyChangeListener listener);

	/**
	 * Returns the <i>contents</i> of this Viewer. The contents is the EditPart
	 * associated with the top-level model object. It is considered to be
	 * "The Diagram". If the user has nothing selected, the <i>contents</i> is
	 * implicitly the selected object.
	 * <P>
	 * The <i>Root</i> of the Viewer is different. By constrast, the root is
	 * never selected or targeted, and does not correspond to something in the
	 * model.
	 * 
	 * @see #getRootEditPart()
	 * @return the <i>contents</i> <code>EditPart</code>
	 */
	EditPart getContents();

	Object getControl();

	/**
	 * Returns the <code>EditPartFactory</code> for this viewer. The
	 * EditPartFactory is used to create the <i>contents</i> EditPart when
	 * {@link #setContents(Object)} is called. It is made available so that
	 * other EditParts can use it to create their children or connection
	 * editparts.
	 * 
	 * @return EditPartFactory
	 */
	EditPartFactory getEditPartFactory();

	/**
	 * Returns the {@link Map} for registering <code>EditParts</code> by
	 * <i>Keys</i>. EditParts may register themselves using any method, and may
	 * register themselves with multiple keys. The purpose of such registration
	 * is to allow an EditPart to be found by other EditParts, or by listeners
	 * of domain notifiers. By default, EditParts are registered by their model.
	 * <P>
	 * Some models use a "domain" notification system, in which all changes are
	 * dispatched to a single listener. Such a listener might use this map to
	 * lookup editparts for a given model, and then ask the editpart to update.
	 * 
	 * @return the registry map
	 */
	Map<Object, EditPart> getEditPartRegistry();

	/**
	 * Returns the value of the given property. Returns <code>null</code> if the
	 * property has not been set, or has been set to null.
	 * 
	 * @param key
	 *            the property's key
	 * @return the given properties value or <code>null</code>.
	 */
	Object getProperty(String key);

	/**
	 * Returns the <code>RootEditPart</code>. The RootEditPart is a special
	 * EditPart that serves as the parent to the contents editpart. The
	 * <i>root</i> is never selected. The root does not correspond to anything
	 * in the model. The User does not interact with the root.
	 * <P>
	 * The RootEditPart has a single child: the {@link #getContents() contents}.
	 * <P>
	 * By defining the concept of "root", GEF allows the application's "real"
	 * EditParts to be more homogeneous. For example, all non-root EditParts
	 * have a parent. Also, it allows applications to change the type of root
	 * being used without affecting their own editpart implementation hierarchy.
	 * 
	 * @see #getContents()
	 * @see #setRootEditPart(RootEditPart)
	 * @return the RootEditPart
	 */
	RootEditPart getRootEditPart();

	/**
	 * Returns the {@link Map} for associating <i>visual parts</i> with their
	 * <code>EditParts</code>. This map is used for hit-testing. Hit testing is
	 * performed by first determining which visual part is hit, and then mapping
	 * that part to an <code>EditPart</code>. What constitutes a <i>visual
	 * part</i> is viewer-specific. Examples include <code>Figures</code> and
	 * <code>TreeItems</code>.
	 * 
	 * @return the visual part map
	 */
	Map getVisualPartMap();

	/**
	 * removes the first instance of the specified property listener.
	 * 
	 * @param listener
	 *            the listener to remove
	 */
	void removePropertyChangeListener(PropertyChangeListener listener);

	/**
	 * Sets the contents for this Viewer. The contents can also be set using
	 * {@link #setContents(Object)}.
	 * 
	 * @param editpart
	 *            the contents
	 * @see #getRootEditPart()
	 */
	void setContents(EditPart editpart);

	/**
	 * Creates an <code>EditPart</code> for the provided model object using the
	 * <code>EditPartFactory</code>. That EditPart is then added to the
	 * {@link #getRootEditPart() RootEditPart}, and becomes the viewer's
	 * contents editpart.
	 * 
	 * @param contents
	 *            the contents model object
	 */
	void setContents(Object contents);

	/**
	 * Sets the <code>Control</code> for this viewer. The viewer's control is
	 * also set automatically if {@link #createControl(Composite)} is called.
	 * 
	 * @param control
	 *            the Control
	 */
	// void setControl(Object control);

	/**
	 * Sets the EditPartFactory.
	 * 
	 * @param factory
	 *            the factory
	 * @see #getEditPartFactory()
	 */
	void setEditPartFactory(EditPartFactory factory);

	/**
	 * Sets a property on this viewer. A viewer property is an arbitrary
	 * key-value pair that can be observed via
	 * {@link #addPropertyChangeListener(PropertyChangeListener)}. A
	 * <code>null</code> value will remove the property from the viewer.
	 * 
	 * @param propertyName
	 *            a unique string identifying the property
	 * @param value
	 *            the properties new value or <code>null</code> to remove
	 * @since 3.0
	 */
	void setProperty(String propertyName, Object value);

	/**
	 * Sets the <i>root</i> of this viewer. The root should not be confused with
	 * the <i>contents</i>.
	 * 
	 * @param root
	 *            the RootEditPart
	 * @see #getRootEditPart()
	 * @see #getContents()
	 */
	void setRootEditPart(RootEditPart root);

	/**
	 * Returns the WidgetViewFactory associated to this Viewer.
	 * WidgetViewFactories are, like viewers, platform specific.
	 * 
	 * @return
	 */
	public WidgetViewFactory getWidgetViewFactory();

	public String getCodeLocatorBaseUri();

	public void setCodeLocatorBaseUri(String baseUri);

	/**
	 * Convenient method for returning the IPointersEvaluator
	 * 
	 * @return
	 */
	public IPointersEvaluator getPointersEvaluator();
}
