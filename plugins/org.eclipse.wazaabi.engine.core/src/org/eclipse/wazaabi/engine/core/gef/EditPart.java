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

import java.util.List;

import org.eclipse.wazaabi.engine.core.editparts.factories.EditPartFactory;

public interface EditPart {

	void activate();

	/**
	 * Adds a listener to the EditPart. Duplicate calls result in duplicate
	 * notification.
	 * 
	 * @param listener
	 *            the Listener
	 */
	void addEditPartListener(EditPartListener listener);

	/**
	 * Called <em>after</em> the EditPart has been added to its parent. This is
	 * used to indicate to the EditPart that it should refresh itself for the
	 * first time.
	 */
	void addNotify();

	/**
	 * Deactivates the EditPart. EditParts that observe a dynamic model or
	 * support editing must be <i>active</i>. <code>deactivate()</code> is
	 * guaranteed to be called when an EditPart will no longer be used. Called
	 * by the managing EditPart, or the Viewer in the case of the
	 * {@link RootEditPart}. This method may be called multiple times.
	 * <P>
	 * During deactivation the receiver should:
	 * <UL>
	 * <LI>remove all listeners that were added in {@link #activate}
	 * <LI>deactivate all of its EditPolicies. EditPolicies may be contributing
	 * additional visuals, such as selection handles or feedback during
	 * interactions. Therefore it is necessary to tell the EditPolicies when to
	 * start doing this, and when to stop.
	 * <LI>call deactivate() on the EditParts it manages. This includes its
	 * children, and for <code>GraphicalEditParts</code>, its <i>source
	 * connections</i>.
	 * </UL>
	 */
	void deactivate();

	/**
	 * Returns the List of children <code>EditParts</code>. This method should
	 * rarely be called, and is only made public so that helper objects of this
	 * EditPart, such as EditPolicies, can obtain the children. The returned
	 * List may be by reference, and should never be modified.
	 * 
	 * @return a <code>List</code> of children
	 */
	List<EditPart> getChildren();

	/**
	 * Returns the primary model object that this EditPart represents. EditParts
	 * may correspond to more than one model object, or even no model object. In
	 * practice, the Object returned is used by other EditParts to identify this
	 * EditPart. In addition, EditPolicies probably rely on this method to build
	 * Commands that operate on the model.
	 * 
	 * @return the primary model object
	 */
	Object getModel();

	/**
	 * Returns the parent <code>EditPart</code>. This method should only be
	 * called internally or by helpers such as EditPolicies.
	 * 
	 * @return the parent
	 */
	EditPart getParent();

	/**
	 * Returns the {@link RootEditPart}. This method should only be called
	 * internally or by helpers such as edit policies. The Root can be used to
	 * get the Viewer.
	 * 
	 * @return the <code>RootEditPart</code>
	 */
	RootEditPart getRoot();

	/**
	 * Convenience method for returning the <code>EditPartViewer</code> for this
	 * part.
	 * 
	 * @return the EditPartViewer
	 */
	EditPartViewer getViewer();

	/**
	 * returns <code>true</code> if the EditPart is active. Editparts are active
	 * after {@link #activate()} is called, and until {@link #deactivate()} is
	 * called.
	 * 
	 * @return <code>true</code> when active
	 */
	boolean isActive();

	/**
	 * Called to force a refresh of this EditPart. All visuals properties will
	 * be updated, as well as structural features like children.
	 */
	void refresh();

	/**
	 * Removes the first occurance of the specified listener from the list of
	 * listeners. Does nothing if the listener was not present.
	 * 
	 * @param listener
	 *            the listener being removed
	 */
	void removeEditPartListener(EditPartListener listener);

	/**
	 * Called when the EditPart is being permanently removed from its
	 * {@link EditPartViewer}. This indicates that the EditPart will no longer
	 * be in the Viewer, and therefore should remove itself from the Viewer.
	 * This method is <EM>not</EM> called when a Viewer is disposed. It is only
	 * called when the EditPart is removed from its parent. This method is the
	 * inverse of {@link #addNotify()}
	 */
	void removeNotify();

	/**
	 * Sets the model. This method is made public to facilitate the use of
	 * {@link EditPartFactory EditPartFactories} .
	 * 
	 * <P>
	 * IMPORTANT: This method should only be called once.
	 * 
	 * @param model
	 *            the Model
	 */
	void setModel(Object model);

	/**
	 * Sets the parent. This should only be called by the parent EditPart.
	 * 
	 * @param parent
	 *            the parent EditPart
	 */
	void setParent(EditPart parent);

}
