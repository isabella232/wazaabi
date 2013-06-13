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
package org.eclipse.wazaabi.engine.core.gef.editparts;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.wazaabi.engine.core.editparts.factories.EditPartFactory;
import org.eclipse.wazaabi.engine.core.gef.EditPart;
import org.eclipse.wazaabi.engine.core.gef.EditPartListener;
import org.eclipse.wazaabi.engine.core.gef.EditPartViewer;
import org.eclipse.wazaabi.engine.core.gef.RootEditPart;

/**
 * The baseline implementation for the {@link EditPart} interface.
 * <P>
 * Since this is the default implementation of an interface, this document deals
 * with proper sub-classing of this implementation. This class is not the API.
 * For documentation on proper usage of the public API, see the documentation
 * for the interface itself: {@link EditPart}.
 * <P>
 * This class assumes no visual representation. Subclasses
 * {@link AbstractGraphicalEditPart} and {@link AbstractTreeEditPart} add
 * support for {@link org.eclipse.draw2d.IFigure Figures} and
 * {@link org.eclipse.swt.widgets.TreeItem TreeItems} respectively.
 * <P>
 * AbstractEditPart provides support for children. All AbstractEditPart's can
 * potentially be containers for other EditParts.
 */
public abstract class AbstractEditPart implements EditPart {

	/**
	 * This flag is set during {@link #activate()}, and reset on
	 * {@link #deactivate()}
	 */
	protected static final int FLAG_ACTIVE = 1;

	/**
	 * The left-most bit that is reserved by this class for setting flags.
	 * Subclasses may define additional flags starting at
	 * <code>(MAX_FLAG << 1)</code>.
	 */
	protected static final int MAX_FLAG = FLAG_ACTIVE;

	private Object model;
	private int flags;
	private EditPart parent;

	/**
	 * The List of children EditParts
	 */
	protected List<EditPart> children;

	ListenerList eventListeners = new ListenerList();

	/**
	 * Activates this EditPart, which in turn activates its children and
	 * EditPolicies. Subclasses should <em>extend</em> this method to add
	 * listeners to the model. Activation indicates that the EditPart is
	 * realized in an EditPartViewer. <code>deactivate()</code> is the inverse,
	 * and is eventually called on all EditParts.
	 * 
	 * @see EditPart#activate()
	 * @see #deactivate()
	 */
	public void activate() {
		setFlag(FLAG_ACTIVE, true);
		List<EditPart> c = getChildren();
		for (int i = 0; i < c.size(); i++)
			c.get(i).activate();
		fireActivated();
	}

	/**
	 * Adds a child <code>EditPart</code> to this EditPart. This method is
	 * called from {@link #refreshChildren()}. The following events occur in the
	 * order listed:
	 * <OL>
	 * <LI>The child is added to the {@link #children} List, and its parent is
	 * set to <code>this</code>
	 * <LI>{@link #addChildVisual(EditPart, int)} is called to add the child's
	 * visual
	 * <LI>{@link EditPart#addNotify()} is called on the child.
	 * <LI><code>activate()</code> is called if this part is active
	 * <LI><code>EditPartListeners</code> are notified that the child has been
	 * added.
	 * </OL>
	 * <P>
	 * Subclasses should implement {@link #addChildVisual(EditPart, int)}.
	 * 
	 * @param child
	 *            The <code>EditPart</code> to add
	 * @param index
	 *            The index
	 * @see #addChildVisual(EditPart, int)
	 * @see #removeChild(EditPart)
	 * @see #reorderChild(EditPart,int)
	 */
	protected void addChild(EditPart child, int index) {
		assert child != null;
		if (index == -1)
			index = getChildren().size();
		if (children == null)
			children = new ArrayList<EditPart>(2);

		children.add(index, child);
		child.setParent(this);
		addChildVisual(child, index);
		child.addNotify();

		if (isActive())
			child.activate();
		fireChildAdded(child, index);
	}

	/**
	 * Performs the addition of the child's <i>visual</i> to this EditPart's
	 * Visual. The provided subclasses {@link AbstractGraphicalEditPart} and
	 * {@link AbstractTreeEditPart} already implement this method correctly, so
	 * it is unlikely that this method should be overridden.
	 * 
	 * @param child
	 *            The EditPart being added
	 * @param index
	 *            The child's position
	 * @see #addChild(EditPart, int)
	 * @see AbstractGraphicalEditPart#removeChildVisual(EditPart)
	 */
	protected abstract void addChildVisual(EditPart child, int index);

	/**
	 * Adds an EditPartListener.
	 * 
	 * @param listener
	 *            the listener
	 */
	public void addEditPartListener(EditPartListener listener) {
		eventListeners.add(listener);
	}

	/**
	 * @see EditPart#addNotify()
	 */
	public void addNotify() {
		register();
		List<EditPart> children = getChildren();
		for (int i = 0; i < children.size(); i++)
			((EditPart) children.get(i)).addNotify();
		refresh();
	}

	/**
	 * Create the child <code>EditPart</code> for the given model object. This
	 * method is called from {@link #refreshChildren()}.
	 * <P>
	 * By default, the implementation will delegate to the
	 * <code>EditPartViewer</code>'s {@link EditPartFactory}. Subclasses may
	 * override this method instead of using a Factory.
	 * 
	 * @param model
	 *            the Child model object
	 * @return The child EditPart
	 */
	protected EditPart createChild(Object model) {
		return (EditPart) getViewer().createComponent(this, model, null,
				EditPart.class);
	}

	/**
	 * Deactivates this EditPart, and in turn deactivates its children and
	 * EditPolicies. Subclasses should <em>extend</em> this method to remove any
	 * listeners established in {@link #activate()}
	 * 
	 * @see EditPart#deactivate()
	 * @see #activate()
	 */
	public void deactivate() {
		List<EditPart> c = getChildren();
		for (int i = 0; i < c.size(); i++)
			((EditPart) c.get(i)).deactivate();

		setFlag(FLAG_ACTIVE, false);
		fireDeactivated();
	}

	/**
	 * This method will log a message to GEF's trace/debug system if the
	 * corresponding flag for EditParts is set to true.
	 * 
	 * @param message
	 *            a debug message
	 * @deprecated in 3.1
	 */
	protected final void debug(String message) {
	}

	/**
	 * This method will log the message to GEF's trace/debug system if the
	 * corrseponding flag for FEEDBACK is set to true.
	 * 
	 * @param message
	 *            Message to be passed
	 * @deprecated in 3.1
	 */
	protected final void debugFeedback(String message) {
	}

	/**
	 * Notifies <code>EditPartListeners</code> that this EditPart has been
	 * activated.
	 */
	protected void fireActivated() {
		for (Object listener : eventListeners.getListeners())
			((EditPartListener) listener).partActivated(this);
	}

	/**
	 * Notifies <code>EditPartListeners</code> that a child has been added.
	 * 
	 * @param child
	 *            <code>EditPart</code> being added as child.
	 * @param index
	 *            Position child is being added into.
	 */
	protected void fireChildAdded(EditPart child, int index) {
		for (Object listener : eventListeners.getListeners())
			((EditPartListener) listener).childAdded(child, index);
	}

	/**
	 * Notifies <code>EditPartListeners </code> that this EditPart has been
	 * deactivated.
	 */
	protected void fireDeactivated() {
		for (Object listener : eventListeners.getListeners())
			((EditPartListener) listener).partDeactivated(this);
	}

	/**
	 * Notifies <code>EditPartListeners</code> that a child is being removed.
	 * 
	 * @param child
	 *            <code>EditPart</code> being removed.
	 * @param index
	 *            Position of the child in children list.
	 */
	protected void fireRemovingChild(EditPart child, int index) {
		for (Object listener : eventListeners.getListeners())
			((EditPartListener) listener).removingChild(child, index);
	}

	/**
	 * @see org.eclipse.wazaabi.engine.core.gef.EditPart#getChildren()
	 */
	@SuppressWarnings("unchecked")
	public List<EditPart> getChildren() {
		if (children == null)
			return (List<EditPart>) Collections.EMPTY_LIST;
		return children;
	}

	/**
	 * Returns the boolean value of the given flag. Specifically, returns
	 * <code>true</code> if the bitwise AND of the specified flag and the
	 * internal flags field is non-zero.
	 * 
	 * @param flag
	 *            Bitmask indicating which flag to return
	 * @return the requested flag's value
	 * @see #setFlag(int,boolean)
	 */
	protected final boolean getFlag(int flag) {
		return (flags & flag) != 0;
	}

	/**
	 * @see org.eclipse.wazaabi.engine.core.gef.EditPart#getModel()
	 */
	public Object getModel() {
		return model;
	}

	/**
	 * Returns a <code>List</code> containing the children model objects. If
	 * this EditPart's model is a container, this method should be overridden to
	 * returns its children. This is what causes children EditParts to be
	 * created.
	 * <P>
	 * Callers must not modify the returned List. Must not return
	 * <code>null</code>.
	 * 
	 * @return the List of children
	 */
	protected List<?> getModelChildren() {
		return Collections.EMPTY_LIST;
	}

	/**
	 * @see org.eclipse.wazaabi.engine.core.gef.EditPart#getParent()
	 */
	public EditPart getParent() {
		return parent;
	}

	/**
	 * @see org.eclipse.wazaabi.engine.core.gef.EditPart#getRoot()
	 */
	public RootEditPart getRoot() {
		return getParent().getRoot();
	}

	/**
	 * @see org.eclipse.wazaabi.engine.core.gef.EditPart#getViewer()
	 */
	public EditPartViewer getViewer() {
		return getRoot().getViewer();
	}

	/**
	 * @return <code>true</code> if this EditPart is active.
	 */
	public boolean isActive() {
		return getFlag(FLAG_ACTIVE);
	}

	/**
	 * Reserved for future use
	 * 
	 * @return boolean
	 */
	public boolean isSelectable() {
		return true;
	}

	/**
	 * Refreshes all properties visually displayed by this EditPart. The default
	 * implementation will call {@link #refreshChildren()} to update its
	 * structural features. It also calls {@link #refreshVisuals()} to update
	 * its own displayed properties. Subclasses should extend this method to
	 * handle additional types of structural refreshing.
	 */
	public void refresh() {
		refreshVisuals();
		refreshChildren();
	}

	/**
	 * Updates the set of children EditParts so that it is in sync with the
	 * model children. This method is called from {@link #refresh()}, and may
	 * also be called in response to notification from the model. This method
	 * requires linear time to complete. Clients should call this method as few
	 * times as possible. Consider also calling {@link #removeChild(EditPart)}
	 * and {@link #addChild(EditPart, int)} which run in constant time.
	 * <P>
	 * The update is performed by comparing the exising EditParts with the set
	 * of model children returned from {@link #getModelChildren()}. EditParts
	 * whose models no longer exist are {@link #removeChild(EditPart) removed}.
	 * New models have their EditParts {@link #createChild(Object) created}.
	 * <P>
	 * This method should <em>not</em> be overridden.
	 * 
	 * @see #getModelChildren()
	 */
	protected void refreshChildren() {
		int i;
		EditPart editPart;
		Object model;

		Map modelToEditPart = new HashMap();
		List<EditPart> children = getChildren();

		for (i = 0; i < children.size(); i++) {
			editPart = (EditPart) children.get(i);
			modelToEditPart.put(editPart.getModel(), editPart);
		}

		List<?> modelObjects = getModelChildren();

		for (i = 0; i < modelObjects.size(); i++) {
			model = modelObjects.get(i);

			// Do a quick check to see if editPart[i] == model[i]
			if (i < children.size()
					&& ((EditPart) children.get(i)).getModel() == model)
				continue;

			// Look to see if the EditPart is already around but in the wrong
			// location
			editPart = (EditPart) modelToEditPart.get(model);

			if (editPart != null)
				reorderChild(editPart, i);
			else {
				// An editpart for this model doesn't exist yet. Create and
				// insert one.
				editPart = createChild(model);
				addChild(editPart, i);
			}
		}
		List trash = new ArrayList();
		for (; i < children.size(); i++)
			trash.add(children.get(i));
		for (i = 0; i < trash.size(); i++) {
			EditPart ep = (EditPart) trash.get(i);
			removeChild(ep);
		}
	}

	/**
	 * Refreshes this EditPart's <i>visuals</i>. This method is called by
	 * {@link #refresh()}, and may also be called in response to notifications
	 * from the model. This method does nothing by default. Subclasses may
	 * override.
	 */
	protected void refreshVisuals() {
	}

	/**
	 * Registers itself in the viewer's various registries. If your EditPart has
	 * a 1-to-1 relationship with a visual object and a 1-to-1 relationship with
	 * a model object, the default implementation should be sufficent.
	 * 
	 * @see #unregister()
	 * @see EditPartViewer#getVisualPartMap()
	 * @see EditPartViewer#getEditPartRegistry()
	 */
	protected void register() {
		registerModel();
		registerVisuals();
	}

	/**
	 * Registers the <i>model</i> in the
	 * {@link EditPartViewer#getEditPartRegistry()}. Subclasses should only
	 * extend this method if they need to register this EditPart in additional
	 * ways.
	 */
	protected void registerModel() {
		getViewer().getEditPartRegistry().put(getModel(), this);
	}

	/**
	 * Registers the <i>visuals</i> in the
	 * {@link EditPartViewer#getVisualPartMap()}. Subclasses should override
	 * this method for the visual part they support.
	 * {@link AbstractGraphicalEditPart} and {@link AbstractTreeEditPart}
	 * already do this.
	 */
	protected void registerVisuals() {
	}

	/**
	 * Removes a child <code>EditPart</code>. This method is called from
	 * {@link #refreshChildren()}. The following events occur in the order
	 * listed:
	 * <OL>
	 * <LI><code>EditPartListeners</code> are notified that the child is being
	 * removed
	 * <LI><code>deactivate()</code> is called if the child is active
	 * <LI>{@link EditPart#removeNotify()} is called on the child.
	 * <LI>{@link #removeChildVisual(EditPart)} is called to remove the child's
	 * visual object.
	 * <LI>The child's parent is set to <code>null</code>
	 * </OL>
	 * <P>
	 * Subclasses should implement {@link #removeChildVisual(EditPart)}.
	 * 
	 * @param child
	 *            EditPart being removed
	 * @see #addChild(EditPart,int)
	 */
	protected void removeChild(EditPart child) {
		assert child != null;
		int index = getChildren().indexOf(child);
		if (index < 0)
			return;
		fireRemovingChild(child, index);
		if (isActive())
			child.deactivate();
		child.removeNotify();
		removeChildVisual(child);
		child.setParent(null);
		getChildren().remove(child);
	}

	/**
	 * Removes the childs visual from this EditPart's visual. Subclasses should
	 * implement this method to support the visual type they introduce, such as
	 * Figures or TreeItems.
	 * 
	 * @param child
	 *            the child EditPart
	 */
	protected abstract void removeChildVisual(EditPart child);

	/**
	 * No reason to override
	 * 
	 * @see EditPart#removeEditPartListener(EditPartListener)
	 */
	public void removeEditPartListener(EditPartListener listener) {
		eventListeners.remove(listener);
	}

	/**
	 * Removes all references from the <code>EditPartViewer</code> to this
	 * EditPart. This includes:
	 * <UL>
	 * <LI>deselecting this EditPart if selected
	 * <LI>setting the Viewer's focus to <code>null</code> if this EditPart has
	 * <i>focus</i>
	 * <LI>{@link #unregister()} this EditPart
	 * </UL>
	 * <P>
	 * In addition, <code>removeNotify()</code> is called recursively on all
	 * children EditParts. Subclasses should <em>extend</em> this method to
	 * perform any additional cleanup.
	 * 
	 * @see EditPart#removeNotify()
	 */
	public void removeNotify() {
		List<EditPart> children = getChildren();
		for (int i = 0; i < children.size(); i++)
			((EditPart) children.get(i)).removeNotify();
		unregister();
	}

	/**
	 * Moves a child <code>EditPart</code> into a lower index than it currently
	 * occupies. This method is called from {@link #refreshChildren()}.
	 * 
	 * @param editpart
	 *            the child being reordered
	 * @param index
	 *            new index for the child
	 */
	protected void reorderChild(EditPart editpart, int index) {
		removeChildVisual(editpart);
		List<EditPart> children = getChildren();
		children.remove(editpart);
		children.add(index, editpart);
		addChildVisual(editpart, index);
	}

	/**
	 * Sets the value of the specified flag. Flag values are decalared as static
	 * constants. Subclasses may define additional constants above
	 * {@link #MAX_FLAG}.
	 * 
	 * @param flag
	 *            Flag being set
	 * @param value
	 *            Value of the flag to be set
	 * @see #getFlag(int)
	 */
	protected final void setFlag(int flag, boolean value) {
		if (value)
			flags |= flag;
		else
			flags &= ~flag;
	}

	/**
	 * Set the primary model object that this EditPart represents. This method
	 * is used by an <code>EditPartFactory</code> when creating an EditPart.
	 * 
	 * @see EditPart#setModel(Object)
	 */
	public void setModel(Object model) {
		if (getModel() == model)
			return;
		this.model = model;
	}

	/**
	 * Sets the parent EditPart. There is no reason to override this method.
	 * 
	 * @see EditPart#setParent(EditPart)
	 */
	public void setParent(EditPart parent) {
		if (this.parent == parent)
			return;
		this.parent = parent;
	}

	/**
	 * Describes this EditPart for developmental debugging purposes.
	 * 
	 * @return a description
	 */
	public String toString() {
		String c = getClass().getName();
		c = c.substring(c.lastIndexOf('.') + 1);
		return c + "( " + getModel() + " )";//$NON-NLS-2$//$NON-NLS-1$
	}

	/**
	 * Undoes any registration performed by {@link #register()}. The provided
	 * base classes will correctly unregister their visuals.
	 */
	protected void unregister() {
		unregisterVisuals();
		unregisterModel();
	}

	/**
	 * Unregisters the <i>model</i> in the
	 * {@link EditPartViewer#getEditPartRegistry()}. Subclasses should only
	 * extend this method if they need to unregister this EditPart in additional
	 * ways.
	 */
	protected void unregisterModel() {
		Map registry = getViewer().getEditPartRegistry();
		if (registry.get(getModel()) == this)
			registry.remove(getModel());
	}

	/**
	 * Unregisters the <i>visuals</i> in the
	 * {@link EditPartViewer#getVisualPartMap()}. Subclasses should override
	 * this method for the visual part they support.
	 * {@link AbstractGraphicalEditPart} and {@link AbstractTreeEditPart}
	 * already do this.
	 */
	protected void unregisterVisuals() {
	}

}
