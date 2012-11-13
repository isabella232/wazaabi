/*******************************************************************************
 * Copyright (c) 2012 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.ui.editors.viewer;

import java.util.Collection;
import java.util.Collections;

import org.eclipse.draw2d.geometry.Point;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.edit.ui.dnd.LocalTransfer;
import org.eclipse.gef.AutoexposeHelper;
import org.eclipse.gef.EditPart;
import org.eclipse.gef.EditPartViewer;
import org.eclipse.gef.TreeEditPart;
import org.eclipse.gef.commands.Command;
import org.eclipse.gef.commands.CompoundCommand;
import org.eclipse.jface.util.TransferDropTargetListener;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DropTarget;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.dnd.TransferData;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.core.widgets.Container;

public abstract class AbstractTransferDropTargetListener implements
		TransferDropTargetListener {

	private final EditPartViewer viewer;
	private AutoexposeHelper exposeHelper;
	private boolean hovering = false;
	private long hoverStartTime = -1;
	private Point prevMouseLoc;
	private boolean showingFeedback;
	private EditPart target;

	protected EditPartViewer getViewer() {
		return viewer;
	}

	public AbstractTransferDropTargetListener(EditPartViewer viewer) {
		this.viewer = viewer;
	}

	public void dragEnter(DropTargetEvent event) {
		resetHover();
	}

	public void dragLeave(DropTargetEvent event) {
		unload(event);
	}

	public void dragOperationChanged(DropTargetEvent event) {
		resetHover();
		// setCurrentEvent(event);
		handleDragOperationChanged(event);
	}

	/**
	 * Tests whether the given event's location is different than the previous
	 * event's location, and sets the remembered location to the current event's
	 * location.
	 * 
	 * @param event
	 * @return boolean
	 */
	private boolean testAndSet(DropTargetEvent event) {
		boolean result = prevMouseLoc == null
				|| !(prevMouseLoc.x == event.x && prevMouseLoc.y == event.y);
		if (prevMouseLoc == null)
			prevMouseLoc = new Point();
		prevMouseLoc.x = event.x;
		prevMouseLoc.y = event.y;
		return result;
	}

	public void dragOver(DropTargetEvent event) {
		handleDragOver(event);
		if (testAndSet(event)) {
			resetHover();
		} else {
			if (hovering)
				return;
			long currentTime = event.time;
			if (hoverStartTime == -1) {
				hoverStartTime = currentTime;
			} else if (currentTime - hoverStartTime > 400) {
				handleHover(event);
				hovering = true;
			}
		}
	}

	/**
	 * Called whenever the User drags over the target. By default, the target
	 * Request and target EditPart are updated, feedback is shown, and
	 * auto-expose occurs.
	 */
	protected void handleDragOver(DropTargetEvent event) {
		showTargetFeedback(event);
		if (exposeHelper != null) {
			// If the expose helper does not wish to continue, set helper to
			// null.
			if (!exposeHelper.step(getDropLocation(event)))
				exposeHelper = null;
		}
	}

	/**
	 * Updates the active {@link AutoexposeHelper}. Does nothing if there is
	 * still an active helper. Otherwise, obtains a new helper (possible
	 * <code>null</code>) at the current mouse location and calls
	 * {@link #setAutoexposeHelper(AutoexposeHelper)}.
	 */
	protected void updateAutoexposeHelper(DropTargetEvent event) {
		if (exposeHelper != null)
			return;
		AutoexposeHelper.Search search;
		search = new AutoexposeHelper.Search(getDropLocation(event));
		getViewer().findObjectAtExcluding(getDropLocation(event),
				Collections.EMPTY_LIST, search);
		setAutoexposeHelper(search.result);
	}

	/**
	 * Sets the current autoexpose helper.
	 * 
	 * @param helper
	 *            the autoexpose helper
	 */
	protected void setAutoexposeHelper(AutoexposeHelper helper) {
		exposeHelper = helper;
	}

	private void resetHover() {
		if (hovering) {
			handleHoverStop();
			hovering = false;
			hoverStartTime = -1;
			prevMouseLoc = null;
		}
	}

	protected void showTargetFeedback(DropTargetEvent event) {
		if (getTargetEditPart() != null) {
			showingFeedback = true;
			showDropFeedback(getTargetEditPart(), getDropLocation(event));
		}
	}

	protected void eraseTargetFeedback(DropTargetEvent event) {
		if (getTargetEditPart() != null && showingFeedback) {
			showingFeedback = false;
			eraseDropFeedback(getTargetEditPart(), getDropLocation(event));
		}
	}

	/**
	 * Called when the mouse resumes motion after having hovered.
	 */
	protected void handleHoverStop() {
	}

	/**
	 * Called when the mouse hovers during drag and drop.
	 */
	protected void handleHover(DropTargetEvent event) {
		updateAutoexposeHelper(event);
	}

	public void drop(DropTargetEvent event) {
		// setCurrentEvent(event);
		eraseTargetFeedback(event);
		handleDrop(event);
		unload(event);
	}

	protected void handleDrop(DropTargetEvent event) {
		// updateTargetRequest();
		// updateTargetEditPart();

		Command command = getCommand(event);
		if (command != null && command.canExecute())
			getViewer().getEditDomain().getCommandStack().execute(command);
		else
			event.detail = DND.DROP_NONE;
	}

	/**
	 * Erases target feedback and sets the request to <code>null</code>.
	 */
	protected void unload(DropTargetEvent event) {
		resetHover();
		eraseTargetFeedback(event);
		setTargetEditPart(null);
		setAutoexposeHelper(null);
	}

	protected void handleDragOperationChanged(DropTargetEvent event) {
		// Erase any old feedback now, in case the request changes substantially
		eraseTargetFeedback(event);

		// Update request based on the new operation type
		// updateTargetRequest();

		// Update the target based on the updated request
		// updateTargetEditPart();
	}

	public void dropAccept(DropTargetEvent event) {
		System.out.println("drop accept");
	}

	public Transfer getTransfer() {
		return LocalTransfer.getInstance();
	}

	protected abstract Object getObjects(final TransferData transferData);

	protected final TreeItem findTreeItemAt(Point pt) {
		return ((Tree) getViewer().getControl())
				.getItem(new org.eclipse.swt.graphics.Point(pt.x, pt.y));
	}

	protected final int findIndexOfTreeItemAt(TreeEditPart editpart, Point pt) {
		int index = -1;
		TreeItem item = findTreeItemAt(pt);
		if (item != null) {
			index = editpart.getChildren().indexOf(item.getData());
			if (index >= 0 && !isInUpperHalf(item.getBounds(), pt))
				index++;
		}
		return index;
	}

	private boolean isInUpperHalf(org.eclipse.swt.graphics.Rectangle rect,
			org.eclipse.draw2d.geometry.Point pt) {
		org.eclipse.swt.graphics.Rectangle tempRect = new org.eclipse.swt.graphics.Rectangle(
				rect.x, rect.y, rect.width, rect.height / 2);
		return tempRect
				.contains(new org.eclipse.swt.graphics.Point(pt.x, pt.y));
	}

	public boolean isEnabled(DropTargetEvent event) {
		Command command = getCommand(event);
		return command != null ? command.canExecute() : false;
	}

	protected Command getCommand(DropTargetEvent event) {
		CompoundCommand compoundCommand = new CompoundCommand();
		for (int i = 0; i < event.dataTypes.length; i++) {
			if (getTransfer().isSupportedType(event.dataTypes[i])) {
				TreeEditPart underMouseEditPart = findTargetEditPartUnderMouse(event);
				if (underMouseEditPart == null)
					continue;
				event.currentDataType = event.dataTypes[i];
				final Object source = getObjects(event.currentDataType);

				TreeEditPart targetEditPart = getTargetEditPart(
						findTargetEditPartUnderMouse(event), source, event);
				setTargetEditPart(targetEditPart);
				if (targetEditPart != null) {
					int index = findIndexOfTreeItemAt(targetEditPart,
							getDropLocation(event));
					Command command = getCommand(targetEditPart, source,
							getDomainIndexOf(index, targetEditPart, source));
					if (command != null)
						compoundCommand.add(command);
				}
			}
		}
		if (compoundCommand.size() == 1)
			return (Command) compoundCommand.getCommands().get(0);
		if (compoundCommand.isEmpty())
			return null;
		return compoundCommand;

	}

	protected abstract int getDomainIndexOf(int index,
			TreeEditPart targetEditPart, Object source);

	protected Point getDropLocation(DropTargetEvent event) {
		org.eclipse.swt.graphics.Point swt;
		swt = new org.eclipse.swt.graphics.Point(event.x, event.y);
		DropTarget target = (DropTarget) event.widget;
		swt = target.getControl().toControl(swt);
		return new Point(swt.x, swt.y);
	}

	protected Collection<?> getExclusionSet(DropTargetEvent event) {
		return Collections.EMPTY_LIST;
	}

	private TreeEditPart findTargetEditPartUnderMouse(DropTargetEvent event) {
		EditPart ep = getViewer().findObjectAtExcluding(getDropLocation(event),
				getExclusionSet(event), null);
		if (ep instanceof TreeEditPart)
			return (TreeEditPart) ep;
		return null;
	}

	protected abstract Command getCommand(TreeEditPart target, Object source,
			int index);

	public abstract TreeEditPart getTargetEditPart(
			TreeEditPart underMouseEditPart, Object source,
			DropTargetEvent event);

	private void showDropFeedback(EditPart editPart, Point pt) {

		if (editPart.getModel() instanceof EObject) {
			EObject model = (EObject) editPart.getModel();
			if (model.eContainer() != null
					&& model.eContainer().eContainer() instanceof Container)
				System.out.print("C");
			if (model instanceof AbstractComponent
					&& model.eContainer() instanceof Container)
				System.out.println(model.eClass().getName()
						+ "["
						+ ((Container) model.eContainer()).getChildren()
								.indexOf(model) + "]");
		}

		if (!(editPart instanceof TreeEditPart))
			return;
		Widget hostWidget = ((TreeEditPart) editPart).getWidget();
		Tree tree = (Tree) getViewer().getControl();

		TreeItem item = findTreeItemAt(pt);
		if (item == null) {
			if (hostWidget == tree) {
				insertMarkAfterLastChild(tree.getItems());
			}
		} else if (item == hostWidget) {
			tree.setInsertMark(null, true);
		} else {
			boolean before = isInUpperHalf(item.getBounds(), pt);
			tree.setInsertMark(item, before);
		}
	}

	private void insertMarkAfterLastChild(TreeItem[] children) {
		if (children != null && children.length > 0) {
			TreeItem item = children[children.length - 1];
			((Tree) getViewer().getControl()).setInsertMark(item, false);
		}
	}

	private void eraseDropFeedback(EditPart targetEditpart, Point pt) {
		((Tree) getViewer().getControl()).setInsertMark(null, true);
	}

	protected void setTargetEditPart(EditPart ep) {
		if (ep != target) {
			if (target != null)
				handleExitingEditPart(ep);
			target = ep;
			if (target != null)
				handleEnteredEditPart(ep);
		}
	}

	/**
	 * Returns the current <i>target</i> <code>EditPart</code>.
	 * 
	 * @return the target EditPart
	 */
	protected EditPart getTargetEditPart() {
		return target;
	}

	protected void handleEnteredEditPart(EditPart ep) {
	}

	/**
	 * Called as the current target EditPart is being exited. By default, the
	 * target is asked to erase feedback.
	 */
	protected void handleExitingEditPart(EditPart ep) {
		eraseDropFeedback(ep, null);
	}

}
