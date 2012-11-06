package org.eclipse.wazaabi.ide.ui.editors.viewer;

import java.util.List;

import org.eclipse.draw2d.geometry.Point;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.edit.ui.dnd.LocalTransfer;
import org.eclipse.gef.EditPart;
import org.eclipse.gef.EditPartViewer;
import org.eclipse.gef.TreeEditPart;
import org.eclipse.gef.commands.Command;
import org.eclipse.gef.commands.UnexecutableCommand;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.TransferData;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.wazaabi.ide.ui.editparts.commands.InsertNewComponentCommand;
import org.eclipse.wazaabi.mm.core.styles.LayoutDataRule;
import org.eclipse.wazaabi.mm.core.styles.LayoutRule;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;

public class LocalTransferDropTargetListener extends
		AbstractTransferDropTargetListener {

	public LocalTransferDropTargetListener(EditPartViewer viewer) {
		super(viewer);
	}

	protected Command getCommand(TreeEditPart target, Object source, int index) {
		if (source == null)
			return null;
		if (target.getModel() instanceof EObject) {
			EObject targetModel = (EObject) target.getModel();
			if (source instanceof EAttribute
					&& targetModel instanceof Container) {
				EAttribute attr = (EAttribute) source;
				InsertNewComponentCommand cmd = new InsertNewComponentCommand();
				cmd.setContainer((Container) targetModel);
				cmd.setChild(CoreWidgetsFactory.eINSTANCE.createTextComponent());
				cmd.setIndex(index);
				return cmd;
			}
		}
		return UnexecutableCommand.INSTANCE;
	}

	public TreeEditPart getTargetEditPart(TreeEditPart underMouseEditPart,
			Object source, DropTargetEvent event) {

		if (underMouseEditPart == null)
			return null;
		if (underMouseEditPart.getModel() instanceof EObject) {
			EObject model = (EObject) underMouseEditPart.getModel();
			if (source instanceof EClass) {
				if (model instanceof Container)
					return underMouseEditPart;
				else if (model instanceof AbstractComponent)
					return (TreeEditPart) underMouseEditPart.getParent();
				else if (model instanceof LayoutDataRule
						|| model instanceof LayoutRule)
					return (TreeEditPart) (underMouseEditPart.getParent() != null ? underMouseEditPart
							.getParent().getParent() : null);
			} else if (source instanceof EAttribute) {
				if (model instanceof Container)
					return underMouseEditPart;
				else if (model instanceof AbstractComponent) {
					Point pt = getDropLocation(event);
					TreeItem item = findTreeItemAt(pt);
					if (item != null) {
						if (isInUpperPart(item.getBounds(), pt)) {
							return (TreeEditPart) underMouseEditPart
									.getParent();
						} else if (isInLowerPart(item.getBounds(), pt)) {
							return (TreeEditPart) underMouseEditPart
									.getParent();
						} else {
							return underMouseEditPart;
						}
					}
				} else if (model instanceof LayoutDataRule
						|| model instanceof LayoutRule)
					return (TreeEditPart) (underMouseEditPart.getParent() != null ? underMouseEditPart
							.getParent() : null);
			}

		}
		return null;
	}

	protected Object getObjects(final TransferData transferData) {
		if (transferData == null)
			return null;
		Object javaObject = ((LocalTransfer) getTransfer())
				.nativeToJava(transferData);
		if (javaObject instanceof TreeSelection) {
			return ((TreeSelection) javaObject).getFirstElement();
		}
		return javaObject;
	}

	private static final int sectorHeight = 4;
	private static final float ratio = new Float(sectorHeight - 1)
			/ new Float(sectorHeight);

	private boolean isInUpperPart(org.eclipse.swt.graphics.Rectangle rect,
			org.eclipse.draw2d.geometry.Point pt) {
		org.eclipse.swt.graphics.Rectangle tempRect = new org.eclipse.swt.graphics.Rectangle(
				rect.x, rect.y, rect.width, new Float(rect.height
						/ sectorHeight).intValue());
		return tempRect
				.contains(new org.eclipse.swt.graphics.Point(pt.x, pt.y));
	}

	private boolean isInLowerPart(org.eclipse.swt.graphics.Rectangle rect,
			org.eclipse.draw2d.geometry.Point pt) {
		org.eclipse.swt.graphics.Rectangle tempRect = new org.eclipse.swt.graphics.Rectangle(
				rect.x, new Float(rect.y + 1 + ratio * rect.height).intValue(),
				rect.width, new Float(rect.height / sectorHeight).intValue());
		return tempRect
				.contains(new org.eclipse.swt.graphics.Point(pt.x, pt.y));
	}

	@SuppressWarnings("unchecked")
	@Override
	protected int getDomainIndexOf(int index, TreeEditPart targetEditPart,
			Object source) {
		if (targetEditPart == null)
			return -1;

		if (index == -1)
			return index;
		int nbrOfNonComponents = 0;
		for (EditPart ep : (List<EditPart>) targetEditPart.getChildren())
			if (!(ep.getModel() instanceof AbstractComponent))
				nbrOfNonComponents++;
		return Math.max(0, index - nbrOfNonComponents);
	}

}
