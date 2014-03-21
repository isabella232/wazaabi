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

import java.util.List;

import org.eclipse.draw2d.geometry.Point;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.edit.ui.dnd.LocalTransfer;
import org.eclipse.gef.EditPart;
import org.eclipse.gef.EditPartViewer;
import org.eclipse.gef.TreeEditPart;
import org.eclipse.gef.commands.Command;
import org.eclipse.gef.commands.CompoundCommand;
import org.eclipse.gef.commands.UnexecutableCommand;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.TransferData;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.wazaabi.ide.mapping.rules.MappingRuleManager;
import org.eclipse.wazaabi.ide.ui.editparts.commands.binding.InsertNewBindingCommand;
import org.eclipse.wazaabi.ide.ui.editparts.commands.components.InsertNewComponentCommand;
import org.eclipse.wazaabi.ide.ui.editparts.commands.stylerules.InsertNewStyleRuleCommand;
import org.eclipse.wazaabi.mm.core.annotations.AnnotatedElement;
import org.eclipse.wazaabi.mm.core.annotations.Annotation;
import org.eclipse.wazaabi.mm.core.annotations.AnnotationContent;
import org.eclipse.wazaabi.mm.core.annotations.CoreAnnotationsFactory;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.core.styles.LayoutDataRule;
import org.eclipse.wazaabi.mm.core.styles.LayoutRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.StyledElement;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.handlers.Binding;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage;

public class LocalTransferDropTargetListener extends
		AbstractTransferDropTargetListener {

	private static final int sectorHeight = 4;
	private static final float ratio = new Float(sectorHeight - 1)
			/ new Float(sectorHeight);

	public LocalTransferDropTargetListener(EditPartViewer viewer) {
		super(viewer);
		assert viewer instanceof ExtendedTreeViewer;
	}

	protected MappingRuleManager getMappingRuleManager() {
		return ((ExtendedTreeViewer) getViewer()).getMappingRuleManager();
	}

	protected Command getCommand(TreeEditPart target, Object source, int index) {
		if (source == null)
			return null;
		if (target.getModel() instanceof EObject
				&& getMappingRuleManager() != null) {
			EObject targetModel = (EObject) target.getModel();

			CompoundCommand compoundCommand = new CompoundCommand();

			if (targetModel instanceof StyledElement
					&& source instanceof EObject) {
				@SuppressWarnings("unchecked")
				List<StyleRule> styleRules = (List<StyleRule>) getMappingRuleManager()
						.get(targetModel, index, (EObject) source,
								CoreStylesPackage.Literals.STYLE_RULE, null);
				for (StyleRule styleRule : styleRules) {
					InsertNewStyleRuleCommand cmd = new InsertNewStyleRuleCommand();
					cmd.setStyledElement((StyledElement) targetModel);
					cmd.setIndex(0);
					cmd.setNewStyleRule(styleRule);
					compoundCommand.add(cmd);
				}
			}

			if (targetModel instanceof Container) {
				@SuppressWarnings("unchecked")
				List<AbstractComponent> components = (List<AbstractComponent>) getMappingRuleManager()
						.get(targetModel, index, (EObject) source,
								CoreWidgetsPackage.Literals.ABSTRACT_COMPONENT,
								null);
				// int transactionId = getNextTransationId((Container)
				// targetModel);
				for (AbstractComponent component : components) {
					InsertNewComponentCommand cmd = new InsertNewComponentCommand();
					cmd.setContainer((Container) targetModel);
					cmd.setChild(component);
					cmd.setIndex(index);
					compoundCommand.add(cmd);
					addTag(component,
							new Long(System.currentTimeMillis()).toString());
				}
			}

			if (targetModel instanceof EventDispatcher) {
				@SuppressWarnings("unchecked")
				List<Binding> bindings = (List<Binding>) getMappingRuleManager()
						.get(targetModel, index, (EObject) source,
								EDPHandlersPackage.Literals.BINDING, null);
				for (Binding binding : bindings) {
					InsertNewBindingCommand cmd = new InsertNewBindingCommand();
					cmd.setNewBinding(binding);
					cmd.setEventDispatcher((EventDispatcher) targetModel);
					cmd.setIndex(0);
					compoundCommand.add(cmd);
				}
			}
			if (!compoundCommand.isEmpty())
				return compoundCommand;
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
			} else if (source instanceof EStructuralFeature) {
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

	protected boolean isInUpperPart(org.eclipse.swt.graphics.Rectangle rect,
			org.eclipse.draw2d.geometry.Point pt) {
		org.eclipse.swt.graphics.Rectangle tempRect = new org.eclipse.swt.graphics.Rectangle(
				rect.x, rect.y, rect.width, new Float(rect.height
						/ sectorHeight).intValue());
		return tempRect
				.contains(new org.eclipse.swt.graphics.Point(pt.x, pt.y));
	}

	protected boolean isInLowerPart(org.eclipse.swt.graphics.Rectangle rect,
			org.eclipse.draw2d.geometry.Point pt) {
		org.eclipse.swt.graphics.Rectangle tempRect = new org.eclipse.swt.graphics.Rectangle(
				rect.x, new Float(rect.y + 1 + ratio * rect.height).intValue(),
				rect.width, new Float(rect.height / sectorHeight).intValue());
		return tempRect
				.contains(new org.eclipse.swt.graphics.Point(pt.x, pt.y));
	}

	protected void addTag(AnnotatedElement annotatedElement, String value) {
		Annotation annotation = CoreAnnotationsFactory.eINSTANCE
				.createAnnotation();
		annotation.setSource("urn:ooo");
		AnnotationContent content = CoreAnnotationsFactory.eINSTANCE
				.createAnnotationContent();
		annotation.getContents().add(content);
		content.setKey("olivier");
		content.setValue("moises");
		annotatedElement.getAnnotations().add(annotation);
	}

	protected int getNextTransationId(AnnotatedElement uiModel) {

		return 0;
	}

}