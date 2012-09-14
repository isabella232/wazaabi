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

package org.eclipse.wazaabi.engine.core.editparts;

import java.util.List;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.wazaabi.engine.core.gef.EditPart;
import org.eclipse.wazaabi.engine.core.views.MenuComponentView;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;
import org.eclipse.wazaabi.mm.core.widgets.MenuComponent;


public class MenuComponentEditPart extends AbstractWidgetEditPart {
	
	public static final String IMAGE_PROPERTY_NAME = "image"; //$NON-NLS-1$
	public static final String TYPE_PROPERTY_NAME = "type"; //$NON-NLS-1$
	
	public EClass getModelEClass() {
		return CoreWidgetsPackage.Literals.MENU_COMPONENT;
	}
	
	@Override
	public void notifyChanged(Notification notification) {
		// TODO Auto-generated method stub
		switch (notification
				.getFeatureID(org.eclipse.wazaabi.mm.core.widgets.Widget.class)) {
		case CoreWidgetsPackage.MENU_COMPONENT__CHILDREN:
			switch (notification.getEventType()){
			case Notification.ADD:
				if (notification.getNewValue() instanceof MenuComponent){
					createChildEditPartAndInsert((MenuComponent)notification.getNewValue(), notification.getPosition());
					break;
				}
			}
		default:
			super.notifyChanged(notification);
		}
	}
	
	protected void createChildEditPartAndInsert(MenuComponent componentModel, int position){
		if (componentModel == null)
			return;
		EditPart newChild = createChild(componentModel);
		if (newChild instanceof MenuComponentEditPart) {
			addChild(newChild, position);
			((MenuComponentEditPart) newChild)
					.processPostUIBuilding();
		}
	}
	
	protected void refreshFeaturesAndStyles() {
		MenuComponentView view = ((MenuComponentView) getWidgetView()); 
		view.setText(((MenuComponent)getModel()).getText());
		view.setEnabled(((MenuComponent)getModel()).isEnabled());
		refreshUniqueStyleRule(TYPE_PROPERTY_NAME);
		refreshUniqueStyleRule(IMAGE_PROPERTY_NAME);
	}
	
	protected void removeSubtreeVisuals(
			AbstractWidgetEditPart rootEditPart) {
		// only containers have children
		if (rootEditPart instanceof MenuComponentEditPart) {
			for (EditPart child : rootEditPart.getChildren())
				if (child.getParent() instanceof MenuComponentEditPart
						&& child instanceof MenuComponentEditPart)
					((MenuComponentEditPart) child.getParent())
							.removeSubtreeVisuals((AbstractWidgetEditPart) child);
		}
		removeChildVisual(rootEditPart);
	}

	protected void addSubtreeVisuals(
			AbstractWidgetEditPart rootEditPart, int idx) {
		addChildVisual(rootEditPart, idx);
		if (rootEditPart instanceof MenuComponentEditPart) {
			int i = 0;
			for (EditPart child : ((MenuComponentEditPart) rootEditPart)
					.getChildren())
				if (child.getParent() instanceof MenuComponentEditPart
						&& child instanceof MenuComponentEditPart)
					((MenuComponentEditPart) child.getParent())
							.addSubtreeVisuals(
									(MenuComponentEditPart) child, i++);
		}
	}

	protected void refreshDescendantsFeaturesAndStyles() {
		for (EditPart child : getChildren()) {
			if (child instanceof MenuComponentEditPart) {
				((MenuComponentEditPart) child)
						.refreshDescendantsFeaturesAndStyles();
				((AbstractWidgetEditPart) child)
						.refreshFeaturesAndStyles();
			} else if (child instanceof AbstractWidgetEditPart)
				((AbstractWidgetEditPart) child)
						.refreshFeaturesAndStyles();
		}
	}

	protected void replaceChildVisual() {
		int idx = getParent().getChildren().indexOf(this);
		if (getParent() instanceof MenuComponentEditPart) {
			((MenuComponentEditPart) getParent()).removeSubtreeVisuals(this);
			((MenuComponentEditPart) getParent()).addSubtreeVisuals(this, idx);
		}
	}

	protected List<MenuComponent> getModelChildren() {
		return ((MenuComponent) getModel()).getChildren();
	}

	protected void reCreateWidgetView() {
		replaceChildVisual();
		refreshDescendantsFeaturesAndStyles();
		refreshFeaturesAndStyles();
		getWidgetView().fireWidgetViewRepainted();
	}

	
	
	
}
