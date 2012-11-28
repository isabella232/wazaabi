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
import org.eclipse.wazaabi.engine.core.views.CollectionView;
import org.eclipse.wazaabi.mm.core.widgets.Collection;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;

public class CollectionEditPart extends AbstractComponentEditPart {

	private boolean isSelectionListening = true;

	public static final String LOOK_AND_FEEL_PROPERTY_NAME = "lookandfeel"; //$NON-NLS-1$
	public static final String CONTENT_PROVIDER_PROPERTY_NAME = "content-provider"; //$NON-NLS-1$
	public static final String LABEL_RENDERER_PROPERTY_NAME = "label-renderer"; //$NON-NLS-1$
	public static final String COLUMN_DESCRIPTOR_PROPERTY_NAME = "column-descriptor"; //$NON-NLS-1$
	public static final String DYNAMIC_PROVIDER_PROPERTY_NAME = "dynamic-provider"; //$NON-NLS-1$

	public static final String HEADER_VISIBLE_PROPERTY_NAME = "header-visible"; //$NON-NLS-1$
	public static final String ALLOW_ROW_SELECTION_PROPERTY_NAME = "allow-row-selection"; //$NON-NLS-1$
	public static final String SHOW_HORIZONTAL_LINES_PROPERTY_NAME = "show-horizontal-lines"; //$NON-NLS-1$
	public static final String MULTIPLE_SELECTION_PROPERTY_NAME = "multiple-selection";

	public EClass getModelEClass() {
		return CoreWidgetsPackage.Literals.COLLECTION;
	}

	@Override
	public void notifyChanged(Notification notification) {
		if (getWidgetView() instanceof CollectionView) {
			switch (notification.getFeatureID(Collection.class)) {
			case CoreWidgetsPackage.COLLECTION__INPUT:
				((CollectionView) getWidgetView()).setInput(notification
						.getNewValue());
				getWidgetView().fireWidgetViewRepainted();
				break;
			case CoreWidgetsPackage.COLLECTION__SELECTION:
				// switch (notification.getEventType()) {
				// case Notification.ADD:
				// throw new UnsupportedOperationException(
				// "I don\'t like Exceptions");
				// case Notification.ADD_MANY:
				// if (areEquals((List<?>) notification.getNewValue(),
				// ((Collection) getModel()).getSelection()))
				// return;
				// case Notification.REMOVE:
				// throw new UnsupportedOperationException(
				// "I don\'t like Exceptions");
				// case Notification.REMOVE_MANY:
				// if (areEquals((List<?>) notification.getOldValue(),
				// ((Collection) getModel()).getSelection()))
				// return;
				// }
				if (isSelectionListening()) {
					((CollectionView) getWidgetView())
							.setSelection(((Collection) getModel())
									.getSelection());
					getWidgetView().fireWidgetViewRepainted();
				}
				break;
			default:
				super.notifyChanged(notification);
			}
		}
	}

	public void refreshFeaturesAndStyles() {
		super.refreshFeaturesAndStyles();
		refreshUniqueStyleRule(LOOK_AND_FEEL_PROPERTY_NAME);
		refreshStyleRules(DYNAMIC_PROVIDER_PROPERTY_NAME);
		refreshStyleRules(CONTENT_PROVIDER_PROPERTY_NAME);
		refreshStyleRules(LABEL_RENDERER_PROPERTY_NAME);
		refreshStyleRules(COLUMN_DESCRIPTOR_PROPERTY_NAME);

		refreshUniqueStyleRule(MULTIPLE_SELECTION_PROPERTY_NAME);
		refreshUniqueStyleRule(HEADER_VISIBLE_PROPERTY_NAME);
		refreshUniqueStyleRule(ALLOW_ROW_SELECTION_PROPERTY_NAME);
		refreshUniqueStyleRule(SHOW_HORIZONTAL_LINES_PROPERTY_NAME);
		((CollectionView) getWidgetView()).setInput(((Collection) getModel())
				.getInput());
		((CollectionView) getWidgetView())
				.setSelection(((Collection) getModel()).getSelection());
		getWidgetView().fireWidgetViewRepainted();
	}

	public void blockSelectionListening() {
		isSelectionListening = false;
	}

	public void releaseSelectionListening() {
		isSelectionListening = true;
	}

	protected boolean isSelectionListening() {
		return isSelectionListening;
	}

	public boolean areEquals(List<?> list1, List<?> list2) {
		if (list1 == null)
			return list2 == null;
		if (list2 == null)
			return false;
		if (list1.size() != list2.size())
			return false;
		for (int i = 0; i < list1.size(); i++) {
			Object item1 = list1.get(i);
			Object item2 = list2.get(i);

			if (item1 == null) {
				if (item2 != null)
					return false;
			} else if (!item1.equals(item2))
				return false;
		}
		return true;
	}
}
