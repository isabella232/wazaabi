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

package org.eclipse.wazaabi.engine.swt.commons.editparts.stylerules.managers;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.wazaabi.engine.core.editparts.AbstractComponentEditPart;
import org.eclipse.wazaabi.engine.core.editparts.AbstractWidgetEditPart.StyleRuleManager;
import org.eclipse.wazaabi.engine.edp.CompareUtils;
import org.eclipse.wazaabi.engine.swt.commons.views.SWTControlView;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.StyledElement;
import org.eclipse.wazaabi.mm.swt.styles.AttachmentToContainer;
import org.eclipse.wazaabi.mm.swt.styles.AttachmentToSibling;
import org.eclipse.wazaabi.mm.swt.styles.FormAttachment;
import org.eclipse.wazaabi.mm.swt.styles.FormDataRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage;

public class FormDataStyleRuleManager extends StyleRuleManager {

	@Override
	public void notifyChanged(Notification notification) {
		assert getHost() != null;
		if (notification.getEventType() != Notification.SET)
			return;
		boolean hasChanged = false;
		switch (notification.getFeatureID(FormDataRule.class)) {

		case SWTStylesPackage.FORM_DATA_RULE__HEIGHT:
		case SWTStylesPackage.FORM_DATA_RULE__WIDTH:
			hasChanged = !CompareUtils.areEquals(notification.getOldIntValue(),
					notification.getNewIntValue());
			break;
		case SWTStylesPackage.FORM_DATA_RULE__BOTTOM:
		case SWTStylesPackage.FORM_DATA_RULE__LEFT:
		case SWTStylesPackage.FORM_DATA_RULE__RIGHT:
		case SWTStylesPackage.FORM_DATA_RULE__TOP:
			// TODO : do we need to make a distinction between cases ?
			hasChanged = true;
			break;
		default:
			super.notifyChanged(notification);
		}
		if (hasChanged
				&& getHost().styleRuleUpdated(
						(StyleRule) notification.getNotifier()))
			reCreateWidgetView();
	}

	/**
	 * Given a FormDataRule, creates a SWT FormData and sets its data using the
	 * FormDataRule.
	 * 
	 * @param rule
	 * @return Always a FormData
	 */
	private static FormData convertIntoSWTLayoutData(SWTControlView view,
			FormDataRule rule) {
		FormData formData = new FormData();
		formData.height = rule.getHeight();
		formData.width = rule.getWidth();
		formData.bottom = convertIntoSWTFormAttachment(view, rule.getBottom());
		formData.left = convertIntoSWTFormAttachment(view, rule.getLeft());
		formData.right = convertIntoSWTFormAttachment(view, rule.getRight());
		formData.top = convertIntoSWTFormAttachment(view, rule.getTop());
		return formData;
	}

	private static org.eclipse.swt.layout.FormAttachment convertIntoSWTFormAttachment(
			SWTControlView view, FormAttachment formAttachment) {
		FormAttachmentManager attachmentManager = createAttachmentManager(formAttachment);
		if (attachmentManager != null) {
			formAttachment.eAdapters().add(attachmentManager);
			return attachmentManager.createSWTFormAttachment(view,
					formAttachment);
		}
		return null;
	}

	/**
	 * Synchronizes the given widgetView according with the given rule.If the
	 * container's layout is a FormLayout, the method sets the SWT Control's
	 * layout data with : the rule's data if the rule is not null, the first
	 * FormDataRule otherwise. If no FormDataRule is attached to the component,
	 * then the control's layout data is set to null.
	 * 
	 * @param widgetView
	 * @param rule
	 */
	public static void platformSpecificRefresh(Object widgetView,
			FormDataRule rule) {
		if (!(widgetView instanceof SWTControlView))
			return;
		SWTControlView context = (SWTControlView) widgetView;
		if (!(context.getSWTWidget() instanceof Control)
				|| context.getSWTWidget().isDisposed())
			return;
		if (context.getParent() == null
				|| !(context.getParent().getSWTWidget() instanceof Composite))
			return;
		Composite parent = (Composite) context.getParent().getSWTWidget();
		if (parent.getLayout() instanceof FormLayout)
			((Control) context.getSWTWidget())
					.setLayoutData(convertIntoSWTLayoutData(context, rule));
		else if (rule == null)
			((Control) context.getSWTWidget())
					.setLayoutData(getFirstFormData(context));
	}

	/**
	 * Returns a SWT FormData built by getting data from the first FormDataRule
	 * found in the list of style rules attached to this WidgetView's model.
	 * 
	 * @param widgetView
	 * @return
	 */
	protected static FormData getFirstFormData(SWTControlView widgetView) {
		return getFirstFormData((AbstractComponentEditPart) widgetView
				.getHost());
	}

	/**
	 * Returns a SWT FormData built by getting data from the first FormDataRule
	 * found in the list of style rules attached to this editPart's model.
	 * 
	 * @param editPart
	 *            A not null editPart
	 * @return
	 */
	static FormData getFirstFormData(AbstractComponentEditPart editPart) {
		assert editPart != null;
		for (StyleRule rule : ((StyledElement) editPart.getModel())
				.getStyleRules())
			if (rule instanceof FormDataRule
					&& editPart.getWidgetView() instanceof SWTControlView)
				return convertIntoSWTLayoutData(
						(SWTControlView) editPart.getWidgetView(),
						(FormDataRule) rule);
		return null;
	}

	static FormAttachmentManager createAttachmentManager(
			FormAttachment formAttachment) {
		FormAttachmentManager attachmentManager = null;
		if (formAttachment instanceof AttachmentToContainer)
			attachmentManager = new AttachmentToContainerManager();
		if (formAttachment instanceof AttachmentToSibling)
			attachmentManager = new AttachmentToSiblingManager();
		return attachmentManager;
	}
}
