package org.eclipse.wazaabi.engine.swt.commons.editparts.stylerules.managers;

import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.wazaabi.engine.swt.commons.views.SWTControlView;
import org.eclipse.wazaabi.mm.swt.styles.FormAttachment;

public abstract class FormAttachmentManager extends AdapterImpl {

	// @Override
	// public void notifyChanged(Notification notification) {
	// assert getHost() != null;
	// if (notification.getEventType() != Notification.SET)
	// return;
	// boolean hasChanged = false;
	// switch (notification.getFeatureID(RowDataRule.class)) {
	// case SWTStylesPackage.FORM_ATTACHMENT__OFFSET:
	// hasChanged = !CompareUtils.areEquals(notification.getOldIntValue(),
	// notification.getNewIntValue());
	// break;
	// default:
	// super.notifyChanged(notification);
	// }
	// if (hasChanged
	// && getHost().styleRuleUpdated(
	// (StyleRule) notification.getNotifier()))
	// reCreateWidgetView();
	// }

	protected abstract org.eclipse.swt.layout.FormAttachment createSWTFormAttachment(
			SWTControlView view, FormAttachment formAttachment);

}
