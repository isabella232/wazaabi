package org.eclipse.wazaabi.engine.swt.editparts.stylerules.managers;

import org.eclipse.wazaabi.engine.swt.views.SWTControlView;
import org.eclipse.wazaabi.mm.swt.styles.AttachmentToContainer;
import org.eclipse.wazaabi.mm.swt.styles.FormAttachment;

public class AttachmentToContainerManager extends FormAttachmentManager {

	@Override
	protected org.eclipse.swt.layout.FormAttachment createSWTFormAttachment(
			SWTControlView view, FormAttachment formAttachment) {
		if (formAttachment instanceof AttachmentToContainer) {
			AttachmentToContainer attachmentToContainer = (AttachmentToContainer) formAttachment;
			return new org.eclipse.swt.layout.FormAttachment(
					attachmentToContainer.getNumerator(),
					attachmentToContainer.getDenominator(),
					attachmentToContainer.getOffset());
		}
		return null;
	}

}
