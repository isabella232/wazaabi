package org.eclipse.wazaabi.engine.swt.editparts.stylerules.managers;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Control;
import org.eclipse.wazaabi.engine.core.editparts.AbstractComponentEditPart;
import org.eclipse.wazaabi.engine.core.editparts.ContainerEditPart;
import org.eclipse.wazaabi.engine.core.gef.EditPart;
import org.eclipse.wazaabi.engine.swt.views.SWTControlView;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.swt.styles.AttachmentToSibling;
import org.eclipse.wazaabi.mm.swt.styles.FormAttachment;
import org.eclipse.wazaabi.mm.swt.styles.ToSiblingAlignment;

public class AttachmentToSiblingManager extends FormAttachmentManager {

	@Override
	protected org.eclipse.swt.layout.FormAttachment createSWTFormAttachment(
			SWTControlView view, FormAttachment formAttachment) {
		if (formAttachment instanceof AttachmentToSibling && view != null) {
			AttachmentToSibling attachmentToSibling = (AttachmentToSibling) formAttachment;
			Control siblingControl = resolveSiblingControl(view,
					attachmentToSibling.getSiblingId());
			if (siblingControl != null)
				return new org.eclipse.swt.layout.FormAttachment(
						siblingControl, attachmentToSibling.getOffset(),
						getSWTAlignment(attachmentToSibling.getAlignment()));
		}
		return null;
	}

	protected Control resolveSiblingControl(SWTControlView view,
			String siblingID) {
		if (siblingID == null || "".equals(siblingID)) //$NON-NLS-1$
			return null;
		if (view.getHost().getParent() instanceof ContainerEditPart) {
			for (EditPart child : ((ContainerEditPart) view.getHost()
					.getParent()).getChildren()) {
				if (child instanceof AbstractComponentEditPart
						&& siblingID
								.equals(((AbstractComponent) ((AbstractComponentEditPart) child)
										.getModel()).getId())
						&& ((AbstractComponentEditPart) child).getWidgetView() instanceof SWTControlView) {
					return (Control) ((SWTControlView) ((AbstractComponentEditPart) child)
							.getWidgetView()).getSWTWidget();
				}
			}

		}
		return null;
	}

	protected int getSWTAlignment(ToSiblingAlignment alignment) {
		if (alignment != null)
			switch (alignment.getValue()) {
			case ToSiblingAlignment.BOTTOM_VALUE:
				return SWT.BOTTOM;
			case ToSiblingAlignment.CENTER_VALUE:
				return SWT.CENTER;
			case ToSiblingAlignment.LEFT_VALUE:
				return SWT.LEFT;
			case ToSiblingAlignment.RIGHT_VALUE:
				return SWT.RIGHT;
			case ToSiblingAlignment.TOP_VALUE:
				return SWT.TOP;
			}
		return SWT.DEFAULT;
	}
}
