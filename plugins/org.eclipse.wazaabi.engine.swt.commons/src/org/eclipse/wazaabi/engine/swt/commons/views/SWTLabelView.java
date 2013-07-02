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

package org.eclipse.wazaabi.engine.swt.commons.views;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.CoolItem;
import org.eclipse.swt.widgets.ExpandItem;
import org.eclipse.swt.widgets.Item;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.wazaabi.engine.core.editparts.LabelEditPart;
import org.eclipse.wazaabi.engine.core.views.LabelView;
import org.eclipse.wazaabi.engine.swt.commons.editparts.stylerules.managers.ImageRuleManager;
import org.eclipse.wazaabi.mm.core.styles.HyperlinkRule;
import org.eclipse.wazaabi.mm.core.styles.ImageRule;
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.StyledElement;
import org.eclipse.wazaabi.mm.swt.descriptors.SWTDescriptorsPackage;

public class SWTLabelView extends SWTControlView implements LabelView {

	private Image image = null;

	public EClass getWidgetViewEClass() {
		return SWTDescriptorsPackage.Literals.LABEL;
	}

	protected Widget createSWTWidget(Widget parent, int swtStyle, int index) {
		StyleRule lookandfeel = ((StyledElement) getHost().getModel())
				.getFirstStyleRule(LabelEditPart.LOOKANDFEEL_PROPERTY_NAME,
						null);
		Control label = null;
		if (lookandfeel instanceof HyperlinkRule)
			label = createLink((Composite) parent,
					computeSWTCreationStyle(getHost()));
		else
			label = createLabel((Composite) parent,
					computeSWTCreationStyle(getHost()));
		return wrapForSpecificParent((Composite) parent, label);
	}

	protected Control createLabel(Composite parent, int style) {
		return new Label((Composite) parent, style);
	}

	protected Control createLink(Composite parent, int style) {
		return new Link((Composite) parent, style);
	}

	protected void setText(StringRule rule) {
		String currentText = ((Label) getSWTControl()).getText();
		if (rule == null) {
			if ("".equals(currentText)) //$NON-NLS-1$
				return;
			else {
				((Label) getSWTControl()).setText(""); //$NON-NLS-1$
				revalidate();
			}
		} else {
			((Label) getSWTControl())
					.setText(rule.getValue() == null ? "" : rule.getValue()); //$NON-NLS-1$
			revalidate();
		}
		Item item = getSWTItem();
		if (item != null) {
			Point size = ((Label) getSWTControl()).computeSize(SWT.DEFAULT,
					SWT.DEFAULT);
			if (item instanceof ToolItem)
				((ToolItem) item).setWidth(size.x);
			if (item instanceof CoolItem)
				((CoolItem) item).setPreferredSize(((CoolItem) item)
						.computeSize(size.x, size.y));
			if (item instanceof ExpandItem)
				((ExpandItem) item).setHeight(getSWTControl().computeSize(
						SWT.DEFAULT, SWT.DEFAULT).y);
		}
	}

	protected void setLinkText(StringRule rule) {
		String currentText = ((Link) getSWTControl()).getText();
		if (rule == null) {
			if ("".equals(currentText)) //$NON-NLS-1$
				return;
			else {
				((Link) getSWTControl()).setText(""); //$NON-NLS-1$
				revalidate();
			}
		} else {
			((Link) getSWTControl())
					.setText(rule.getValue() == null ? "" : rule.getValue()); //$NON-NLS-1$
			revalidate();
		}
		Item item = getSWTItem();
		if (item != null) {
			Point size = ((Link) getSWTControl()).computeSize(SWT.DEFAULT,
					SWT.DEFAULT);
			if (item instanceof ToolItem)
				((ToolItem) item).setWidth(size.x);
			if (item instanceof CoolItem)
				((CoolItem) item).setPreferredSize(((CoolItem) item)
						.computeSize(size.x, size.y));
			if (item instanceof ExpandItem)
				((ExpandItem) item).setHeight(getSWTControl().computeSize(
						SWT.DEFAULT, SWT.DEFAULT).y);
		}
	}

	protected void setImage(ImageRule rule) {
		if (rule == null)
			if (image == null)
				return;
			else {
				System.out.println("disposing image from "
						+ System.identityHashCode(this));
				image.dispose();
				image = null;
			}
		else {
			Image newImage = ImageRuleManager.convertToPlatformSpecificObject(
					this, rule);
			if (image != null) {
				if (newImage != null
						&& image.getImageData().equals(newImage.getImageData()))
					return;
				System.out.println("disposing image from "
						+ System.identityHashCode(this));
				image.dispose();
			}
			image = newImage;
		}
		((Label) getSWTControl()).setImage(image);
		getSWTControl().update();
		Item item = getSWTItem();
		if (item != null) {
			Point size = ((Label) getSWTControl()).computeSize(SWT.DEFAULT,
					SWT.DEFAULT);
			if (item instanceof ToolItem)
				((ToolItem) item).setWidth(size.x);
			if (item instanceof CoolItem)
				((CoolItem) item).setPreferredSize(((CoolItem) item)
						.computeSize(size.x, size.y));
			if (item instanceof ExpandItem)
				((ExpandItem) item).setHeight(getSWTControl().computeSize(
						SWT.DEFAULT, SWT.DEFAULT).y);
		}
		System.out.println("setImage " + image);
		revalidate();
	}

	@Override
	public void updateStyleRule(StyleRule rule) {
		if (rule == null)
			return;
		if (LabelEditPart.TEXT_PROPERTY_NAME.equals(rule.getPropertyName())) {
			if (rule instanceof StringRule)
				if (getSWTControl() instanceof Label) {
					setText((StringRule) rule);
				} else {
					if (getSWTControl() instanceof Link)
						setLinkText((StringRule) rule);
				}
			else {
				if (getSWTControl() instanceof Label) {
					setText(null);
				} else {
					if (getSWTControl() instanceof Link)
						setLinkText(null);
				}
			}
		} else if (LabelEditPart.IMAGE_PROPERTY_NAME.equals(rule
				.getPropertyName())) {
			if (getSWTControl() instanceof Label) {
				if (rule instanceof ImageRule)
					setImage((ImageRule) rule);
				else
					setImage(null);
			}
		}

		else
			super.updateStyleRule(rule);
	}

	protected void widgetDisposed() {
		super.widgetDisposed();
		if (image != null && !image.isDisposed()) {
			System.out.println("disposing image from "
					+ System.identityHashCode(this));
			image.dispose();
		}
	}

	@Override
	protected boolean needReCreateWidgetView(StyleRule rule, Widget widget) {
		if (rule == null) {
			return false;
		}
		if (LabelEditPart.LOOKANDFEEL_PROPERTY_NAME.equals(rule
				.getPropertyName())) {
			if (rule instanceof HyperlinkRule) {
				return true;
			}
		}
		return super.needReCreateWidgetView(rule, widget);

	}

}
