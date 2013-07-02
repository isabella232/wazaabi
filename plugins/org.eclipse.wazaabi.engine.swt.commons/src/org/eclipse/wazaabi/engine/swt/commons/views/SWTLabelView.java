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
import org.eclipse.wazaabi.mm.core.styles.BlankRule;
import org.eclipse.wazaabi.mm.core.styles.HyperlinkRule;
import org.eclipse.wazaabi.mm.core.styles.ImageRule;
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.StyledElement;
import org.eclipse.wazaabi.mm.swt.descriptors.SWTDescriptorsPackage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SWTLabelView extends SWTControlView implements LabelView {
	private final Logger logger = LoggerFactory.getLogger(SWTLabelView.class);
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

	public void setText(StringRule rule) {
		if (getSWTWidget() instanceof Label)
			setLabelText(rule, (Label) getSWTWidget());
		else if (getSWTWidget() instanceof Link)
			setLinkText(rule, (Link) getSWTWidget());
	}

	protected void setLabelText(StringRule rule, Label label) {
		if (label.isDisposed())
			return;
		String currentText = label.getText();
		if (rule == null) {
			if ("".equals(currentText)) //$NON-NLS-1$
				return;
			else {
				label.setText(""); //$NON-NLS-1$
				revalidate();
			}
		} else {
			label.setText(rule.getValue() == null ? "" : rule.getValue()); //$NON-NLS-1$
			revalidate();
		}
		Item item = getSWTItem();
		if (item != null) {
			Point size = label.computeSize(SWT.DEFAULT, SWT.DEFAULT);
			if (item instanceof ToolItem)
				((ToolItem) item).setWidth(size.x);
			if (item instanceof CoolItem)
				((CoolItem) item).setPreferredSize(((CoolItem) item)
						.computeSize(size.x, size.y));
			if (item instanceof ExpandItem)
				((ExpandItem) item).setHeight(label.computeSize(SWT.DEFAULT,
						SWT.DEFAULT).y);
		}
	}

	protected void setLinkText(StringRule rule, Link link) {
		if (link.isDisposed())
			return;
		String currentText = link.getText();
		if (rule == null) {
			if ("".equals(currentText)) //$NON-NLS-1$
				return;
			else {
				link.setText(""); //$NON-NLS-1$
				revalidate();
			}
		} else {
			link.setText(rule.getValue() == null ? "" : rule.getValue()); //$NON-NLS-1$
			revalidate();
		}
		Item item = getSWTItem();
		if (item != null) {
			Point size = link.computeSize(SWT.DEFAULT, SWT.DEFAULT);
			if (item instanceof ToolItem)
				((ToolItem) item).setWidth(size.x);
			if (item instanceof CoolItem)
				((CoolItem) item).setPreferredSize(((CoolItem) item)
						.computeSize(size.x, size.y));
			if (item instanceof ExpandItem)
				((ExpandItem) item).setHeight(link.computeSize(SWT.DEFAULT,
						SWT.DEFAULT).y);
		}
	}

	protected void setImage(ImageRule rule) {
		if (rule == null)
			if (image == null)
				return;
			else {
				logger.debug("disposing image from {}",
						System.identityHashCode(this));
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
				logger.debug("disposing image from {}",
						System.identityHashCode(this));
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
		logger.debug("set Image {}", image);
		revalidate();
	}

	@Override
	public void updateStyleRule(StyleRule rule) {
		if (rule == null)
			return;
		if (LabelEditPart.TEXT_PROPERTY_NAME.equals(rule.getPropertyName())) {
			if (rule instanceof StringRule)
				setText((StringRule) rule);
			else if (rule instanceof BlankRule)
				setText(null);
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
			logger.debug("disposing image from {}",
					System.identityHashCode(this));
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
