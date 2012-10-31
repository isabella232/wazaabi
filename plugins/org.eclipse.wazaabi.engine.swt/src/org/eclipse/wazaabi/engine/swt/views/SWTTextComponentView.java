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

package org.eclipse.wazaabi.engine.swt.views;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.CoolItem;
import org.eclipse.swt.widgets.ExpandItem;
import org.eclipse.swt.widgets.Item;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.wazaabi.engine.core.editparts.TextComponentEditPart;
import org.eclipse.wazaabi.engine.core.views.TextComponentView;
import org.eclipse.wazaabi.mm.core.styles.BooleanRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.widgets.TextComponent;
import org.eclipse.wazaabi.mm.swt.descriptors.SWTDescriptorsPackage;

public class SWTTextComponentView extends SWTControlView implements
		TextComponentView {

	private ModifyListener modifyListener = new ModifyListener() {
		public void modifyText(ModifyEvent e) {
			String modifiedText = ((org.eclipse.swt.widgets.Text) e.widget)
					.getText();
			if (!modifiedText.equals(((TextComponent) getHost().getModel())
					.getText()))
				((TextComponent) getHost().getModel()).setText(modifiedText);
		}
	};

	public EClass getWidgetViewEClass() {
		return SWTDescriptorsPackage.Literals.TEXT;
	}

	protected ModifyListener getModifyListener() {
		return this.modifyListener;
	}

	protected Widget createSWTWidget(Widget parent, int swtStyle, int index) {
		final Text text = new org.eclipse.swt.widgets.Text(
				(org.eclipse.swt.widgets.Composite) parent,
				computeSWTCreationStyle(getHost()));
		if (getModifyListener() != null)
			text.addModifyListener(getModifyListener());

		return checkParentLayout((Composite) parent, text);
	}

	public void setText(String text) {
		((Text) getSWTControl()).setText(text == null ? "" : text); //$NON-NLS-1$
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
		revalidate();
	}

	public String getText() {
		return ((Text) getSWTControl()).getText();
	}

	// FIXME : rename this ugly named method !!
	protected void widgetDisposed() {
		super.widgetDisposed();
		if (getSWTControl() != null && !getSWTControl().isDisposed()
				&& getModifyListener() != null)
			((Text) getSWTControl()).removeModifyListener(getModifyListener());
	}

	protected int computeSWTCreationStyle(StyleRule rule) {
		final String propertyName = rule.getPropertyName();
		if (TextComponentEditPart.MULTI_LINE_PROPERTY_NAME.equals(propertyName)
				&& ((BooleanRule) rule).isValue())
			return SWT.MULTI;
		return super.computeSWTCreationStyle(rule);
	}

	@Override
	public boolean needReCreateWidgetView(StyleRule styleRule) {
		if (styleRule == null)
			return false;
		org.eclipse.swt.widgets.Widget widget = getSWTWidget();
		if (TextComponentEditPart.MULTI_LINE_PROPERTY_NAME.equals(styleRule
				.getPropertyName()) && styleRule instanceof BooleanRule) {
			return !(isStyleBitCorrectlySet(widget, org.eclipse.swt.SWT.MULTI,
					((BooleanRule) styleRule).isValue()));
		} else
			return super.needReCreateWidgetView(styleRule);
	}

}
