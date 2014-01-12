/***********************************************************************************************************************
 * Copyright (c) 2008 Olivier Moises, 2014 Pavel Erofeev
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises - initial API and implementation
 *   Pavel Erofeev - rendering engine for JavaFX
***********************************************************************************************************************/

package org.eclipse.wazaabi.engine.fx.views;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.wazaabi.engine.core.editparts.TextComponentEditPart;
import org.eclipse.wazaabi.engine.core.views.TextComponentView;
import org.eclipse.wazaabi.mm.core.styles.BooleanRule;
import org.eclipse.wazaabi.mm.core.styles.ScrollBarRule;
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.widgets.TextComponent;
import org.eclipse.wazaabi.mm.swt.descriptors.SWTDescriptorsPackage;


public class FXTextComponentView extends FXWidgetView implements TextComponentView {

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

        return text;
    }

    public void setText(String text) {
        ((Text) getSWTControl()).setText(text == null ? "" : text); //$NON-NLS-1$
        revalidate();
    }

    public String getText() {
        return ((Text) getSWTControl()).getText();
    }

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
        if (TextComponentEditPart.READ_ONLY_PROPERTY_NAME.equals(propertyName)
                && ((BooleanRule) rule).isValue())
            return SWT.READ_ONLY;
        if (TextComponentEditPart.WRAP_PROPERTY_NAME.equals(propertyName)
                && ((BooleanRule) rule).isValue())
            return SWT.WRAP;
        if (TextComponentEditPart.HORIZONTAL_SCROLLBAR_PROPERTY_NAME
                .equals(propertyName) && rule instanceof ScrollBarRule)
            return SWT.H_SCROLL;
        if (TextComponentEditPart.VERTICAL_SCROLLBAR_PROPERTY_NAME
                .equals(propertyName) && rule instanceof ScrollBarRule)
            return SWT.V_SCROLL;
        return super.computeSWTCreationStyle(rule);
    }

    @Override
    protected boolean needReCreateWidgetView(StyleRule styleRule,
            org.eclipse.swt.widgets.Widget widget) {
        if (styleRule == null)
            return false;
        if (TextComponentEditPart.MULTI_LINE_PROPERTY_NAME.equals(styleRule
                .getPropertyName()) && styleRule instanceof BooleanRule) {
            return !(isStyleBitCorrectlySet(widget, org.eclipse.swt.SWT.MULTI,
                    ((BooleanRule) styleRule).isValue()));
        } else if (TextComponentEditPart.READ_ONLY_PROPERTY_NAME
                .equals(styleRule.getPropertyName())
                && styleRule instanceof BooleanRule) {
            return !(isStyleBitCorrectlySet(widget,
                    org.eclipse.swt.SWT.READ_ONLY,
                    ((BooleanRule) styleRule).isValue()));
        } else if (TextComponentEditPart.WRAP_PROPERTY_NAME.equals(styleRule
                .getPropertyName()) && styleRule instanceof BooleanRule) {
            return !(isStyleBitCorrectlySet(widget, org.eclipse.swt.SWT.WRAP,
                    ((BooleanRule) styleRule).isValue()));
        } else if (TextComponentEditPart.HORIZONTAL_SCROLLBAR_PROPERTY_NAME
                .equals(styleRule.getPropertyName())
                && styleRule instanceof ScrollBarRule) {
            return !(isStyleBitCorrectlySet(widget,
                    org.eclipse.swt.SWT.H_SCROLL, true));
        } else if (TextComponentEditPart.VERTICAL_SCROLLBAR_PROPERTY_NAME
                .equals(styleRule.getPropertyName())
                && styleRule instanceof ScrollBarRule) {
            return !(isStyleBitCorrectlySet(widget,
                    org.eclipse.swt.SWT.V_SCROLL, true));
        } else
            return super.needReCreateWidgetView(styleRule, widget);
    }

    @Override
    public void updateStyleRule(StyleRule rule) {
        if (rule == null)
            return;

        if (TextComponentEditPart.ECHO_CHAR_PROPERTY_NAME.equals(rule
                .getPropertyName())
                && rule instanceof StringRule
                && ((StringRule) rule).getValue() != null
                && ((StringRule) rule).getValue().length() != 0) {

            ((Text) getSWTControl()).setEchoChar(((StringRule) rule).getValue()
                    .charAt(0));

        } else {
            super.updateStyleRule(rule);
        }
    }

}
