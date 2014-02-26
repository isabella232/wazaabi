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

package org.eclipse.wazaabi.engine.gwt.views;


import org.eclipse.emf.ecore.EClass;
import org.eclipse.wazaabi.engine.core.editparts.PushButtonEditPart;
import org.eclipse.wazaabi.engine.core.views.PushButtonView;
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;

import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.Panel;
import com.google.gwt.user.client.ui.Widget;


public class GWTPushButtonView extends GWTWidgetView implements PushButtonView {

    public EClass getWidgetViewEClass() {
        return CoreWidgetsPackage.Literals.PUSH_BUTTON;
    }

    protected Widget createGWTWidget(Panel parent, int index) {
        return createButton(parent, index);
    }

    protected Button createButton(Panel parent, int index) {
        Button b = new Button();
        GWTLayoutUtil.addChild(b, parent, index);
        return b;
    }

    protected void setText(StringRule rule) {
        String currentText = ((Button) getGWTWidget()).getText();
        if (rule == null) {
            if ("".equals(currentText)) //$NON-NLS-1$
                return;
            else {
                ((Button) getGWTWidget()).setText(""); //$NON-NLS-1$
                revalidate();
            }
        } else {
            ((Button) getGWTWidget()).setText(rule.getValue() == null ? "" : rule.getValue()); //$NON-NLS-1$
            revalidate();
        }
    }


    @Override
    public void updateStyleRule(StyleRule rule) {
        if (rule == null)
            return;
        if (PushButtonEditPart.TEXT_PROPERTY_NAME.equals(rule.getPropertyName())) {
            if (rule instanceof StringRule)
                setText((StringRule) rule);
            else
                setText(null);
        } else
            super.updateStyleRule(rule);
    }


//    @Override
//    protected boolean needReCreateWidgetView(StyleRule styleRule,
//            org.eclipse.swt.widgets.Widget widget) {
//        if (styleRule == null)
//            return false;
//        if (AbstractButtonEditPart.FLAT_PROPERTY_NAME.equals(styleRule
//                .getPropertyName()) && styleRule instanceof BooleanRule) {
//            return !(isStyleBitCorrectlySet(widget, org.eclipse.swt.SWT.FLAT,
//                    ((BooleanRule) styleRule).isValue()));
//        } else
//            return super.needReCreateWidgetView(styleRule, widget);
//    }
}
