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

package org.eclipse.wazaabi.ide.ui.editparts;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.widgets.Event;
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;

public class LabelTreeEditPart extends AbstractComponentTreeEditPart {

    protected String getTextRuleValue() {
        for (StyleRule rule : ((AbstractComponent) getModel()).getStyleRules())
            if ("text".equals(rule.getPropertyName()) //$NON-NLS-1$
                    && rule instanceof StringRule)
                return ((StringRule) rule).getValue();
        return null;
    }

    protected String getExtendedInfo() {
        String textRuleValue = getTextRuleValue();
        if (textRuleValue != null)
            return textRuleValue;
        return super.getExtendedInfo();
    }

    @Override
    public void styleRuleAdded(StyleRule newRule) {
        if (newRule != null && "text".equals(newRule.getPropertyName())) //$NON-NLS-1$
            refreshVisuals();
        super.styleRuleAdded(newRule);
    }

    @Override
    public void styleRuleRemoved(StyleRule oldRule) {
        if (oldRule != null && "text".equals(oldRule.getPropertyName())) //$NON-NLS-1$
            refreshVisuals();
        super.styleRuleRemoved(oldRule);
    }

    @Override
    public void styleRuleUpdated(StyleRule rule) {
        if (rule != null && "text".equals(rule.getPropertyName())) //$NON-NLS-1$
            refreshVisuals();
        super.styleRuleUpdated(rule);
    }

    public void measureWidget(Event event) {

    }

    public void eraseWidget(Event event) {

    }

    public void paintWidget(Event event) {
        if (event.index == 1) {
            
            FontData fontData = event.gc.getFont().getFontData()[0];
            fontData.setStyle(SWT.ITALIC);
            Font font = new Font(event.display, fontData);
            event.gc.setFont(font);
            

            event.gc.drawText(getExtendedInfo(), event.x /* + TEXT_MARGIN */, event.y
                    /*+ yOffset*/, true);
            font.dispose();
        }
    }
}