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



//public class FXLabelView extends FXWidgetView implements LabelView {
//
//    public EClass getWidgetViewEClass() {
//        return SWTDescriptorsPackage.Literals.LABEL;
//    }
//
//    protected Widget createSWTWidget(Widget parent, int swtStyle, int index) {
//        StyleRule lookandfeel = ((StyledElement) getHost().getModel())
//                .getFirstStyleRule(LabelEditPart.LOOKANDFEEL_PROPERTY_NAME,
//                        null);
//        Control label = null;
//        if (lookandfeel instanceof HyperlinkRule)
//            label = createLink((Composite) parent,
//                    computeSWTCreationStyle(getHost()));
//        else
//            label = createLabel((Composite) parent,
//                    computeSWTCreationStyle(getHost()));
//        return label;
//    }
//
//    protected Control createLabel(Composite parent, int style) {
//        return new Label((Composite) parent, style);
//    }
//
//    protected Control createLink(Composite parent, int style) {
//        return new Link((Composite) parent, style);
//    }
//
//    public void setText(StringRule rule) {
//        if (getSWTWidget() instanceof Label)
//            setLabelText(rule, (Label) getSWTWidget());
//        else if (getSWTWidget() instanceof Link)
//            setLinkText(rule, (Link) getSWTWidget());
//    }
//
//    protected void setLabelText(StringRule rule, Label label) {
//        if (label.isDisposed())
//            return;
//        String currentText = label.getText();
//        if (rule == null) {
//            if ("".equals(currentText)) //$NON-NLS-1$
//                return;
//            else {
//                label.setText(""); //$NON-NLS-1$
//                revalidate();
//            }
//        } else {
//            label.setText(rule.getValue() == null ? "" : rule.getValue()); //$NON-NLS-1$
//            revalidate();
//        }
//        Item item = getSWTItem();
//        if (item != null) {
//            Point size = label.computeSize(SWT.DEFAULT, SWT.DEFAULT);
//            if (item instanceof ToolItem)
//                ((ToolItem) item).setWidth(size.x);
//            if (item instanceof CoolItem)
//                ((CoolItem) item).setPreferredSize(((CoolItem) item)
//                        .computeSize(size.x, size.y));
//            if (item instanceof ExpandItem)
//                ((ExpandItem) item).setHeight(label.computeSize(SWT.DEFAULT,
//                        SWT.DEFAULT).y);
//        }
//    }
//
//    protected void setLinkText(StringRule rule, Link link) {
//        if (link.isDisposed())
//            return;
//        String currentText = link.getText();
//        if (rule == null) {
//            if ("".equals(currentText)) //$NON-NLS-1$
//                return;
//            else {
//                link.setText(""); //$NON-NLS-1$
//                revalidate();
//            }
//        } else {
//            link.setText(rule.getValue() == null ? "" : rule.getValue()); //$NON-NLS-1$
//            revalidate();
//        }
//        Item item = getSWTItem();
//        if (item != null) {
//            Point size = link.computeSize(SWT.DEFAULT, SWT.DEFAULT);
//            if (item instanceof ToolItem)
//                ((ToolItem) item).setWidth(size.x);
//            if (item instanceof CoolItem)
//                ((CoolItem) item).setPreferredSize(((CoolItem) item)
//                        .computeSize(size.x, size.y));
//            if (item instanceof ExpandItem)
//                ((ExpandItem) item).setHeight(link.computeSize(SWT.DEFAULT,
//                        SWT.DEFAULT).y);
//        }
//    }
//
//
//    @Override
//    public void updateStyleRule(StyleRule rule) {
//        if (rule == null)
//            return;
//        if (LabelEditPart.TEXT_PROPERTY_NAME.equals(rule.getPropertyName())) {
//            if (rule instanceof StringRule)
//                setText((StringRule) rule);
//            else if (rule instanceof BlankRule)
//                setText(null);
//        }
//
//        else
//            super.updateStyleRule(rule);
//    }
//
//    @Override
//    protected boolean needReCreateWidgetView(StyleRule rule, Widget widget) {
//        if (rule == null) {
//            return false;
//        }
//        if (LabelEditPart.LOOKANDFEEL_PROPERTY_NAME.equals(rule
//                .getPropertyName())) {
//            if (rule instanceof HyperlinkRule) {
//                return true;
//            }
//        }
//        return super.needReCreateWidgetView(rule, widget);
//
//    }
//
//}
