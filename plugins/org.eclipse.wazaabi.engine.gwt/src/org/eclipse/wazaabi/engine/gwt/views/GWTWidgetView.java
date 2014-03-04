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

import java.util.HashMap;
import java.util.List;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.wazaabi.engine.core.editparts.AbstractComponentEditPart;
import org.eclipse.wazaabi.engine.core.editparts.WidgetEditPart;
import org.eclipse.wazaabi.engine.core.editparts.WidgetViewListener;
import org.eclipse.wazaabi.engine.core.editparts.stylerules.StylePropertyDescriptor;
import org.eclipse.wazaabi.engine.core.gef.editparts.ListenerList;
import org.eclipse.wazaabi.engine.core.views.AbstractComponentView;
import org.eclipse.wazaabi.engine.core.views.WidgetView;
import org.eclipse.wazaabi.mm.core.styles.BooleanRule;
import org.eclipse.wazaabi.mm.core.styles.ColorRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;

import com.google.gwt.user.client.ui.Panel;
import com.google.gwt.user.client.ui.Widget;


public abstract class GWTWidgetView implements AbstractComponentView {

    private WidgetEditPart host;
    private final ListenerList listenerList = new ListenerList();
    private Widget widget;


    public HashMap<String, StylePropertyDescriptor> getPlatformSpecificStylePropertyDescriptors() {
        throw new RuntimeException();
    }
    
    public void setHost(WidgetEditPart host) { this.host = host; }
    public WidgetEditPart getHost() { return host; }

    public GWTWidgetView getParent() {
        if (getHost().getParent() instanceof AbstractComponentEditPart)
            return (GWTWidgetView) ((AbstractComponentEditPart) getHost().getParent()).getWidgetView();
        return null;
    }

    public void add(WidgetView childView, int index) {
        if (!(childView instanceof GWTWidgetView))
            throw new RuntimeException("Invalid parent WidgetView");
        Widget newNode = ((GWTWidgetView) childView).createGWTWidget((Panel) getGWTWidget(), index);
        if (newNode == null)
            throw new RuntimeException("Unable to create FX node");

        ((GWTWidgetView) childView).widget = newNode;
        // TODO ?
        //newNode.addDisposeListener(((FXWidgetView) childView).disposeListener);
    }

    public void remove(WidgetView view) {
        if (!(view instanceof GWTWidgetView))
            throw new RuntimeException("Invalid view");
        if (!(view.getParent() instanceof GWTWidgetView))
            throw new RuntimeException("Invalid parent view");

        if (((GWTWidgetView) view).getGWTWidget() != null) {
            GWTWidgetView parent = (GWTWidgetView) view.getParent();
            ((Panel) parent.getGWTWidget()).remove(((GWTWidgetView) view).getGWTWidget());
            // TODO ?
            ;//((FXWidgetView) view).getSWTWidget().dispose();
        }
    }


    public void addWidgetViewListener(WidgetViewListener l)    { listenerList.add(l);    }
    public void removeWidgetViewListener(WidgetViewListener l) { listenerList.remove(l); }

    public void fireWidgetViewRepainted() {
        for (Object l : listenerList.getListeners())
            ((WidgetViewListener) l).viewChanged(this, WidgetViewListener.VIEW_REPAINTED);
    }

    public void fireWidgetViewValidated() {
        for (Object l : listenerList.getListeners())
            ((WidgetViewListener) l).viewChanged(this, WidgetViewListener.VIEW_VALIDATED);
    }

    protected abstract Widget createGWTWidget(Panel parent, int index);

    public Widget getGWTWidget() {
        return widget;
    }

    public abstract EClass getWidgetViewEClass();


    public boolean needReCreateWidgetView(List<StyleRule> styleRules) {
        for (StyleRule styleRule : styleRules)
            if (needReCreateWidgetView(styleRule))
                return true;
        return false;
    }

    public boolean needReCreateWidgetView(StyleRule styleRule) {
        return needReCreateWidgetView(styleRule, getGWTWidget());
    }

    protected boolean needReCreateWidgetView(StyleRule styleRule, Widget widget) {
        return false;
    }

    public void updateStyleRule(StyleRule rule) { 
        if (rule == null) return;
        
        String prop = rule.getPropertyName();
        if (AbstractComponentEditPart.ENABLED_PROPERTY_NAME.equals(prop)) {
            if (rule instanceof BooleanRule)
                setEnabled((BooleanRule) rule);
            else
                setEnabled(null);
        } else if (AbstractComponentEditPart.BACKGROUND_COLOR_PROPERTY_NAME.equals(prop)) {
            if (rule instanceof ColorRule)
                setBackgroundColor((ColorRule) rule);
            else
                setBackgroundColor(null);
        } else if (AbstractComponentEditPart.FOREGROUND_COLOR_PROPERTY_NAME
                .equals(rule.getPropertyName())) {
            if (rule instanceof ColorRule)
                setForegroundColor((ColorRule) rule);
            else
                setForegroundColor(null);
        }
    }

    public void updateSameStyleRules(List<StyleRule> rules) { }

    /**
     * Where the children's WidgetViews should be attached to. In most of the cases, it returns the WidgetView itself.
     * 
     * @return A non null WidgetView
     */
    public Widget getContentPane() {
        return getGWTWidget();
    }

    public void addNotify() {
        assert getHost() != null;
        if (getGWTWidget() != null)
            // TODO ?
            ;//getFXNode().setData(WAZAABI_HOST_KEY, getHost());
    }

    protected boolean isValidationRoot() {
        return false;
    }

    public void revalidate() {
        invalidate();
        if (getParent() == null || isValidationRoot())
            ;//throw new RuntimeException("not implemented");//getUpdateManager().addInvalidFigure(this);
        else
            getParent().revalidate();
    }

    public void validate() { 
    }
    public void invalidate() {
    }
    public void removeNotify() {
    }
    public void setValid(boolean value) { 
    }
    
    protected void setBackgroundColor(ColorRule colorRule) {
        setBackgroundColor(getGWTWidget(), colorRule);
    }

    protected void setBackgroundColor(Widget widget, ColorRule colorRule) {
// TODO ?
//        org.eclipse.swt.graphics.RGB oldRGBValue = null;
//        org.eclipse.swt.graphics.RGB newRGBValue = null;
//
//        if (backgroundColor != null)
//            oldRGBValue = backgroundColor.getRGB();
//        if (colorRule != null)
//            newRGBValue = new org.eclipse.swt.graphics.RGB(colorRule.getRed(),
//                    colorRule.getGreen(), colorRule.getBlue());
//
//        if (oldRGBValue == null && newRGBValue == null)
//            return;
//        if (oldRGBValue != null && oldRGBValue.equals(newRGBValue))
//            return;
//
//        if (backgroundColor != null && !backgroundColor.isDisposed())
//            backgroundColor.dispose();
//
//        if (colorRule == null)
//            backgroundColor = null;
//        else
//            backgroundColor = new org.eclipse.swt.graphics.Color(
//                    getSWTControl().getDisplay(), newRGBValue);
//        control.setBackground(backgroundColor);
    }

    protected void setForegroundColor(ColorRule colorRule) {
        setForegroundColor(getGWTWidget(), colorRule);
    }

    protected void setForegroundColor(Widget widget, ColorRule colorRule) {
// TODO ?
//        org.eclipse.swt.graphics.RGB oldRGBValue = null;
//        org.eclipse.swt.graphics.RGB newRGBValue = null;
//
//        if (foregroundColor != null)
//            oldRGBValue = foregroundColor.getRGB();
//        if (colorRule != null)
//            newRGBValue = new org.eclipse.swt.graphics.RGB(colorRule.getRed(),
//                    colorRule.getGreen(), colorRule.getBlue());
//
//        if (oldRGBValue == null && newRGBValue == null)
//            return;
//        if (oldRGBValue != null && oldRGBValue.equals(newRGBValue))
//            return;
//
//        if (foregroundColor != null && !foregroundColor.isDisposed())
//            foregroundColor.dispose();
//
//        if (colorRule == null)
//            foregroundColor = null;
//        else
//            foregroundColor = new org.eclipse.swt.graphics.Color(
//                    getSWTControl().getDisplay(), newRGBValue);
//        control.setForeground(foregroundColor);
    }

    public void processPostControlCreation() { }

    protected void setEnabled(BooleanRule rule) {
        setEnabled(getGWTWidget(), rule);
    }

    protected void setEnabled(Widget widget, BooleanRule rule) {
//        if (rule != null) {
//            if (widget.isDisabled() == rule.isValue())
//                widget.setDisable(!rule.isValue());
//        } else if (node.isDisabled())
//            widget.setDisable(false);
    }
}
