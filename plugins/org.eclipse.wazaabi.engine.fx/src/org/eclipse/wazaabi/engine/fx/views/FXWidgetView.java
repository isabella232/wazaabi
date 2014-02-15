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

import java.util.HashMap;
import java.util.List;

import javafx.scene.Node;
import javafx.scene.layout.Pane;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.wazaabi.engine.core.editparts.AbstractComponentEditPart;
import org.eclipse.wazaabi.engine.core.editparts.WidgetEditPart;
import org.eclipse.wazaabi.engine.core.editparts.WidgetViewListener;
import org.eclipse.wazaabi.engine.core.editparts.stylerules.StylePropertyDescriptor;
import org.eclipse.wazaabi.engine.core.editparts.stylerules.StyleRulesHelper;
import org.eclipse.wazaabi.engine.core.gef.editparts.ListenerList;
import org.eclipse.wazaabi.engine.core.stylerules.factories.StyleRuleManagerFactory;
import org.eclipse.wazaabi.engine.core.views.AbstractComponentView;
import org.eclipse.wazaabi.engine.core.views.WidgetView;
import org.eclipse.wazaabi.engine.fx.viewers.FXViewer;
import org.eclipse.wazaabi.engine.fx.views.updman.UpdateManager;
import org.eclipse.wazaabi.mm.core.styles.BooleanRule;
import org.eclipse.wazaabi.mm.core.styles.ColorRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public abstract class FXWidgetView implements AbstractComponentView {

    public static final String WAZAABI_HOST_KEY = "org.eclipse.wazaabi.engine.fx.DATA_KEY";
    
    private static final Logger log = LoggerFactory.getLogger(FXWidgetView.class);
    private static HashMap<String, StylePropertyDescriptor> platformSpecificStylePropertyDescriptors;

    private WidgetEditPart host;
    private final ListenerList listenerList = new ListenerList();
    private Node node;
//    private Color foregroundColor;
//    private Color backgroundColor;


    public FXWidgetView() {
        initPlatformPropertyDescriptors();
    }

    // FIXME protected method must not be called from constructors
    protected void initPlatformPropertyDescriptors() {
        if (platformSpecificStylePropertyDescriptors == null) {
            platformSpecificStylePropertyDescriptors = new HashMap<String, StylePropertyDescriptor>();
            StyleRulesHelper.buildPlatformSpecificStylePropertyDescritors(
                    getWidgetViewEClass(), platformSpecificStylePropertyDescriptors);
        }
    }

    public HashMap<String, StylePropertyDescriptor> getPlatformSpecificStylePropertyDescriptors() {
        return platformSpecificStylePropertyDescriptors;
    }


    public void setHost(WidgetEditPart host) { this.host = host; }
    public WidgetEditPart getHost() { return host; }

    public FXWidgetView getParent() {
        if (getHost().getParent() instanceof AbstractComponentEditPart)
            return (FXWidgetView) ((AbstractComponentEditPart) getHost().getParent()).getWidgetView();
        return null;
    }

//    private final DisposeListener disposeListener = new DisposeListener() {
//        public void widgetDisposed(DisposeEvent e) {
//            log.debug("SWT DisposeEvent called on \"{}\" ", e.widget);
//            getHost().deactivate();
//            FXWidgetView.this.widgetDisposed();
//            if (getHost().getParent() instanceof RootEditPart)
//                getHost().getViewer().dispose();
//        }
//    };


    public void add(WidgetView childView, int index) {
        if (!(childView instanceof FXWidgetView))
            throw new RuntimeException("Invalid parent WidgetView");
        Node newNode = ((FXWidgetView) childView).createFXNode((Pane) getFXNode(), index);
        if (newNode == null)
            throw new RuntimeException("Unable to create FX node");

        ((FXWidgetView) childView).node = newNode;
        // TODO ?
        //newNode.addDisposeListener(((FXWidgetView) childView).disposeListener);
    }

    public void remove(WidgetView view) {
        if (view instanceof FXWidgetView && ((FXWidgetView) view).getFXNode() != null)
            // TODO ?
            ;//((FXWidgetView) view).getSWTWidget().dispose();
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


    
//    protected int computeSWTCreationStyle(StyleRule rule) { return SWT.NONE; }
//
//    protected int computeSWTCreationStyle(WidgetEditPart editPart) {
//        int style = SWT.NONE;
//        List<String> processedStyles = new ArrayList<String>();
//        for (StyleRule rule : ((StyledElement) getHost().getModel()).getStyleRules())
//            if (!processedStyles.contains(rule.getPropertyName())) {
//                processedStyles.add(rule.getPropertyName());
//                style |= computeSWTCreationStyle(rule);
//            }
//        return style;
//    }

    protected abstract Node createFXNode(Pane parent, int index);


    public Node getFXNode() {
        return node;
    }

    public abstract EClass getWidgetViewEClass();


    public boolean needReCreateWidgetView(List<StyleRule> styleRules) {
        for (StyleRule styleRule : styleRules)
            if (needReCreateWidgetView(styleRule))
                return true;
        return false;
    }

    public boolean needReCreateWidgetView(StyleRule styleRule) {
        return needReCreateWidgetView(styleRule, getFXNode());
    }

    protected boolean needReCreateWidgetView(StyleRule styleRule, Node node) {
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

//    protected void widgetDisposed() {
//        for (Object l : listenerList.getListeners())
//            ((WidgetViewListener) l).viewChanged(this, WidgetViewListener.VIEW_DISPOSED);
//        if (backgroundColor != null && !backgroundColor.isDisposed())
//            backgroundColor.dispose();
//        if (foregroundColor != null && !foregroundColor.isDisposed())
//            foregroundColor.dispose();
//    }

    /**
     * Where the children's WidgetViews should be attached to. In most of the cases, it returns the WidgetView itself.
     * 
     * @return A non null WidgetView
     */
    public Node getContentPane() {
        return getFXNode();
    }


//    protected static boolean isStyleBitCorrectlySet(org.eclipse.swt.widgets.Widget widget, int styleBitMask, 
//            boolean newStyleBitValue) 
//    {
//        int styleValue = widget.getStyle();
//        if (newStyleBitValue && (styleValue & styleBitMask) == 0) {
//            styleValue |= styleBitMask;
//        } else if (!newStyleBitValue && (styleValue & styleBitMask) != 0) {
//            styleValue ^= styleBitMask;
//        }
//        return styleValue == widget.getStyle();
//    }

    
    public void addNotify() {
        assert getHost() != null;
        if (getFXNode() != null)
            // TODO ?
            ;//getFXNode().setData(WAZAABI_HOST_KEY, getHost());
    }

    protected boolean isValidationRoot() {
        return false;
    }

    public UpdateManager getUpdateManager() {
        return ((FXViewer) getHost().getViewer()).getUpdateManager();
    }

    public void revalidate() {
        invalidate();
        if (getParent() == null || isValidationRoot())
            getUpdateManager().addInvalidFigure(this);
        else
            getParent().revalidate();
    }

    public void validate() { }
    public void invalidate() { }
    public void removeNotify() { }
    public void setValid(boolean value) { }

    protected void setBackgroundColor(ColorRule colorRule) {
        setBackgroundColor(getFXNode(), colorRule);
    }

    protected void setBackgroundColor(Node node, ColorRule colorRule) {
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
        setForegroundColor(getFXNode(), colorRule);
    }

    protected void setForegroundColor(Node node, ColorRule colorRule) {
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

    protected void platformSpecificRefreshStyleRule(Object context,
            StyleRule rule) {
        StyleRuleManagerFactory factory = (StyleRuleManagerFactory) getHost()
                .getViewer().getFactoryFor(context, rule, null,
                        StyleRuleManagerFactory.class);
        if (factory != null)
            factory.platformSpecificRefresh(context, rule);
    }

    public void processPostControlCreation() {

    }

    protected void setEnabled(BooleanRule rule) {
        setEnabled(getFXNode(), rule);
    }

    protected void setEnabled(Node node, BooleanRule rule) {
        if (rule != null) {
            if (node.isDisabled() == rule.isValue())
                node.setDisable(!rule.isValue());
        } else if (node.isDisabled())
            node.setDisable(false);
    }
}
