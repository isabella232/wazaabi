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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.gef.EditPolicy;
import org.eclipse.wazaabi.mm.core.styles.LayoutDataRule;
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.StyledElement;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.wazaabi.ide.ui.editors.viewer.ExtendedTreeViewer;
import org.eclipse.wazaabi.ide.ui.editparts.stylerule.managers.StringRuleManager;
import org.eclipse.wazaabi.ide.ui.editpolicies.ComponentEditPolicy;
import org.eclipse.wazaabi.ide.ui.editpolicies.TreeContainerEditPolicy;
import org.eclipse.wazaabi.ide.ui.editpolicies.TreeEditPolicy;
import org.eclipse.wazaabi.ide.ui.internal.Activator;

public class AbstractComponentTreeEditPart extends AbstractTreeEditPart {

    public static class StyleRuleManager extends AdapterImpl {

        private AbstractComponentTreeEditPart host = null;

        protected AbstractComponentTreeEditPart getHost() {
            return host;
        }

        @Override
        public void notifyChanged(Notification notification) {
            assert getHost() != null;
            if (notification.getEventType() != Notification.SET)
                return;
            switch (notification.getFeatureID(StyleRule.class)) {
            case CoreStylesPackage.STYLE_RULE__PROPERTY_NAME:
                String oldPropertyName = notification.getOldStringValue();
                String newPropertyName = notification.getNewStringValue();
                StyleRule previousStyleRule = null;
                if (oldPropertyName != null && !"".equals(oldPropertyName)) //$NON-NLS-1$
                    if (!oldPropertyName.equals(newPropertyName)) {
                        // we duplicate the previous styleRule and set the
                        // propertyName
                        previousStyleRule = (StyleRule) EcoreUtil
                                .copy((EObject) notification.getNotifier());
                        previousStyleRule.setPropertyName(oldPropertyName);
                        getHost().styleRuleRemoved(previousStyleRule);
                    }
                if (newPropertyName != null && !"".equals(newPropertyName)) //$NON-NLS-1$
                    getHost().styleRuleAdded(
                            (StyleRule) notification.getNotifier());
                break;
            }
        }

        protected void setHost(AbstractComponentTreeEditPart host) {
            this.host = host;
        }
    }

    /**
     * Creates and installs pertinent EditPolicies for this.
     */
    protected void createEditPolicies() {
        installEditPolicy(EditPolicy.COMPONENT_ROLE, new ComponentEditPolicy());
        installEditPolicy(EditPolicy.PRIMARY_DRAG_ROLE, new TreeEditPolicy());
        installEditPolicy(EditPolicy.TREE_CONTAINER_ROLE,
                new TreeContainerEditPolicy());
    }

    public AbstractComponent getAbstractComponentModel() {
        return (AbstractComponent) getModel();
    }

    protected String getLabel() {
        return getAbstractComponentModel().eClass().getName();
    }

    protected String getExtendedInfo() {
        return null;
    }

    /**
     * Returns <code>null</code> as a Tree EditPart holds no children under it.
     * 
     * @return <code>null</code>
     */
    protected List<?> getModelChildren() {
        List<EObject> children = new ArrayList<EObject>();
        children.addAll(getModelEventHandlers());
        children.addAll(getModelDataLayoutRules());
        return children;
    }

    protected List<LayoutDataRule> getModelDataLayoutRules() {
        List<LayoutDataRule> children = new ArrayList<LayoutDataRule>();
        if (((ExtendedTreeViewer) getViewer()).isDisplayingLayoutInfo())
            for (StyleRule rule : getAbstractComponentModel().getStyleRules())
                if (rule instanceof LayoutDataRule)
                    children.add((LayoutDataRule) rule);
        return children;
    }

    protected List<EventHandler> getModelEventHandlers() {
        List<EventHandler> children = new ArrayList<EventHandler>();
        // if (((ExtendedTreeViewer) getViewer()).isDisplayingLayoutInfo())
        for (EventHandler eventHandler : getAbstractComponentModel()
                .getHandlers())
            children.add(eventHandler);
        return children;
    }

    protected void hookModel() {
        super.hookModel();
        for (StyleRule styleRule : ((StyledElement) getModel()).getStyleRules())
            hookStyleRule(styleRule);
    }

    /**
     * Finds the StyleRuleManager corresponding to the given StyleRule and if
     * found, attaches it and sets initialization parameters.
     * 
     * @param styleRule
     */
    protected void hookStyleRule(StyleRule styleRule) {
        StyleRuleManager manager = null;
        if (styleRule instanceof StringRule)
            manager = new StringRuleManager();
        if (manager != null) {
            manager.setHost(this);
            // create a style rule adapter and attach it to the style rule model
            styleRule.eAdapters().add(manager);
        }
    }

    @SuppressWarnings("unchecked")
    public void notifyChanged(Notification notification) {
        switch (notification
                .getFeatureID(org.eclipse.wazaabi.mm.core.widgets.AbstractComponent.class)) {
        case CoreWidgetsPackage.ABSTRACT_COMPONENT__STYLE_RULES:
            switch (notification.getEventType()) {
            case Notification.ADD:
                hookStyleRule((StyleRule) notification.getNewValue());
                styleRuleAdded((StyleRule) notification.getNewValue());
                break;
            case Notification.ADD_MANY:
                for (StyleRule rule : (List<StyleRule>) notification
                        .getNewValue()) {
                    hookStyleRule(rule);
                    styleRuleAdded(rule);
                }
                break;
            case Notification.REMOVE:
                unhookStyleRule((StyleRule) notification.getOldValue());
                styleRuleRemoved((StyleRule) notification.getOldValue());
                break;
            case Notification.REMOVE_MANY:
                for (StyleRule rule : (List<StyleRule>) notification
                        .getOldValue()) {
                    styleRuleRemoved(rule);
                    unhookStyleRule(rule);
                }
                break;
            case Notification.MOVE:
                // TODO
                break;
            }
            refresh();
            break;
        default:
            super.notifyChanged(notification);
        }
    }

    /**
     * Refreshes the visual properties of the TreeItem for this part.
     */
    protected void refreshVisuals() {
        if (getWidget() instanceof Tree)
            return;
        Image image = Activator.getDefault().getImageRegistry()
                .get(getAbstractComponentModel().eClass().getName());
        TreeItem item = (TreeItem) getWidget();
        if (image != null)
            image.setBackground(item.getParent().getBackground());
        setWidgetImage(image);
        setWidgetText(getLabel(), 0);
//        String extendedInfo = getExtendedInfo();
//        setWidgetText(extendedInfo != null ? extendedInfo : "", 1);
    }

    public void styleRuleAdded(StyleRule newRule) {
    }

    public void styleRuleRemoved(StyleRule oldRule) {
    }

    public void styleRuleUpdated(StyleRule rule) {
    }

    protected void unhookModel() {
        for (StyleRule styleRule : ((StyledElement) getModel()).getStyleRules())
            unhookStyleRule(styleRule);
        super.unhookModel();
    }

    /**
     * Detaches/removes all the StyleRuleManagers attached to the given style
     * rule. Actually, only one StyleRuleManager is supposed to be attached to a
     * StyleRule, but we prefer being sure.
     * 
     * @param styleRule
     */
    protected void unhookStyleRule(StyleRule styleRule) {
        List<Adapter> toRemove = new ArrayList<Adapter>(2);
        for (Adapter adapter : styleRule.eAdapters())
            if (adapter instanceof StyleRuleManager)
                toRemove.add(adapter);
        for (Adapter adapter : toRemove)
            styleRule.eAdapters().remove(adapter);
    }

    public void measureWidget(Event event) {

    }

    public void eraseWidget(Event event) {

    }

    public void paintWidget(Event event) {
    }
}