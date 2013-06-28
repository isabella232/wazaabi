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

import java.util.SortedMap;
import java.util.TreeMap;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.CoolBar;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.ExpandBar;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.wazaabi.engine.core.editparts.AbstractComponentEditPart;
import org.eclipse.wazaabi.engine.core.editparts.ContainerEditPart;
import org.eclipse.wazaabi.engine.core.gef.EditPart;
import org.eclipse.wazaabi.engine.core.views.AbstractComponentView;
import org.eclipse.wazaabi.engine.core.views.ContainerView;
import org.eclipse.wazaabi.engine.core.views.WidgetView;
import org.eclipse.wazaabi.engine.swt.commons.editparts.stylerules.managers.StackLayoutStyleRuleManager;
import org.eclipse.wazaabi.mm.core.Orientation;
import org.eclipse.wazaabi.mm.core.Position;
import org.eclipse.wazaabi.mm.core.styles.BarLayoutRule;
import org.eclipse.wazaabi.mm.core.styles.BlankRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.core.styles.ExpandLayoutRule;
import org.eclipse.wazaabi.mm.core.styles.LayoutRule;
import org.eclipse.wazaabi.mm.core.styles.SashFormLayoutRule;
import org.eclipse.wazaabi.mm.core.styles.StackLayoutRule;
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.StyledElement;
import org.eclipse.wazaabi.mm.core.styles.TabbedLayoutRule;
import org.eclipse.wazaabi.mm.core.styles.impl.BarLayoutRuleImpl;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.swt.descriptors.SWTDescriptorsPackage;

public class SWTContainerView extends SWTControlView implements ContainerView {

	public static final String GROUP_STYLE = "group"; //$NON-NLS-1$

	public EClass getWidgetViewEClass() {
		return SWTDescriptorsPackage.Literals.COMPOSITE;
	}

	protected Widget createSWTWidget(Widget parent, int swtStyle, int index) {
		for (StyleRule rule : ((StyledElement) getHost().getModel())
				.getStyleRules()) {
			if (rule instanceof BarLayoutRuleImpl
					&& ContainerEditPart.LAYOUT_PROPERTY_NAME.equals(rule
							.getPropertyName())) {
				if (((BarLayoutRule) rule).isDraggable()) {
					// If the elements are draggable, then we need a coolbar
					CoolBar bar = new CoolBar(
							(org.eclipse.swt.widgets.Composite) parent,
							computeSWTCreationStyle(getHost()));
					bar.setLocked(false);
					bar.addListener(SWT.Resize, new Listener() {
						public void handleEvent(Event event) {
							Composite parent = (Composite) ((CoolBar) event.widget)
									.getParent();
							if (parent != null)
								parent.layout();
						}
					});
					return bar;
				} else {
					// If the elements are not draggable, we need a toolbar
					return new ToolBar(
							(org.eclipse.swt.widgets.Composite) parent,
							computeSWTCreationStyle(getHost()));
				}
			} else if (rule instanceof TabbedLayoutRule
					&& ContainerEditPart.LAYOUT_PROPERTY_NAME.equals(rule
							.getPropertyName())) {
				CTabFolder folder = new CTabFolder(
						(org.eclipse.swt.widgets.Composite) parent,
						computeSWTCreationStyle(getHost()));
				folder.setMaximizeVisible(((TabbedLayoutRule) rule)
						.isMaximizeVisible());
				folder.setMinimizeVisible(((TabbedLayoutRule) rule)
						.isMinimizeVisible());
				folder.marginWidth = ((TabbedLayoutRule) rule).getMarginWidth();
				folder.marginHeight = ((TabbedLayoutRule) rule)
						.getMarginHeight();
				return folder;
			} else if (rule instanceof ExpandLayoutRule
					&& ContainerEditPart.LAYOUT_PROPERTY_NAME.equals(rule
							.getPropertyName())) {
				return createExpandBar(
						(org.eclipse.swt.widgets.Composite) parent,
						computeSWTCreationStyle(getHost()) | SWT.V_SCROLL);
			} else if (rule instanceof SashFormLayoutRule
					&& ContainerEditPart.LAYOUT_PROPERTY_NAME.equals(rule
							.getPropertyName())) {
				SashForm sashForm = new SashForm(
						(org.eclipse.swt.widgets.Composite) parent,
						computeSWTCreationStyle(getHost()));
				return sashForm;
			} else if (rule instanceof StringRule
					&& AbstractComponentEditPart.LOOK_AND_FEEL.equals(rule
							.getPropertyName())) {
				String laf = ((StringRule) rule).getValue();
				if (GROUP_STYLE.equals(laf))
					return new Group(
							(org.eclipse.swt.widgets.Composite) parent,
							computeSWTCreationStyle(getHost()));
			}
		}
		return wrapForSpecificParent(
				(Composite) parent,
				createComposite((org.eclipse.swt.widgets.Composite) parent,
						computeSWTCreationStyle(getHost())));
	}

	protected Composite createComposite(Composite parent, int style) {
		return new org.eclipse.swt.widgets.Composite(parent, style);
	}

	protected Widget createExpandBar(Composite parent, int style) {
		return new ExpandBar(parent, style);
	}

	private LayoutRule currentLayoutRule = null;

	protected void setLayout(LayoutRule rule) {
		if (!(rule instanceof BlankRule))
			currentLayoutRule = rule;
		else
			currentLayoutRule = (LayoutRule) ((StyledElement) getHost()
					.getModel()).getFirstStyleRule(
					ContainerEditPart.LAYOUT_PROPERTY_NAME,
					CoreStylesPackage.Literals.LAYOUT_RULE);

		if (currentLayoutRule != null)
			platformSpecificRefreshStyleRule(this, currentLayoutRule);
		else
			((Composite) getContentPane()).setLayout(null);
		revalidate();
	}

	@Override
	protected int computeSWTCreationStyle(StyleRule rule) {
		if (ContainerEditPart.LAYOUT_PROPERTY_NAME.equals(rule
				.getPropertyName()) && rule instanceof SashFormLayoutRule) {
			if (((SashFormLayoutRule) rule).getOrientation() == Orientation.VERTICAL)
				return SWT.VERTICAL;
			return SWT.HORIZONTAL;
		} else if (ContainerEditPart.LAYOUT_PROPERTY_NAME.equals(rule
				.getPropertyName()) && rule instanceof TabbedLayoutRule)
			return ((TabbedLayoutRule) rule).getPosition() == Position.TOP ? SWT.TOP
					: SWT.BOTTOM;
		return super.computeSWTCreationStyle(rule);
	}

	@Override
	public void updateStyleRule(StyleRule rule) {
		if (rule == null)
			return;
		if (ContainerEditPart.LAYOUT_PROPERTY_NAME.equals(rule
				.getPropertyName())) {
			if (rule instanceof LayoutRule) {
				setLayout((LayoutRule) rule);
			} else
				setLayout(null);
		} else if (ContainerEditPart.HEADER_TITLE
				.equals(rule.getPropertyName()) && rule instanceof StringRule)
			setHeaderTitle((StringRule) rule);
		else
			super.updateStyleRule(rule);
	}

	protected void setHeaderTitle(StringRule rule) {
		if (rule != null && getSWTWidget() instanceof Group)
			((Group) getSWTWidget()).setText(rule.getValue() != null ? rule
					.getValue() : ""); //$NON-NLS-1$
	}

	@Override
	protected boolean needReCreateWidgetView(StyleRule styleRule,
			org.eclipse.swt.widgets.Widget widget) {
		if (styleRule == null)
			return false;
		if (ContainerEditPart.LAYOUT_PROPERTY_NAME.equals(styleRule
				.getPropertyName())) {
			if (styleRule instanceof BarLayoutRule)
				return !(widget instanceof ToolBar)
						&& !(widget instanceof CoolBar);
			else if (styleRule instanceof TabbedLayoutRule)
				return !(widget instanceof CTabFolder);
			else if (styleRule instanceof ExpandLayoutRule)
				return !(widget instanceof ExpandBar);
			else if (styleRule instanceof SashFormLayoutRule) {
				if (widget instanceof SashForm)
					return !(isStyleBitCorrectlySet(
							widget,
							org.eclipse.swt.SWT.HORIZONTAL,
							Orientation.HORIZONTAL == ((SashFormLayoutRule) styleRule)
									.getOrientation()) & isStyleBitCorrectlySet(
							widget,
							org.eclipse.swt.SWT.VERTICAL,
							Orientation.VERTICAL == ((SashFormLayoutRule) styleRule)
									.getOrientation()));
				else
					return false;
			} else
				return false;
		} else if (AbstractComponentEditPart.LOOK_AND_FEEL.equals(styleRule
				.getPropertyName())) {
			if (styleRule instanceof StringRule)
				if (GROUP_STYLE.equals(((StringRule) styleRule).getValue()))
					return !(getSWTWidget() instanceof Group);
				else
					return true;
			else if (styleRule instanceof BlankRule)
				return getSWTWidget() instanceof Group;
			return false;

		} else
			return super.needReCreateWidgetView(styleRule, widget);
	}

	/**
	 * Indicates that this SWTComposite should make itself valid. Validation
	 * includes invoking layout on a LayoutManager if present, and then
	 * validating all children figures. Default validation uses pre-order,
	 * depth-first ordering.
	 */

	@Override
	public void add(WidgetView view, int index) {
		// first we create the widget
		super.add(view, index);
		if (index != ((Composite) getContentPane()).getChildren().length - 1)
			if (view instanceof SWTControlView)
				reorderChild((SWTControlView) view, index);
	}

	public void reorderChild(AbstractComponentView child, int index) {

		if (!(((SWTWidgetView) child).getSWTWidget() instanceof org.eclipse.swt.widgets.Control)
				|| ((SWTWidgetView) child).getSWTWidget().isDisposed())
			return;

		// get the SWT Control child
		final org.eclipse.swt.widgets.Control childControl = (org.eclipse.swt.widgets.Control) ((SWTWidgetView) child)
				.getSWTWidget();
		// get the SWT Composite (this)
		final org.eclipse.swt.widgets.Composite composite = (Composite) getContentPane();

		EditPart parentModel = (EditPart) getHost();
		if (parentModel instanceof ContainerEditPart
				&& parentModel.getModel() != null) {
			StyleRule parentLayoutRule = ((StyledElement) parentModel
					.getModel()).getFirstStyleRule(
					ContainerEditPart.LAYOUT_PROPERTY_NAME,
					CoreStylesPackage.Literals.SASH_FORM_LAYOUT_RULE);
			if (parentLayoutRule != null) {
				return;
			}
		}

		if (childControl.getParent() != composite)
			return;
		int oldIndex = -1;
		for (int i = 0; i < composite.getChildren().length; i++)
			if (composite.getChildren()[i] == childControl) {
				oldIndex = i;
				break;
			}
		if (index == oldIndex)
			return;

		if (oldIndex < index)
			childControl.moveBelow(composite.getChildren()[index]);
		else
			childControl.moveAbove(composite.getChildren()[index]);

	}

	@Override
	public void validate() {
		if (currentLayoutRule instanceof StackLayoutRule)
			StackLayoutStyleRuleManager.platformSpecificRefresh(this,
					(StackLayoutRule) currentLayoutRule);
		super.validate();
	}

	public void refreshTabIndexes() {
		if (getSWTWidget().isDisposed())
			return;
		SortedMap<Integer, Control> tabList = null;
		for (EditPart child : getHost().getChildren()) {
			if (child.getModel() instanceof AbstractComponent
					&& ((AbstractComponentEditPart) child).getWidgetView() instanceof SWTWidgetView) {
				int index = ((AbstractComponent) child.getModel())
						.getTabIndex();
				if (index != -1) {
					if (tabList == null)
						tabList = new TreeMap<Integer, Control>();
					tabList.put(
							index,
							(Control) ((SWTWidgetView) ((AbstractComponentEditPart) child)
									.getWidgetView()).getSWTWidget());
				}
			}
		}
		if (tabList != null)
			((Composite) getContentPane()).setTabList((Control[]) tabList
					.values().toArray(new Control[] {}));
	}

	protected void validateContent() {
		final Composite composite = (Composite) getContentPane();
		if (composite.isDisposed())
			return;
		org.eclipse.swt.widgets.Control[] children = composite.getChildren();

		composite.layout();

		for (int i = 0; i < children.length; i++)
			if (children[i].getData(WAZAABI_HOST_KEY) instanceof AbstractComponentEditPart)
				((WidgetView) ((AbstractComponentEditPart) children[i]
						.getData(WAZAABI_HOST_KEY)).getWidgetView()).validate();
	}

}
