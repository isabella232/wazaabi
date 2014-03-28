/*******************************************************************************
 * Copyright (c) 2014 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.propertysheets.tester;

import java.util.List;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.jface.window.ApplicationWindow;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.wazaabi.ide.propertysheets.TargetChangeListener;
import org.eclipse.wazaabi.ide.propertysheets.forms.viewers.FormBasedStyleRuleTableViewer;
import org.eclipse.wazaabi.mm.core.styles.BooleanRule;
import org.eclipse.wazaabi.mm.core.styles.ColorRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.StyledElement;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;
import org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;

public class TableTester extends ApplicationWindow implements
		TargetChangeListener {

	private FormBasedStyleRuleTableViewer viewer = null;

	public TableTester() {
		super(null);

		setBlockOnOpen(true);
		open();
		Display.getCurrent().dispose();
	}

	@Override
	protected Point getInitialSize() {
		return new Point(800, 300);
	}

	protected Control createContents(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		composite.setLayout(new FillLayout());

		viewer = new FormBasedStyleRuleTableViewer();
		viewer.createControls(composite);

		viewer.setInput(createStyledElement());
		viewer.addTargetChangeListener(this);
		return composite;
	}

	public static void main(String[] args) {
		new TableTester();
	}

	protected StyledElement createStyledElement() {
		CoreWidgetsPackage.eINSTANCE.getClass();

		// final PushButton control = CoreWidgetsFactory.eINSTANCE
		// .createPushButton();
		final Container control = CoreWidgetsFactory.eINSTANCE
				.createContainer();

		BooleanRule br = CoreStylesFactory.eINSTANCE.createBooleanRule();
		br.setPropertyName("border");
		br.setValue(true);
		br.eAdapters().add(new AdapterImpl() {

			@Override
			public void notifyChanged(Notification msg) {
				System.out.println(msg);
			}

		});
		control.getStyleRules().add(br);

		ColorRule cr = CoreStylesFactory.eINSTANCE.createColorRule();
		cr.setBlue(50);
		cr.setGreen(200);
		cr.setPropertyName("background-color");
		cr.eAdapters().add(new AdapterImpl() {

			@Override
			public void notifyChanged(Notification msg) {
				System.out.println(msg);
			}

		});
		control.getStyleRules().add(cr);

		StringRule sr = CoreStylesFactory.eINSTANCE.createStringRule();
		sr.setPropertyName("tooltip");
		sr.setValue("value one");

		sr.eAdapters().add(new AdapterImpl() {

			@Override
			public void notifyChanged(Notification msg) {
				System.out.println(msg);
			}

		});
		control.getStyleRules().add(sr);

		RowLayoutRule rl = SWTStylesFactory.eINSTANCE.createRowLayoutRule();
		rl.setPropertyName("layout");
		rl.setFill(true);
		control.getStyleRules().add(rl);

		return control;

	}

	@Override
	public void targetRemoved(EObject styledElement, EObject styleRule) {
		System.out
				.println("REMOVED : " + styleRule + " from: " + styledElement);
		((StyledElement) viewer.getInput()).getStyleRules().remove(styleRule);
		viewer.refresh();
	}

	@Override
	public void targetAdded(EObject styledElement, EObject styleRule,
			int position) {
		System.out.println("ADDED on [" + position + "] : " + styleRule
				+ " from: " + styledElement);
		if (position == -1)
			((StyledElement) viewer.getInput()).getStyleRules().add(
					(StyleRule) styleRule);
		else
			((StyledElement) viewer.getInput()).getStyleRules().add(position,
					(StyleRule) styleRule);
		viewer.refresh();
	}

	@Override
	public void targetModified(EObject styleRule, EStructuralFeature feature,
			int position, Object oldValue, Object newValue) {
		System.out.println("MODIFIED " + oldValue + ", " + newValue);
		styleRule.eSet(feature, newValue);
	}

	@Override
	public void targetMultipleModified(EObject styleRule,
			List<EStructuralFeature> features, List<Integer> positions,
			List<Object> oldValues, List<Object> newValues) {

		System.out.print("MODIFIED ");
		for (int i = 0; i < features.size(); i++)
			System.out.println(features.get(i).getName() + ": "
					+ oldValues.get(i) + ", " + newValues.get(i));

		for (int i = 0; i < features.size(); i++)
			styleRule.eSet(features.get(i), newValues.get(i));

	}

	@Override
	public void undo() {
		System.out.println("undo");
	}

	@Override
	public void redo() {
		System.out.println("redo");
	}
}
