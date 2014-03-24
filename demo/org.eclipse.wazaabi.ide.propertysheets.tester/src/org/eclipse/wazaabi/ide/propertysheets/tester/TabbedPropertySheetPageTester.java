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

import java.util.ArrayList;
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
import org.eclipse.wazaabi.ide.propertysheets.PropertySection;
import org.eclipse.wazaabi.ide.propertysheets.TargetChangeListener;
import org.eclipse.wazaabi.ide.propertysheets.forms.viewers.FormBasedEventHandlerViewer;
import org.eclipse.wazaabi.ide.propertysheets.forms.viewers.FormBasedPropertyTableViewer;
import org.eclipse.wazaabi.ide.propertysheets.forms.viewers.FormBasedStyleRuleTableViewer;
import org.eclipse.wazaabi.ide.propertysheets.tabbed.TabbedPropertySheetPage;
import org.eclipse.wazaabi.mm.core.Orientation;
import org.eclipse.wazaabi.mm.core.styles.BooleanRule;
import org.eclipse.wazaabi.mm.core.styles.BoxLayoutRule;
import org.eclipse.wazaabi.mm.core.styles.ColorRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.core.widgets.Collection;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;
import org.eclipse.wazaabi.mm.edp.events.EDPEventsFactory;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.events.PropertyChangedEvent;
import org.eclipse.wazaabi.mm.edp.handlers.Binding;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersFactory;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.wazaabi.mm.edp.handlers.Parameter;
import org.eclipse.wazaabi.mm.edp.handlers.Parameterized;
import org.eclipse.wazaabi.mm.edp.handlers.StringParameter;
import org.eclipse.wazaabi.mm.swt.styles.GridDataRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;

public class TabbedPropertySheetPageTester extends ApplicationWindow implements
		TargetChangeListener {

	private TabbedPropertySheetPage page = null;
	private AbstractComponent input = null;

	public TabbedPropertySheetPageTester() {
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
		page = new TabbedPropertySheetPage() {

			@Override
			protected List<PropertySection> createPropertySections(Object input) {
				List<PropertySection> result = new ArrayList<PropertySection>();
				if (input instanceof AbstractComponent) {
					result.add(new FormBasedPropertyTableViewer());
					result.add(new FormBasedStyleRuleTableViewer());
					result.add(new FormBasedEventHandlerViewer());
				}
				return result;
			}

			@Override
			protected boolean needRecreatePropertySections(Object input,
					List<PropertySection> propertySections) {
				if (input instanceof AbstractComponent)
					return propertySections.size() != 3
							|| (propertySections.size() == 3 && !(propertySections
									.get(0) instanceof FormBasedPropertyTableViewer
									&& propertySections.get(1) instanceof FormBasedStyleRuleTableViewer && propertySections
										.get(2) instanceof FormBasedEventHandlerViewer));
				return true;
			}

		};
		page.createControl(composite);
		Object input = getInput();
		page.buildUI(input);
		page.setInput(input);
		page.addTargetChangeListener(this);
		page.selectTab(1);
		return composite;
	}

	public static void main(String[] args) {
		new TabbedPropertySheetPageTester();
	}

	protected AbstractComponent getInput() {
		if (input == null)
			input = createInput();
		return input;
	}

	protected AbstractComponent createInput() {

		final Collection collection = CoreWidgetsFactory.eINSTANCE
				.createCollection();

		return collection;
	}

	protected AbstractComponent createInput1() {
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

		// RowLayoutRule rl = SWTStylesFactory.eINSTANCE.createRowLayoutRule();
		BoxLayoutRule rl = CoreStylesFactory.eINSTANCE.createBoxLayoutRule();
		rl.setPropertyName("layout");
		rl.setOrientation(Orientation.VERTICAL);
		control.getStyleRules().add(rl);

		GridDataRule gdr = SWTStylesFactory.eINSTANCE.createGridDataRule();
		gdr.setPropertyName("layout-data");
		control.getStyleRules().add(gdr);

		EventHandler eh = EDPHandlersFactory.eINSTANCE.createEventHandler();
		// control.getHandlers().add(eh);
		Event ev = EDPEventsFactory.eINSTANCE.createEvent();
		ev.setId("core:ui:refresh");
		eh.getEvents().add(ev);
		eh.setUri("hello/world");

		Binding binding = EDPHandlersFactory.eINSTANCE.createBinding();
		control.getHandlers().add(binding);
		Event ev2 = EDPEventsFactory.eINSTANCE.createEvent();
		ev2.setId("core:ui:refresh");
		PropertyChangedEvent ev3 = EDPEventsFactory.eINSTANCE
				.createPropertyChangedEvent();
		ev3.setPath("$input/@name");
		binding.getEvents().add(ev2);
		binding.getEvents().add(ev3);
		StringParameter source = EDPHandlersFactory.eINSTANCE
				.createStringParameter();
		StringParameter target = EDPHandlersFactory.eINSTANCE
				.createStringParameter();
		source.setName("source");
		source.setValue("@text");
		target.setName("target");
		target.setValue("${value}");
		binding.getParameters().add(source);
		binding.getParameters().add(target);

		return control;

	}

	@Override
	public void targetRemoved(EObject container, EObject target) {
		System.out.println("REMOVED : " + target);
		if (target instanceof StyleRule)
			input.getStyleRules().remove(target);
		else if (target instanceof EventHandler)
			input.getHandlers().remove(target);
		else if (target instanceof Event)
			((EventHandler) container).getEvents().remove(target);
		else if (container instanceof Parameterized
				&& target instanceof Parameter)
			((Parameterized) container).getParameters().remove(target);

		page.refresh();
	}

	@Override
	public void targetAdded(EObject container, EObject target, int position) {
		System.out.println("ADDED on [" + position + "] : " + target);
		if (target instanceof StyleRule)
			if (position == -1)
				input.getStyleRules().add((StyleRule) target);
			else
				input.getStyleRules().add(position, (StyleRule) target);
		else if (target instanceof EventHandler)
			if (position == -1)
				input.getHandlers().add((EventHandler) target);
			else
				input.getHandlers().add(position, (EventHandler) target);
		else if (target instanceof Event && container instanceof EventHandler)
			if (position == -1)
				((EventHandler) container).getEvents().add((Event) target);
			else
				((EventHandler) container).getEvents().add(position,
						(Event) target);
		else if (container instanceof Parameterized
				&& target instanceof Parameter)
			((Parameterized) container).getParameters().add((Parameter) target);

		page.refresh();
	}

	@Override
	public void targetModified(EObject row, EStructuralFeature feature,
			int position, Object oldValue, Object newValue) {
		System.out.println("MODIFIED " + oldValue + ", " + newValue);
		row.eSet(feature, newValue);
	}

	@Override
	public void targetMultipleModified(EObject row,
			List<EStructuralFeature> features, List<Integer> positions,
			List<Object> oldValues, List<Object> newValues) {

		System.out.print("MODIFIED ");
		for (int i = 0; i < features.size(); i++)
			System.out.println(features.get(i).getName() + ": "
					+ oldValues.get(i) + ", " + newValues.get(i));

		for (int i = 0; i < features.size(); i++)
			row.eSet(features.get(i), newValues.get(i));

	}
}