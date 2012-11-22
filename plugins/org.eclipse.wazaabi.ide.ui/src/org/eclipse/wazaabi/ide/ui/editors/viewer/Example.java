/*******************************************************************************
 * Copyright (c) 2012 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.ui.editors.viewer;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;
import org.eclipse.wazaabi.mm.core.widgets.Label;
import org.eclipse.wazaabi.mm.core.widgets.TextComponent;
import org.eclipse.wazaabi.mm.edp.events.EDPEventsFactory;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.events.PropertyChangedEvent;
import org.eclipse.wazaabi.mm.edp.handlers.Binding;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersFactory;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.wazaabi.mm.edp.handlers.StringParameter;
import org.eclipse.wazaabi.mm.swt.styles.GridDataRule;
import org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;

public class Example {
	private static FFactory ff = null;
	static {
		ff = new FFactory();
		ff.registerContainingInstance(new Example());
	}

	@EAttributeMappingRule(datatype = "EString", target = TextComponent.class, droppedType = EventHandler.class)
	public List<EventHandler> getEStringOnTextComponentEventHandlers(
			TextComponent target, int index, EAttribute source, Object context) {
		List<EventHandler> eventHandlers = new ArrayList<EventHandler>();
		Binding binding = EDPHandlersFactory.eINSTANCE.createBinding();
		eventHandlers.add(binding);
		return eventHandlers;
	}

	@SuppressWarnings("unchecked")
	@EClassMappingRule(target = Container.class, droppedType = AbstractComponent.class)
	public List<AbstractComponent> getClassOnContainerComponents(
			Container target, int index, EClass source, Object context) {
		GridLayoutRule gridLayoutRule = SWTStylesFactory.eINSTANCE
				.createGridLayoutRule();
		// target.getStyleRules().add(gridLayoutRule);
		gridLayoutRule.setNumColumns(2);
		List<AbstractComponent> components = new ArrayList<AbstractComponent>();
		for (EStructuralFeature structuralFeature : source
				.getEStructuralFeatures()) {
			components.addAll((List<AbstractComponent>) ff.get(target, index,
					structuralFeature,
					CoreWidgetsPackage.Literals.ABSTRACT_COMPONENT, context));
		}

		return components;
	}

	@SuppressWarnings("unchecked")
	@EAttributeMappingRule(datatype = "EString", target = Container.class, droppedType = AbstractComponent.class)
	public List<AbstractComponent> getEStringOnContainerComponents(
			Container target, int index, EAttribute source, Object context) {
		List<AbstractComponent> components = new ArrayList<AbstractComponent>();
		Label label = CoreWidgetsFactory.eINSTANCE.createLabel();
		label.setText(source.getName());
		GridDataRule labelLayoutData = SWTStylesFactory.eINSTANCE
				.createGridDataRule();
		labelLayoutData.setPropertyName("layout-data");
		label.getStyleRules().add(labelLayoutData);
		TextComponent text = CoreWidgetsFactory.eINSTANCE.createTextComponent();
		GridDataRule textLayoutData = SWTStylesFactory.eINSTANCE
				.createGridDataRule();
		textLayoutData.setPropertyName("layout-data");
		text.getStyleRules().add(textLayoutData);
		components.add(label);
		components.add(text);

		text.getHandlers().addAll(
				(List<Binding>) ff.get(text, 0,
						CoreWidgetsPackage.Literals.TEXT_COMPONENT__TEXT,
						EDPHandlersPackage.Literals.BINDING, context));
		return components;
	}

	@EAttributeMappingRule(datatype = "EString", target = TextComponent.class, droppedType = Binding.class)
	public List<Binding> getEStringOnTextComponentBindings(
			TextComponent target, int index, EAttribute source, Object context) {
		List<Binding> bindings = new ArrayList<Binding>();
		Binding model2UIBinding = createBinding("$input/@" + source.getName(),
				"@text");
		addPropertyChangedEvent(model2UIBinding, "$input/@" + source.getName());
		bindings.add(model2UIBinding);
		Binding UI2ModelBinding = createBinding("@text",
				"$input/@" + source.getName());
		addEvent(UI2ModelBinding, "core:ui:focus:out");
		bindings.add(UI2ModelBinding);
		return bindings;
	}

	@EClassMappingRule(target = Container.class, droppedType = StyleRule.class)
	public List<StyleRule> getClassOnContainerStyleRules(Container target,
			int index, EClass source, Object context) {
		GridLayoutRule gridLayoutRule = SWTStylesFactory.eINSTANCE
				.createGridLayoutRule();
		gridLayoutRule.setPropertyName("layout");
		gridLayoutRule.setNumColumns(2);
		List<StyleRule> styleRules = new ArrayList<StyleRule>();
		styleRules.add(gridLayoutRule);
		return styleRules;
	}

	protected Binding createBinding(String sourcePath, String targetPath) {
		Binding binding = EDPHandlersFactory.eINSTANCE.createBinding();
		StringParameter source = EDPHandlersFactory.eINSTANCE
				.createStringParameter();
		source.setName("source");
		StringParameter target = EDPHandlersFactory.eINSTANCE
				.createStringParameter();
		target.setName("target");
		source.setValue(sourcePath);
		target.setValue(targetPath);
		binding.getParameters().add(source);
		binding.getParameters().add(target);
		return binding;
	}

	protected void addPropertyChangedEvent(Binding binding, String path) {
		PropertyChangedEvent event = EDPEventsFactory.eINSTANCE
				.createPropertyChangedEvent();
		event.setPath(path);
		binding.getEvents().add(event);
	}

	protected void addEvent(Binding binding, String id) {
		Event event = EDPEventsFactory.eINSTANCE.createEvent();
		event.setId(id);
		binding.getEvents().add(event);
	}

}