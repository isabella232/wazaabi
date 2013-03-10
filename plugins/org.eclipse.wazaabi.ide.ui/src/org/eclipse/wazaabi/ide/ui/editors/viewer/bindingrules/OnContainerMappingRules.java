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

package org.eclipse.wazaabi.ide.ui.editors.viewer.bindingrules;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.wazaabi.ide.mapping.annotations.EAttributeMappingRule;
import org.eclipse.wazaabi.ide.mapping.annotations.EClassMappingRule;
import org.eclipse.wazaabi.ide.mapping.rules.MappingUtils;
import org.eclipse.wazaabi.ide.ui.editors.viewer.LabelProviderInfo;
import org.eclipse.wazaabi.mm.core.styles.BooleanRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.collections.ColumnDescriptor;
import org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeel;
import org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeelRule;
import org.eclipse.wazaabi.mm.core.styles.collections.PathSelector;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.core.widgets.Collection;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;
import org.eclipse.wazaabi.mm.core.widgets.Label;
import org.eclipse.wazaabi.mm.core.widgets.TextComponent;
import org.eclipse.wazaabi.mm.edp.handlers.Binding;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage;
import org.eclipse.wazaabi.mm.swt.styles.GridDataRule;
import org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;

public class OnContainerMappingRules {

	@LabelProviderInfo(text = "Map EENum into Collection")
	@EAttributeMappingRule(datatype = "EEnum")
	public List<AbstractComponent> getEEnumOnContainerComponents(
			Container target, int index, EAttribute source, Object context) {
		List<AbstractComponent> components = new ArrayList<AbstractComponent>();

		Label label = CoreWidgetsFactory.eINSTANCE.createLabel();
		label.setText(source.getName());
		GridDataRule labelLayoutData = SWTStylesFactory.eINSTANCE
				.createGridDataRule();
		labelLayoutData.setPropertyName("layout-data");
		label.getStyleRules().add(labelLayoutData);

		final Collection collection = CoreWidgetsFactory.eINSTANCE
				.createCollection();

		BooleanRule booleanRule = CoreStylesFactory.eINSTANCE
				.createBooleanRule();
		booleanRule.setValue(true);
		// booleanRule.setPropertyName("allow-row-selection");
		// booleanRule.setPropertyName("show-horizontal-lines");
		booleanRule.setPropertyName("header-visible");

		collection.getStyleRules().add(booleanRule);

		collection.setAnnotation("http://www.wazaabi.org/set-feature",
				"feature-name", "input");
		collection.setAnnotation("http://www.wazaabi.org/set-feature", "type",
				"locationpath");

		collection.setAnnotation("http://www.wazaabi.org/set-feature", "value",
				"eClassifier('"
						+ source.getEAttributeType().getEPackage().getNsURI()
						+ "', '" + source.getEAttributeType().getName() + "')");

		LookAndFeelRule lookAndFeelRule = CoreCollectionsStylesFactory.eINSTANCE
				.createLookAndFeelRule();
		lookAndFeelRule.setPropertyName("lookandfeel"); //$NON-NLS-1$
		lookAndFeelRule.setValue(LookAndFeel.COMBOBOX);
		collection.getStyleRules().add(lookAndFeelRule);

		PathSelector pathSelector1 = CoreCollectionsStylesFactory.eINSTANCE
				.createPathSelector();
		pathSelector1.setPropertyName("content-provider");
		pathSelector1.setEClassifierName("EEnum");
		pathSelector1.getPaths().add("&eLiterals/@instance");

		collection.getStyleRules().add(pathSelector1);

		GridDataRule collectionLayoutData = SWTStylesFactory.eINSTANCE
				.createGridDataRule();
		collectionLayoutData.setPropertyName("layout-data");
		collection.getStyleRules().add(collectionLayoutData);

		ColumnDescriptor columnDescriptor1 = CoreCollectionsStylesFactory.eINSTANCE
				.createColumnDescriptor();
		columnDescriptor1.setLabel("test1");
		columnDescriptor1.setPropertyName("column-descriptor");
		columnDescriptor1.setWidth(100);

		collection.getStyleRules().add(columnDescriptor1);
		components.add(label);
		components.add(collection);
		return components;
	}

	@SuppressWarnings("unchecked")
	@EClassMappingRule
	public List<AbstractComponent> getClassOnContainerComponents(
			Container target, int index, EClass source, Object context) {
		Container container = CoreWidgetsFactory.eINSTANCE.createContainer();
		GridLayoutRule gridLayoutRule = SWTStylesFactory.eINSTANCE
				.createGridLayoutRule();
		container.getStyleRules().add(gridLayoutRule);
		gridLayoutRule.setPropertyName("layout");
		gridLayoutRule.setNumColumns(2);
		List<AbstractComponent> components = new ArrayList<AbstractComponent>();
		for (EStructuralFeature structuralFeature : source
				.getEStructuralFeatures()) {
			container.getChildren().addAll(
					(List<AbstractComponent>) MappingUtils.getFFactory().get(
							target,
							index,
							structuralFeature,
							CoreWidgetsPackage.Literals.ABSTRACT_COMPONENT
									.getInstanceClass(), context));
		}
		components.add(container);
		return components;
	}

	@SuppressWarnings("unchecked")
	@EAttributeMappingRule(datatype = "EString")
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
				(List<Binding>) MappingUtils.getFFactory().get(text, 0, source,
						EDPHandlersPackage.Literals.BINDING, context));
		return components;
	}
}