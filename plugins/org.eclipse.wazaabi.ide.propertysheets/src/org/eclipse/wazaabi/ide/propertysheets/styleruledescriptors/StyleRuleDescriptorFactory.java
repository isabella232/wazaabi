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

package org.eclipse.wazaabi.ide.propertysheets.styleruledescriptors;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.wazaabi.engine.core.editparts.AbstractButtonEditPart;
import org.eclipse.wazaabi.engine.core.editparts.AbstractComponentEditPart;
import org.eclipse.wazaabi.engine.core.editparts.ContainerEditPart;
import org.eclipse.wazaabi.engine.core.editparts.TextComponentEditPart;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.StyledElement;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;

public class StyleRuleDescriptorFactory {

	// This class is used locally until we found a better place for storing the
	// descriptors
	private static class StyleRuleDescriptorChildrenEntry {
		private final String propertyName;
		private final StyleRuleDescriptor[] descriptors;

		public StyleRuleDescriptorChildrenEntry(String propertyName,
				StyleRuleDescriptor[] descriptors) {
			this.propertyName = propertyName;
			this.descriptors = descriptors;
		}

		public StyleRuleDescriptor[] getDescriptors() {
			return descriptors;
		}

		public String getPropertyName() {
			return propertyName;
		}

	}

	// This class is used locally until we found a better place for storing the
	// descriptors
	private static class StyleRuleDescriptorEntry {
		private final EClass key;
		private final StyleRuleDescriptor[] descriptors;

		public StyleRuleDescriptorEntry(EClass key,
				StyleRuleDescriptor[] descriptors) {
			this.key = key;
			this.descriptors = descriptors;
		}

		public StyleRuleDescriptor[] getDescriptors() {
			return descriptors;
		}

		public EClass getKey() {
			return key;
		}

	}

	private static final StyleRuleDescriptorEntry entries[] = new StyleRuleDescriptorEntry[] {
			new StyleRuleDescriptorEntry(
					CoreWidgetsPackage.Literals.ABSTRACT_COMPONENT,
					new StyleRuleDescriptor[] {
							new StyleRuleDescriptor(
									AbstractComponentEditPart.BACKGROUND_COLOR_PROPERTY_NAME,
									"", "description",
									"http://www.wazaabi.org/core/styles",
									"ColorRule"),
							new StyleRuleDescriptor(
									AbstractComponentEditPart.FOREGROUND_COLOR_PROPERTY_NAME,
									"", "description",
									"http://www.wazaabi.org/core/styles",
									"ColorRule"),
							new StyleRuleDescriptor(
									AbstractComponentEditPart.FONT_PROPERTY_NAME,
									"", "description",
									"http://www.wazaabi.org/core/styles",
									"FontRule"),
							new StyleRuleDescriptor(
									AbstractComponentEditPart.TOOLTIP_TEXT_PROPERTY_NAME,
									"", "description",
									"http://www.wazaabi.org/core/styles",
									"StringRule"),
							new StyleRuleDescriptor(
									AbstractComponentEditPart.ERROR_TEXT_PROPERTY_NAME,
									"", "description",
									"http://www.wazaabi.org/core/styles",
									"StringRule"),
							new StyleRuleDescriptor(
									AbstractComponentEditPart.DIRECTION_PROPERTY_NAME,
									"", "description",
									"http://www.wazaabi.org/core/styles",
									"StringRule"),
							new StyleRuleDescriptor(
									AbstractComponentEditPart.LAYOUT_DATA_PROPERTY_NAME,
									"", "description",
									"http://www.wazaabi.org/core/styles",
									"StringRule"),
							new StyleRuleDescriptor(
									AbstractComponentEditPart.ENABLED_PROPERTY_NAME,
									"", "description",
									"http://www.wazaabi.org/core/styles",
									"StringRule"),
							new StyleRuleDescriptor(
									AbstractComponentEditPart.VISIBLE_PROPERTY_NAME,
									"", "description",
									"http://www.wazaabi.org/core/styles",
									"StringRule"),
							new StyleRuleDescriptor(
									AbstractComponentEditPart.ORIENTATION_PROPERTY_NAME,
									"", "description",
									"http://www.wazaabi.org/core/styles",
									"StringRule"),
							new StyleRuleDescriptor(
									AbstractComponentEditPart.BORDER_PROPERTY_NAME,
									"", "description",
									"http://www.wazaabi.org/core/styles",
									"BooleanRule"),
							new StyleRuleDescriptor(
									AbstractComponentEditPart.TAB_INDEX_PROPERTY_NAME,
									"", "description",
									"http://www.wazaabi.org/core/styles",
									"StringRule") }),

			new StyleRuleDescriptorEntry(
					CoreWidgetsPackage.Literals.ABSTRACT_BUTTON,
					new StyleRuleDescriptor[] {
							new StyleRuleDescriptor(
									AbstractButtonEditPart.TEXT_PROPERTY_NAME,
									"", "description",
									"http://www.wazaabi.org/core/styles",
									"StringRule"),
							new StyleRuleDescriptor(
									AbstractButtonEditPart.IMAGE_PROPERTY_NAME,
									"", "description",
									"http://www.wazaabi.org/core/styles",
									"StringRule"),
							new StyleRuleDescriptor(
									AbstractButtonEditPart.FLAT_PROPERTY_NAME,
									"", "description",
									"http://www.wazaabi.org/core/styles",
									"StringRule") }),

			new StyleRuleDescriptorEntry(CoreWidgetsPackage.Literals.CONTAINER,
					new StyleRuleDescriptor[] {
							new StyleRuleDescriptor(
									ContainerEditPart.LAYOUT_PROPERTY_NAME, "",
									"description",
									"http://www.wazaabi.org/core/styles",
									"LayoutRule"),
							new StyleRuleDescriptor(ContainerEditPart.TITLE,
									"", "description",
									"http://www.wazaabi.org/core/styles",
									"StringRule") }),
			new StyleRuleDescriptorEntry(
					CoreWidgetsPackage.Literals.TEXT_COMPONENT,
					new StyleRuleDescriptor[] {
							new StyleRuleDescriptor(
									TextComponentEditPart.ORIENTATION_PROPERTY_NAME,
									"", "description",
									"http://www.wazaabi.org/core/styles",
									"StringRule"),
							new StyleRuleDescriptor(
									TextComponentEditPart.MULTI_LINE_PROPERTY_NAME,
									"", "description",
									"http://www.wazaabi.org/core/styles",
									"StringRule"),
							new StyleRuleDescriptor(
									TextComponentEditPart.READ_ONLY_PROPERTY_NAME,
									"", "description",
									"http://www.wazaabi.org/core/styles",
									"StringRule"),
							new StyleRuleDescriptor(
									TextComponentEditPart.ECHO_CHAR_PROPERTY_NAME,
									"", "description",
									"http://www.wazaabi.org/core/styles",
									"StringRule"),
							new StyleRuleDescriptor(
									TextComponentEditPart.WRAP_PROPERTY_NAME,
									"", "description",
									"http://www.wazaabi.org/core/styles",
									"StringRule") })

	};

	private static StyleRuleDescriptorChildrenEntry childrenEntries[] = new StyleRuleDescriptorChildrenEntry[] { new StyleRuleDescriptorChildrenEntry(
			ContainerEditPart.LAYOUT_PROPERTY_NAME, new StyleRuleDescriptor[] {
					new StyleRuleDescriptor("RowLayout", "RowLayout",
							"RowLayoutDescription",
							"http://www.wazaabi.org/swt/styles",
							"RowLayoutRule"),
					new StyleRuleDescriptor("GridLayout", "GridLayout",
							"GridLayoutDescription",
							"http://www.wazaabi.org/swt/styles",
							"GridLayoutRule") })

	};

	private static HashMap<EClass, List<StyleRuleDescriptor>> store = null;

	protected HashMap<EClass, List<StyleRuleDescriptor>> createStore() {
		HashMap<EClass, List<StyleRuleDescriptor>> store = new HashMap<EClass, List<StyleRuleDescriptor>>();
		for (StyleRuleDescriptorEntry entry : entries)
			store.put(entry.getKey(), enrichDescriptorsWithChildren(Arrays
					.asList(entry.getDescriptors())));
		return store;
	}

	protected List<StyleRuleDescriptor> enrichDescriptorsWithChildren(
			List<StyleRuleDescriptor> descriptors) {
		List<StyleRuleDescriptor> result = new ArrayList<StyleRuleDescriptor>();
		for (StyleRuleDescriptor descriptor : descriptors) {
			Set<StyleRuleDescriptor> children = getChildren(descriptor);
			if (!children.isEmpty()) {
				descriptor.getChildren().addAll(children);
				for (StyleRuleDescriptor child : children)
					child.setContainer(descriptor);
			}
			result.add(descriptor);
		}
		return result;
	}

	public StyleRuleDescriptor findDescriptor(StyledElement styledElement,
			String propertyName) {
		for (StyleRuleDescriptor descriptor : getDescriptors(styledElement))
			if (descriptor.getPropertyName().equals(propertyName))
				return descriptor;
		return null;
	}

	protected Set<StyleRuleDescriptor> getChildren(
			StyleRuleDescriptor descriptor) {
		Set<StyleRuleDescriptor> result = new HashSet<StyleRuleDescriptor>();
		for (StyleRuleDescriptorChildrenEntry childrenDescriptor : childrenEntries)
			if (childrenDescriptor.getPropertyName().equals(
					descriptor.getPropertyName()))
				result.addAll(Arrays.asList(childrenDescriptor.getDescriptors()));
		return result;
	}

	public StyleRuleDescriptor getDescriptor(StyleRule rule) {
		if (rule != null) {
			String eClassName = rule.eClass().getInstanceClassName();
			int idx = eClassName.lastIndexOf('.');
			if (idx != -1)
				eClassName = eClassName.substring(idx + 1);
			for (List<StyleRuleDescriptor> descriptors : getStore().values())
				for (StyleRuleDescriptor descriptor : descriptors) {
					if (eClassName.equals(descriptor.getEClassName())
							&& rule.eClass().getEPackage().getNsURI()
									.equals(descriptor.getPackageURI()))
						return descriptor;
					for (StyleRuleDescriptor child : descriptor.getChildren())
						if (eClassName.equals(child.getEClassName())
								&& rule.eClass().getEPackage().getNsURI()
										.equals(child.getPackageURI()))
							return child;
				}
		}
		return null;
	}

	protected Set<StyleRuleDescriptor> getDescriptors(EClass eClass) {
		Set<StyleRuleDescriptor> result = new HashSet<StyleRuleDescriptor>();
		List<EClass> superTypes = eClass.getEAllSuperTypes();
		List<StyleRuleDescriptor> descriptors = getStore().get(eClass);
		if (descriptors != null)
			result.addAll(descriptors);
		for (EClass type : superTypes) {
			descriptors = getStore().get(type);
			if (descriptors != null)
				result.addAll(descriptors);
		}
		return result;
	}

	public Set<StyleRuleDescriptor> getDescriptors(StyledElement styledElement) {
		if (styledElement == null)
			return Collections.emptySet();
		return getDescriptors(styledElement.eClass());
	}

	protected HashMap<EClass, List<StyleRuleDescriptor>> getStore() {
		if (store == null)
			store = createStore();
		return store;
	}
}
