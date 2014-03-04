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

package org.eclipse.wazaabi.engine.core.editparts.stylerules;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map.Entry;

import org.eclipse.emf.ecore.EAnnotation;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EEnumLiteral;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EcorePackage;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;

public class StyleRulesHelper {

	public static final String STYLE_PROPERTY_DEFINITION_URI = "http://www.wazaabi.org/style/property/definition"; //$NON-NLS-1$
	public static final String PACKAGE_KEYWORD = "package"; //$NON-NLS-1$
	public static final String ECLASS_KEYWORD = "EClass"; //$NON-NLS-1$

	private static final String PACKAGE_PREFIX = PACKAGE_KEYWORD + "="; //$NON-NLS-1$
	private static final String ECLASS_PREFIX = ECLASS_KEYWORD + "="; //$NON-NLS-1$

	public static void buildCoreStylePropertyDescriptors(EClass eClass,
			HashMap<String, StylePropertyDescriptor> descriptors) {
		for (EClass ancestor : eClass.getEAllSuperTypes())
			if (!CoreStylesPackage.Literals.STYLED_ELEMENT.equals(ancestor)
					&& CoreStylesPackage.Literals.STYLED_ELEMENT
							.isSuperTypeOf(ancestor))
				addStyleDefinitions(ancestor, descriptors);
		addStyleDefinitions(eClass, descriptors);
	}

	public static void buildPlatformSpecificStylePropertyDescritors(
			EClass eClass, HashMap<String, StylePropertyDescriptor> descriptors) {
		for (EClass ancestor : eClass.getEAllSuperTypes())
			addStyleDefinitions(ancestor, descriptors);
		addStyleDefinitions(eClass, descriptors);
	}

	protected static void addStyleDefinitions(EClass eClass,
			HashMap<String, StylePropertyDescriptor> descriptors) {
		for (EAnnotation annotation : eClass.getEAnnotations()) {
			StylePropertyDescriptor descriptor = createStylePropertyDescriptor(annotation);
			if (descriptor != null)
				descriptors.put(descriptor.getName(), descriptor);
		}
	}

	/**
	 * Creates a StylePropertyDescriptor by parsing the given annotation. This
	 * EAnnotation is parsed only if its source is
	 * 'http://www.wazaabi.org/style/property/definition' and if all the
	 * mandatory details are well formed and drive to non null values.
	 * 
	 * @param annotation
	 *            Any EAnnotation
	 * 
	 * @return A StylePropertyDescriptor if found, null otherwise.
	 */
	protected static StylePropertyDescriptor createStylePropertyDescriptor(
			EAnnotation annotation) {
		if (STYLE_PROPERTY_DEFINITION_URI.equals(annotation.getSource())) {
			String name = annotation.getDetails().get("name"); //$NON-NLS-1$
			if (name == null || "".equals(name)) //$NON-NLS-1$
				throw new RuntimeException("No name found in an annotation of " //$NON-NLS-1$
						+ ((EClass) annotation.getEModelElement()).getName());
			EClass type = resolveEClass(annotation);
			if (type == null)
				throw new RuntimeException(
						"No type resolved in annotation whose name is " //$NON-NLS-1$
								+ annotation.getDetails().get("name") //$NON-NLS-1$
								+ " in " //$NON-NLS-1$
								+ ((EClass) annotation.getEModelElement())
										.getName());
			return new StylePropertyDescriptor(name, type, buildDefault(
					annotation, type));
		}
		return null;
	}

	/**
	 * Returns an EClass using the description found in the 'type' of a
	 * EAnnotation. This method checks also that expected values are well
	 * provided.
	 * 
	 * @param annotation
	 *            An StyleDescriptor EAnnotation whose name is not null or blank
	 * 
	 * @return An EClass if found, null otherwise
	 */
	protected static EClass resolveEClass(EAnnotation annotation) {
		EClass result = null;
		String packageURI = null;
		String className = null;
		final String typeDescription = annotation.getDetails().get("type"); //$NON-NLS-1$
		if (typeDescription == null || "".equals(typeDescription)) //$NON-NLS-1$
			throw new RuntimeException(
					"No type described in annotation whose name is " //$NON-NLS-1$
							+ annotation.getDetails().get("name") //$NON-NLS-1$
							+ " in " //$NON-NLS-1$
							+ ((EClass) annotation.getEModelElement())
									.getName());
		String lines[] = typeDescription.split("[\\r\\n]+"); //$NON-NLS-1$
		for (String line : lines)
			if (line.startsWith(PACKAGE_PREFIX))
				packageURI = line.substring(PACKAGE_PREFIX.length());
			else if (line.startsWith(ECLASS_PREFIX))
				className = line.substring(ECLASS_PREFIX.length());
			else
				throw new RuntimeException(
						"Only " //$NON-NLS-1$
								+ PACKAGE_KEYWORD //$NON-NLS-1$
								+ " and " //$NON-NLS-1$
								+ ECLASS_KEYWORD //$NON-NLS-1$
								+ " are allowed in a 'type' details of EAnnotation whose name is " //$NON-NLS-1$
								+ annotation.getDetails().get("name") //$NON-NLS-1$
								+ " in " //$NON-NLS-1$
								+ ((EClass) annotation.getEModelElement())
										.getName());

		if (packageURI != null && !"".equals(packageURI)) {
			EPackage ePackage = EPackage.Registry.INSTANCE
					.getEPackage(packageURI);
			if (ePackage != null
					&& ePackage.getEClassifier(className) instanceof EClass)
				result = (EClass) ePackage.getEClassifier(className);
			else
				throw new RuntimeException(
						"No EPackage found in annotation whose name is " //$NON-NLS-1$
								+ annotation.getDetails().get("name") //$NON-NLS-1$
								+ " in " //$NON-NLS-1$
								+ ((EClass) annotation.getEModelElement())
										.getName());
		} else
			throw new RuntimeException(
					"Bad EPackage declaration in annotation whose name is " //$NON-NLS-1$
							+ annotation.getDetails().get("name") //$NON-NLS-1$
							+ " in " //$NON-NLS-1$
							+ ((EClass) annotation.getEModelElement())
									.getName());
		return result;
	}

	/**
	 * Builds a default EObject by iterating over the annotation 'default'
	 * content. When found, attributes are set using conversion mechanisms. All
	 * the attribute are not supposed to be set.
	 * 
	 * @param annotation
	 *            A non null EAnnotation whose 'name' is not null.
	 * @param eClass
	 *            The EClass of the EObject to create
	 * @return An EObject if the build mechanisms succeded.
	 */

	protected static StyleRule buildDefault(EAnnotation annotation,
			EClass eClass) {
		EObject result = null;
		final String content = annotation.getDetails().get("default"); //$NON-NLS-1$
		if (content == null || "".equals(content)) //$NON-NLS-1$
			return null;

//		try {
//			Properties defaultValues = new Properties();
//			defaultValues.load(new ByteArrayInputStream(content
//					.getBytes("UTF-8")));
//			result = EcoreUtil.create(eClass);
//			@SuppressWarnings("rawtypes")
//			Iterator iterator = defaultValues.entrySet().iterator();
//			while (iterator.hasNext()) {
//				@SuppressWarnings("unchecked")
//				Entry<String, String> entry = (Entry<String, String>) iterator
//						.next();
//				EAttribute attr = getEAttribute(eClass, entry.getKey());
//				if (attr != null) {
//					EDataType attrType = attr.getEAttributeType();
//					if (attrType instanceof EEnum) {
//						EEnumLiteral eEnumLiteral = ((EEnum) attrType)
//								.getEEnumLiteral(entry.getValue());
//						if (eEnumLiteral != null)
//							result.eSet(attr, eEnumLiteral.getInstance());
//					} else
//						switch (attrType.getClassifierID()) {
//						case EcorePackage.EBOOLEAN:
//							result.eSet(attr,
//									Boolean.parseBoolean(entry.getValue()));
//							break;
//						// TODO : continue to implement conversion mechanisms
//						default:
//							throw new RuntimeException(
//									"No conversion mechanism for " + //$NON-NLS-1$
//											(((EClass) annotation
//													.getEModelElement())
//													.getName())
//											+ "." //$NON-NLS-1$
//											+ annotation.getDetails().get(
//													"name") + //$NON-NLS-1$ 
//											".default"); //$NON-NLS-1$
//						}
//				} else
//					throw new RuntimeException(
//							"Unable to find an attribute for " + //$NON-NLS-1$
//									(((EClass) annotation.getEModelElement())
//											.getName()) + "." //$NON-NLS-1$
//									+ annotation.getDetails().get("name")); //$NON-NLS-1$ 
//			}
//		} catch (IOException e) {
//			throw new RuntimeException("Unable to read default for " + //$NON-NLS-1$
//					(((EClass) annotation.getEModelElement()).getName()) + "." //$NON-NLS-1$
//					+ annotation.getDetails().get("name")); //$NON-NLS-1$ 
//		}
		return (StyleRule) result;
	}

	/**
	 * Simply returns an EAttribute of the given EClass knowing its name
	 * 
	 * @param eClass
	 *            The EClass where the EAttribute is supposed to be located in.
	 * @param name
	 *            The attribute's name
	 * @return Null if no attribute with this name exists in this class
	 */
	private static EAttribute getEAttribute(EClass eClass, String name) {
		for (EAttribute attr : eClass.getEAllAttributes())
			if (attr.getName().equals(name))
				return attr;
		return null;
	}

}
