/*******************************************************************************
 * Copyright (c) 2013 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.locationpaths.runtime;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.util.FeatureMap;
import org.eclipse.wazaabi.locationpaths.model.Axis;

public class FeatureMapEvaluator {

	protected static List<?> evaluate(FeatureMap fm, int axis,
			String nameTest) {
		if (fm == null)
			return Collections.emptyList();
		switch (axis) {
		case Axis.CHILD: {
			List<Object> result = new ArrayList<Object>(30);
			for (EObject child : getEContent(fm)) {
				if ((nameTest != null && nameTest.equals(child.eClass()
						.getName()))
						|| nameTest == null
						|| "*".equals(nameTest))
					if (child != null)
						result.add(child);
			}
			return result;
		}
		case Axis.DESCENDANT_OR_SELF:
			break;
		case Axis.PARENT:
			// only FeatureMap.Internal provides a way to access its owner (aka
			// EObject)
			if (nameTest == null
					&& fm instanceof FeatureMap.Internal
					&& ((FeatureMap.Internal) fm).getEObject()
							.eContainer() != null) {
				List<Object> result = new ArrayList<Object>(1);
				result.add(((FeatureMap.Internal) fm).getEObject()
						.eContainer());
				return result;
			}
			break;
		case Axis.CLASS:
			// only FeatureMap.Internal provides a way to access its owner (aka
			// EObject)
			if (nameTest == null && fm instanceof FeatureMap.Internal) {
				List<Object> result = new ArrayList<Object>(1);
				result.add(((FeatureMap.Internal) fm).getEObject()
						.eClass());
				return result;
			}
			break;
		case Axis.PACKAGE:
			if (nameTest == null) {
				List<Object> result = new ArrayList<Object>(1);
				if (fm instanceof EClass)
					result.add(((EClass) fm).getEPackage());
				else if (fm instanceof FeatureMap.Internal)
					// only FeatureMap.Internal provides a way to access its
					// owner (aka EObject)
					result.add(((FeatureMap.Internal) fm).getEObject()
							.eClass().getEPackage());
				return result;
			}
			break;
		case Axis.ATTRIBUTE:
			if (nameTest == null || "*".equals(nameTest)) //$NON-NLS-1$
				return getAllEAttributesContentAsList(fm);
			for (FeatureMap.Entry entry : fm)
				if (entry.getEStructuralFeature().getName().equals(nameTest)
						&& entry.getEStructuralFeature() instanceof EAttribute)
					return getFeatureContentAsList(fm,
							entry.getEStructuralFeature());
			break;
		case Axis.REFERENCE: {
			if (nameTest == null || "*".equals(nameTest)) //$NON-NLS-1$
				return getAllEReferencesContentAsList(fm);
			for (FeatureMap.Entry entry : fm)
				if (entry.getEStructuralFeature().getName().equals(nameTest)
						&& entry.getEStructuralFeature() instanceof EReference)
					return getFeatureContentAsList(fm,
							entry.getEStructuralFeature());
		}
			break;
		case Axis.VARIABLE: {
			try {
				// TODO : not finished, the name of the method will change

				// only FeatureMap.Internal provides a way to access its
				// owner (aka EObject)
				if (!(fm instanceof FeatureMap.Internal))
					return Collections.emptyList();
				Object object = ((FeatureMap.Internal) fm).getEObject();
				Method getValueMethod = object.getClass().getMethod(
						"get", new Class[] { String.class }); //$NON-NLS-1$
				if (getValueMethod != null) {
					Object value = getValueMethod.invoke(object,
							new Object[] { nameTest });
					if (value instanceof List)
						return (List<?>) value;
					else if (value instanceof Object) {
						List<Object> returnedAsList = new ArrayList<Object>(1);
						returnedAsList.add(value);
						return returnedAsList;
					}
				}
			} catch (SecurityException e) {
				e.printStackTrace();
			} catch (NoSuchMethodException e) {
				// Nothing to do here
			} catch (IllegalArgumentException e) {
				// Nothing to do here
			} catch (IllegalAccessException e) {
				e.printStackTrace();
				// Nothing to do here
			} catch (InvocationTargetException e) {
				// Nothing to do here
			}
		}
			break;
		case Axis.SELF:
			if (nameTest == null) {
				List<Object> result = new ArrayList<Object>(1);
				result.add(fm);
				return result;
			}
			break;
		}
		return Collections.emptyList();

	}

	private static List<EObject> getEContent(FeatureMap fm) {
		List<EObject> result = new ArrayList<EObject>();
		for (FeatureMap.Entry entry : fm) {
			if (entry.getEStructuralFeature() instanceof EReference) {
				EReference ref = (EReference) entry.getEStructuralFeature();
				if (ref.isContainment())
					if (entry.getValue() instanceof EObject) {
						if (!result.contains(entry.getValue()))
							result.add((EObject) entry.getValue());
					} else if (entry.getValue() instanceof List<?>) {
						for (Object item : (List<?>) entry.getValue())
							if (item instanceof EObject
									&& result.contains(item))
								result.add((EObject) item);
					}
			}
		}
		return result;
	}

	@SuppressWarnings("unchecked")
	protected static List<?> getAllEAttributesContentAsList(FeatureMap fm) {
		List<Object> result = new ArrayList<Object>();
		for (FeatureMap.Entry entry : fm)
			if (entry.getEStructuralFeature() instanceof EAttribute) {
				if (entry.getValue() instanceof List<?>)
					result.addAll((List<Object>) entry.getValue());
				else
					result.add(entry.getValue());
			}
		return result;
	}

	@SuppressWarnings("unchecked")
	protected static List<?> getAllEReferencesContentAsList(FeatureMap fm) {
		List<Object> result = new ArrayList<Object>();
		for (FeatureMap.Entry entry : fm)
			if (entry.getEStructuralFeature() instanceof EReference) {
				if (entry.getValue() instanceof List<?>)
					result.addAll((List<Object>) entry.getValue());
				else
					result.add(entry.getValue());
			}
		return result;
	}

	protected static List<?> getFeatureContentAsList(FeatureMap fm,
			EStructuralFeature feature) {
		Object value = fm.get(feature, false);
		if (value instanceof List<?>)
			return (List<?>) value;
		else if (value instanceof Object) {
			List<Object> result = new ArrayList<Object>(1);
			result.add(value);
			return result;
		}
		return Collections.emptyList();
	}
}
