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

package org.eclipse.wazaabi.engine.locationpaths.runtime;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.EcorePackage;
import org.eclipse.emf.ecore.util.FeatureMap;
import org.eclipse.wazaabi.engine.locationpaths.model.Axis;
import org.eclipse.wazaabi.engine.locationpaths.model.BinaryExpression;
import org.eclipse.wazaabi.engine.locationpaths.model.EqualityExpression;
import org.eclipse.wazaabi.engine.locationpaths.model.Expression;
import org.eclipse.wazaabi.engine.locationpaths.model.IntegerExpression;
import org.eclipse.wazaabi.engine.locationpaths.model.LiteralExpression;
import org.eclipse.wazaabi.engine.locationpaths.model.LocationPath;
import org.eclipse.wazaabi.engine.locationpaths.model.Operator;
import org.eclipse.wazaabi.engine.locationpaths.model.Pointer;
import org.eclipse.wazaabi.engine.locationpaths.model.Step;

public class Evaluator {

	public static Object getSingleObject(Object context, String path) {
		List<Pointer<?>> pointers = LocationSelector.select(context, path);
		if (!pointers.isEmpty()) {
			List<?> result = Evaluator.evaluate(pointers.get(0));
			if (result.isEmpty())
				return null;
			return result.get(0);
		}
		return null;
	}

	public static List<?> getObjects(Object context, String path) {
		List<Object> result = new ArrayList<Object>();
		Iterator<Pointer<?>> selectionIterator = LocationSelector.select(
				context, path).iterator();
		while (selectionIterator.hasNext()) {
			Pointer<?> pointer = selectionIterator.next();
			Iterator<Object> evaluationIterator = Evaluator.evaluate(pointer)
					.iterator();
			while (evaluationIterator.hasNext()) {
				Object item = evaluationIterator.next();
				if (!result.contains(item))
					result.add(item);
			}
		}
		return result;
	}

	public static List evaluate(Pointer<?> pointer) {
		if (pointer == null)
			return Collections.emptyList();
		return evaluate(pointer.getContext(), pointer.getStep());
	}

	// TODO : manage unsettable structural feature
	protected static List evaluate(Object context, Step step) {
		List pointers = evaluate(context, step.getAxis(), step.getNameTest());
		if (!step.getPredicates().isEmpty())
			pointers = filter(pointers, step.getPredicates());
		return removeDuplicates(pointers);
	}

	/**
	 * Evaluates the given context according to this axis and this nameTest
	 * 
	 * @param context
	 * @param axis
	 * @param nameTest
	 * @return A list which cannot be null.
	 */
	protected static List evaluate(EObject eContext, int axis, String nameTest) {
		if (eContext == null)
			return Collections.emptyList();
		switch (axis) {
		case Axis.CHILD: {
			List result = new ArrayList(30);
			Iterator contentIterator = eContext.eContents().iterator();
			while (contentIterator.hasNext()) {
				EObject child = (EObject) contentIterator.next();
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
			if (nameTest == null && eContext.eContainer() != null) {
				List result = new ArrayList(1);
				result.add(eContext.eContainer());
				return result;
			}
			break;
		case Axis.CLASS:
			if (nameTest == null) {
				List result = new ArrayList(1);
				result.add(eContext.eClass());
				return result;
			}
			break;
		case Axis.PACKAGE:
			if (nameTest == null) {
				List result = new ArrayList(1);
				if (eContext instanceof EClass)
					result.add(((EClass) eContext).getEPackage());
				else
					result.add(eContext.eClass().getEPackage());
				return result;
			}
			break;
		case Axis.ATTRIBUTE: {
			if (nameTest == null || "*".equals(nameTest)) //$NON-NLS-1$
				return getAllEAttributesContentAsList(eContext);
			EStructuralFeature attribute = eContext.eClass()
					.getEStructuralFeature(nameTest);
			if (attribute instanceof EAttribute)
				return getFeatureContentAsList(eContext, attribute);
		}
			break;
		case Axis.REFERENCE: {
			if (nameTest == null || "*".equals(nameTest)) //$NON-NLS-1$
				return getAllEReferencesContentAsList(eContext);
			EStructuralFeature reference = eContext.eClass()
					.getEStructuralFeature(nameTest);
			if (reference instanceof EReference)
				return getFeatureContentAsList(eContext, reference);
		}
			break;
		case Axis.VARIABLE: {
			try {
				// TODO : not finished, the name of the method will change
				Method getValueMethod = eContext.getClass().getMethod(
						"get", new Class[] { String.class }); //$NON-NLS-1$
				if (getValueMethod != null) {
					Object value = getValueMethod.invoke(eContext,
							new Object[] { nameTest });
					if (value instanceof List)
						return (List) value;
					else if (value instanceof Object) {
						List returnedAsList = new ArrayList(1);
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
				List result = new ArrayList(1);
				result.add(eContext);
				return result;
			}
			break;
		}
		return Collections.emptyList();
	}

	protected static boolean hasClassOrInterfaceName(String name, Object object) {
		if (object == null || name == null || name.isEmpty())
			return false;
		if (object.getClass().getSimpleName().equals(name))
			return true;
		for (Class<?> clazz : object.getClass().getInterfaces()) {
			System.out.println(clazz.getSimpleName());
			if (name.equals(clazz.getSimpleName()))
				return true;
		}
		return false;
	}

	protected static List<?> evaluate(List<?> context, int axis, String nameTest) {
		if (context == null)
			return Collections.emptyList();

		switch (axis) {
		case Axis.CHILD: {
			List<Object> result = new ArrayList<Object>(30);
			Iterator<?> contentIterator = context.iterator();
			while (contentIterator.hasNext()) {
				Object child = contentIterator.next();
				if ("*".equals(nameTest)
						|| hasClassOrInterfaceName(nameTest, child))
					if (child != null)
						result.add(child);
			}
			return result;
		}
		// case Axis.DESCENDANT_OR_SELF:
		// break;
		// case Axis.PARENT:
		// break;
		// case Axis.CLASS:
		// if (nameTest == null) {
		// List<String> result = new ArrayList<String>(1);
		// result.add(context.getClass().getName());
		// return result;
		// }
		// break;
		// case Axis.PACKAGE:
		// if (nameTest == null) {
		// List result = new ArrayList(1);
		// if (eContext instanceof EClass)
		// result.add(((EClass) eContext).getEPackage());
		// else
		// result.add(eContext.eClass().getEPackage());
		// return result;
		// }
		// break;
		// case Axis.ATTRIBUTE: {
		//			if (nameTest == null || "*".equals(nameTest)) //$NON-NLS-1$
		// return getAllEAttributesContentAsList(eContext);
		// EStructuralFeature attribute = eContext.eClass()
		// .getEStructuralFeature(nameTest);
		// if (attribute instanceof EAttribute)
		// return getFeatureContentAsList(eContext, attribute);
		// }
		// break;
		// case Axis.REFERENCE: {
		//			if (nameTest == null || "*".equals(nameTest)) //$NON-NLS-1$
		// return getAllEReferencesContentAsList(eContext);
		// EStructuralFeature reference = eContext.eClass()
		// .getEStructuralFeature(nameTest);
		// if (reference instanceof EReference)
		// return getFeatureContentAsList(eContext, reference);
		// }
		// break;
		// case Axis.VARIABLE: {
		// try {
		// // TODO : not finished, the name of the method will change
		// Method getValueMethod = eContext.getClass().getMethod(
		//						"get", new Class[] { String.class }); //$NON-NLS-1$
		// if (getValueMethod != null) {
		// Object value = getValueMethod.invoke(eContext,
		// new Object[] { nameTest });
		// if (value instanceof List)
		// return (List) value;
		// else if (value instanceof Object) {
		// List returnedAsList = new ArrayList(1);
		// returnedAsList.add(value);
		// return returnedAsList;
		// }
		// }
		// } catch (SecurityException e) {
		// e.printStackTrace();
		// } catch (NoSuchMethodException e) {
		// // Nothing to do here
		// } catch (IllegalArgumentException e) {
		// // Nothing to do here
		// } catch (IllegalAccessException e) {
		// e.printStackTrace();
		// // Nothing to do here
		// } catch (InvocationTargetException e) {
		// // Nothing to do here
		// }
		// }
		// break;
		// case Axis.SELF:
		// if (nameTest == null) {
		// List result = new ArrayList(1);
		// result.add(eContext);
		// return result;
		// }
		// break;
		}
		return Collections.emptyList();
	}


	protected static List<?> evaluate(Object context, int axis, String nameTest) {
		if (context == null)
			return Collections.emptyList();
		if (context instanceof EObject)
			return evaluate((EObject) context, axis, nameTest);
		if (context instanceof FeatureMap)
			return FeatureMapEvaluator.evaluate((FeatureMap) context, axis, nameTest);
		if (context instanceof List<?>)
			return evaluate((List<?>) context, axis, nameTest);
		return Collections.emptyList();
	}

	protected static List getAllEAttributesContentAsList(EObject object) {
		List result = new ArrayList(30);
		Iterator attributeIterator = object.eClass().getEAllAttributes()
				.iterator();
		while (attributeIterator.hasNext())
			result.addAll(getFeatureContentAsList(object,
					(EAttribute) attributeIterator.next()));
		return result;
	}

	protected static List<?> getAllEReferencesContentAsList(EObject object) {
		List<Object> result = new ArrayList<Object>(30);
		Iterator<EReference> referenceIterator = object.eClass()
				.getEAllReferences().iterator();
		while (referenceIterator.hasNext())
			result.addAll(getFeatureContentAsList(object,
					referenceIterator.next()));
		return result;
	}

	protected static List<?> getFeatureContentAsList(EObject object,
			EStructuralFeature feature) {
		Object value = object.eGet(feature);
		if (value instanceof List<?>)
			return (List<?>) value;
		else if (value instanceof Object) {
			List<Object> result = new ArrayList<Object>(1);
			result.add(value);
			return result;
		}
		return Collections.emptyList();
	}

	/**
	 * Filters a list of Java object using the given list of predicates.
	 * 
	 * @param rawChildren
	 *            A list of Java objects
	 * @param predicate
	 *            The list of predicates used to filter the list.
	 * @return A List of object, the list can be empty.
	 */
	protected static List filter(List rawChildren, List<Expression> predicates) {
		List result = rawChildren;
		for (int i = 0; i < predicates.size(); i++) {
			result = filter(result, predicates.get(i));
		}
		return result;
	}

	/**
	 * Filters a list of Object using the predicate.
	 * 
	 * @param rawChildren
	 *            A list of Java objects
	 * @param predicate
	 *            The predicate used to filter the list.
	 * @return A List of object, the list can be empty.
	 */
	protected static List filter(List rawChildren, Expression predicate) {
		// TODO provisional
		if (predicate instanceof IntegerExpression)
			return filter(rawChildren, (IntegerExpression) predicate);
		else if (predicate instanceof EqualityExpression)
			return filter(rawChildren, (EqualityExpression) predicate);
		// else if (predicate instanceof BinaryExpression)
		// return filter(rawChildren, (BinaryExpression) predicate);
		return rawChildren;
	}

	/**
	 * Gets and returns the element of the list whose index is given by the
	 * predicate.
	 * 
	 * @param rawChildren
	 *            A list of objects.
	 * @param predicate
	 *            An predicate containing the 0 based index of the returned
	 *            element.
	 * @return A List containing only one element when found, an empty list
	 *         otherwise.
	 */
	protected static List filter(List rawChildren, IntegerExpression predicate) {
		// in xpath, [2] is equivalent to [position()==2]
		if (rawChildren.isEmpty() || predicate.getValue() > rawChildren.size())
			return Collections.emptyList();
		List result = new ArrayList(1);
		result.add(rawChildren.get(predicate.getValue()));
		return result;
	}

	protected static List filter(List rawChildren, BinaryExpression predicate) {
		List result = new ArrayList(rawChildren.size());
		return result;
	}

	protected static List filter(List rawChildren, EqualityExpression predicate) {
		if (predicate.getOperator() == Operator.EQUAL) {
			LocationPath locationPath = null;
			Expression otherExpression = null;
			if (predicate.getLeftOperand() instanceof LocationPath) {
				locationPath = (LocationPath) predicate.getLeftOperand();
				otherExpression = predicate.getRightOperand();
			} else if (predicate.getRightOperand() instanceof LocationPath) {
				locationPath = (LocationPath) predicate.getRightOperand();
				otherExpression = predicate.getLeftOperand();
			}

			if (locationPath.getSteps().size() == 1
					&& ((Step) locationPath.getSteps().get(0)).getAxis() == Axis.ATTRIBUTE) {
				final String attributeName = ((Step) locationPath.getSteps()
						.get(0)).getNameTest();
				if (otherExpression instanceof LiteralExpression
						&& ((LiteralExpression) otherExpression).getValue() != null) {
					final String value = ((LiteralExpression) otherExpression)
							.getValue();
					return filterByStringEAttributeValue(rawChildren,
							attributeName, value);
				} else if (otherExpression instanceof IntegerExpression) {
					final int value = ((IntegerExpression) otherExpression)
							.getValue();
					return filterByIntegerEAttributeValue(rawChildren,
							attributeName, value);
				}
			}
		}
		return Collections.emptyList();
	}

	public static List filterByStringEAttributeValue(List rawChildren,
			String attributeName, String value) {
		List result = new ArrayList(rawChildren.size());
		Iterator iterator = rawChildren.iterator();
		while (iterator.hasNext()) {
			Object item = iterator.next();
			if (item instanceof EObject) {
				EStructuralFeature feature = ((EObject) item).eClass()
						.getEStructuralFeature(attributeName);
				if (feature instanceof EAttribute
						&& ((EAttribute) feature).getEAttributeType() == EcorePackage.Literals.ESTRING) {
					if (value.equals(((EObject) item).eGet(feature)))
						result.add(item);
				}
			}
		}
		return result;
	}

	public static List filterByIntegerEAttributeValue(List rawChildren,
			String attributeName, int value) {
		List result = new ArrayList(rawChildren.size());
		Iterator iterator = rawChildren.iterator();
		while (iterator.hasNext()) {
			Object item = iterator.next();
			if (item instanceof EObject) {
				EStructuralFeature feature = ((EObject) item).eClass()
						.getEStructuralFeature(attributeName);
				if (feature instanceof EAttribute) {
					switch (((EAttribute) feature).getEAttributeType()
							.getClassifierID()) {
					case EcorePackage.EINT:
					case EcorePackage.ELONG:
					case EcorePackage.ESHORT:
						if (new Integer(value).equals(((EObject) item)
								.eGet(feature)))
							result.add(item);
						break;
					}
				}
			}
		}
		return result;
	}

	/**
	 * Iterates over the given list and ensures that no value can be duplicated.
	 * 
	 * @param rawChildren
	 *            The initial list
	 * @return A non null list of Object.
	 */
	protected static List removeDuplicates(List rawChildren) {
		List result = new ArrayList(rawChildren.size());
		Iterator rawChildrenIterator = rawChildren.iterator();
		while (rawChildrenIterator.hasNext()) {
			Object item = rawChildrenIterator.next();
			if (!result.contains(item))
				result.add(item);
		}
		return rawChildren;
	}
}
