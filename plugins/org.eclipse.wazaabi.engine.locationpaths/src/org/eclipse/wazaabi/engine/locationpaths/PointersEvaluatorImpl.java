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

package org.eclipse.wazaabi.engine.locationpaths;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.wazaabi.engine.edp.locationpaths.IConverter;
import org.eclipse.wazaabi.engine.edp.locationpaths.IPointersEvaluator;
import org.eclipse.wazaabi.engine.locationpaths.model.Axis;
import org.eclipse.wazaabi.engine.locationpaths.model.EMFPointer;
import org.eclipse.wazaabi.engine.locationpaths.model.Pointer;
import org.eclipse.wazaabi.engine.locationpaths.runtime.Evaluator;
import org.eclipse.wazaabi.engine.locationpaths.runtime.LocationSelector;

public class PointersEvaluatorImpl implements IPointersEvaluator {

	public List<Pointer<?>> selectPointers(Object context, String path) {
		if (context instanceof EObject || context instanceof List<?>)
			return LocationSelector.select(context, path);
		return Collections.emptyList();
	}

	// public void copy(List sourcePointers, List targetPointers) {
	// if (sourcePointers.size() == 1 && targetPointers.size() == 1) {
	// EMFPointer source = (EMFPointer) sourcePointers.get(0);
	// EMFPointer target = (EMFPointer) targetPointers.get(0);
	// Object sourceValues = Evaluator.evaluate(source);
	// setValue(sourceValues, target);
	// }
	// }

	protected void internalSetValue(EMFPointer target, Object newValue,
			IConverter converter) {
		if (target.getStep().getAxis() == Axis.ATTRIBUTE
				|| target.getStep().getAxis() == Axis.REFERENCE
				|| target.getStep().getAxis() == Axis.VARIABLE
				&& target.getStep().getNameTest() != null
				&& !target.getStep().getNameTest().equals("") //$NON-NLS-1$
				&& !target.getStep().getNameTest().equals("*")) //$NON-NLS-1$ 
		{

			if (target.getStep().getAxis() == Axis.VARIABLE) {
				// TODO: experimental work, need verification
				Object convertedValue = null;
				if (newValue instanceof List)
					convertedValue = converter != null ? converter
							.convert(newValue) : newValue;
//				else if (((List) newValue).size() == 1)
//					convertedValue = converter != null ? converter
//							.convert(((List) newValue).get(0)) : ((List) newValue)
//							.get(0);

				else if (newValue == Collections.EMPTY_LIST)
					convertedValue = converter != null ? converter.convert(null)
							: null;
				try {
					// TODO : not finished, the name of the method will change
					Method setValueMethod = target
							.getContext()
							.getClass()
							.getMethod(
									"set", new Class[] { String.class, Object.class }); //$NON-NLS-1$
					if (setValueMethod != null)
						setValueMethod.invoke(target.getContext(),
								new Object[] { target.getStep().getNameTest(),
										convertedValue });

				} catch (SecurityException e) {
					e.printStackTrace();
				} catch (NoSuchMethodException e) {
					// Nothing to do here
				} catch (IllegalArgumentException e) {
					// Nothing to do here
				} catch (IllegalAccessException e) {
					// Nothing to do here
				} catch (InvocationTargetException e) {
					// Nothing to do here
				}
				return;
			}
			EStructuralFeature feature = target.getContext().eClass()
					.getEStructuralFeature(target.getStep().getNameTest());
			if (feature != null) {
				switch (target.getStep().getAxis()) {
				case Axis.ATTRIBUTE:
					if (!(feature instanceof EAttribute))
						return;
					break;
				case Axis.REFERENCE:
					if (!(feature instanceof EReference))
						return;
					break;
				default:
					return;
				}

				// TODO : not enough, need to manage different cases List->List,
				// iteration on List, etc...
				if (newValue instanceof List) {
					if (feature.isMany())
						setFeature(target.getContext(), feature,
								converter != null ? converter.convert(newValue)
										: newValue);
					else if (((List) newValue).size() == 1)
						setFeature(
								target.getContext(),
								feature,
								converter != null ? converter
										.convert(((List) newValue).get(0))
										: ((List) newValue).get(0));
					// TODO : check & test this, not sure at all ==> check side
					// effects
					else if (newValue == Collections.EMPTY_LIST)
						// target.getContext().eSet(feature, null);
						setFeature(target.getContext(), feature,
								converter != null ? converter.convert(null)
										: null);
				}
			}
		}
	}
	
	protected void internalSetValue2(EMFPointer target, Object newValue) {
		if (target.getStep().getAxis() == Axis.ATTRIBUTE
				|| target.getStep().getAxis() == Axis.REFERENCE
				|| target.getStep().getAxis() == Axis.VARIABLE
				&& target.getStep().getNameTest() != null
				&& !target.getStep().getNameTest().equals("") //$NON-NLS-1$
				&& !target.getStep().getNameTest().equals("*")) //$NON-NLS-1$ 
		{

			if (target.getStep().getAxis() == Axis.VARIABLE) {
				// TODO: experimental work, need verification
				Object convertedValue = null;
				if (newValue instanceof List)
					convertedValue = newValue;

				// TODO this should go into the validator 
//				else if (newValue == Collections.EMPTY_LIST)
//					convertedValue = converter != null ? converter.convert(null)
//							: null;
				try {
					// TODO : not finished, the name of the method will change
					Method setValueMethod = target
							.getContext()
							.getClass()
							.getMethod(
									"set", new Class[] { String.class, Object.class }); //$NON-NLS-1$
					if (setValueMethod != null)
						setValueMethod.invoke(target.getContext(),
								new Object[] { target.getStep().getNameTest(),
										convertedValue });

				} catch (SecurityException e) {
					e.printStackTrace();
				} catch (NoSuchMethodException e) {
					// Nothing to do here
				} catch (IllegalArgumentException e) {
					// Nothing to do here
				} catch (IllegalAccessException e) {
					// Nothing to do here
				} catch (InvocationTargetException e) {
					// Nothing to do here
				}
				return;
			}
			EStructuralFeature feature = target.getContext().eClass()
					.getEStructuralFeature(target.getStep().getNameTest());
			if (feature != null) {
				switch (target.getStep().getAxis()) {
				case Axis.ATTRIBUTE:
					if (!(feature instanceof EAttribute))
						return;
					break;
				case Axis.REFERENCE:
					if (!(feature instanceof EReference))
						return;
					break;
				default:
					return;
				}

				// TODO : not enough, need to manage different cases List->List,
				// iteration on List, etc...
				if (newValue instanceof List) {
					if (feature.isMany())
						setFeature(target.getContext(), feature, newValue);
					else if (((List) newValue).size() == 1)
						setFeature(
								target.getContext(),
								feature, ((List) newValue).get(0));
					// TODO : check & test this, not sure at all ==> check side
					// effects
					else if (newValue == Collections.EMPTY_LIST)
						// target.getContext().eSet(feature, null);
						setFeature(target.getContext(), feature, null);
				}
			}
		}
	}

	protected void setFeature(EObject target, EStructuralFeature feature,
			Object value) {
		if (target != null && feature != null)
			target.eSet(feature, value);
	}

	public String getPropertyName(Object pointer) {
		if (pointer instanceof EMFPointer) {
			switch (((EMFPointer) pointer).getStep().getAxis()) {
			case Axis.ATTRIBUTE:
			case Axis.REFERENCE:
				return ((EMFPointer) pointer).getStep().getNameTest();
			default:
				return null;
			}
		}
		return null;
	}

	public Object getContext(Object pointer) {
		if (pointer instanceof EMFPointer)
			return ((EMFPointer) pointer).getContext();
		return null;
	}

	public Object getValue(Object pointer) {
		if (pointer == null)
			throw new NullPointerException("Pointer is null."); //$NON-NLS-1$
		if (!(pointer instanceof EMFPointer))
			throw new ClassCastException(
					"Pointer is not an instance of EMFPointer."); //$NON-NLS-1$
		return Evaluator.evaluate((EMFPointer) pointer);
	}

	public void setValue(Object pointer, Object newValue, IConverter converter) {
		if (pointer == null)
			throw new NullPointerException("Pointer is null."); //$NON-NLS-1$
		if (!(pointer instanceof EMFPointer))
			throw new ClassCastException(
					"Pointer is not an instance of EMFPointer."); //$NON-NLS-1$
		internalSetValue((EMFPointer) pointer, newValue, converter);
	}
	
	public void setValue2(Object pointer, Object newValue) {
		if (pointer == null)
			throw new NullPointerException("Pointer is null."); //$NON-NLS-1$
		if (!(pointer instanceof EMFPointer))
			throw new ClassCastException(
					"Pointer is not an instance of EMFPointer."); //$NON-NLS-1$
		internalSetValue2((EMFPointer) pointer, newValue);
	}

	// TODO very temporary, will be replaced.
	// @Deprecated
	public IConverter resolveConverter(EMFPointer target, Object newValue) {
		if (target.getStep().getAxis() == Axis.ATTRIBUTE
				|| target.getStep().getAxis() == Axis.REFERENCE
				&& target.getStep().getNameTest() != null
				&& !target.getStep().getNameTest().equals("") //$NON-NLS-1$
				&& !target.getStep().getNameTest().equals("*")) //$NON-NLS-1$ 
		{
			EStructuralFeature feature = target.getContext().eClass()
					.getEStructuralFeature(target.getStep().getNameTest());
			if (feature != null) {
				switch (target.getStep().getAxis()) {
				case Axis.ATTRIBUTE:
					if (!(feature instanceof EAttribute))
						return null;
					break;
				case Axis.REFERENCE:
					if (!(feature instanceof EReference))
						return null;
					break;
				default:
					return null;
				}

				// TODO : not enough, need to manage different cases List->List,
				// iteration on List, etc...
				if (newValue instanceof List) {
					if (feature.isMany())
						return resolveConverter(feature, newValue);
					else if (((List) newValue).size() == 1)
						return resolveConverter(feature,
								((List) newValue).get(0));
					// TODO : check & test this, not sure at all ==> check side
					// effects
					else if (newValue == Collections.EMPTY_LIST)
						return resolveConverter(feature, null);
				}
			}
		}
		return null;
	}

	protected IConverter resolveConverter(EStructuralFeature feature,
			Object newValue) {
		if (feature.getEType().getName().equals("EString")
				&& newValue instanceof Integer)
			return new IConverter() {

				public Object getToType() {
					return null;
				}

				public Object getFromType() {
					return null;
				}

				public Object convert(Object fromObject) {
					return ((Integer) fromObject).toString();
				}
			};
		else if (feature.getEType().getName().equals("EInt")
				&& newValue instanceof String)
			return new IConverter() {

				public Object getToType() {
					return null;
				}

				public Object getFromType() {
					return null;
				}

				public Object convert(Object fromObject) {
					return new Integer((String) fromObject);
				}
			};

		return null;
	}

	public IConverter resolveConverter(Object pointer, Object newValue) {
		if (pointer instanceof EMFPointer)
			return resolveConverter(((EMFPointer) pointer), newValue);
		return null;
	}

}
