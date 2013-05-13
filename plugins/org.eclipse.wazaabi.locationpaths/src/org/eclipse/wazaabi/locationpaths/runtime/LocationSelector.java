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

package org.eclipse.wazaabi.locationpaths.runtime;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.wazaabi.locationpaths.model.Axis;
import org.eclipse.wazaabi.locationpaths.model.EMFPointer;
import org.eclipse.wazaabi.locationpaths.model.JavaObjectPointer;
import org.eclipse.wazaabi.locationpaths.model.LocationPath;
import org.eclipse.wazaabi.locationpaths.model.Pointer;
import org.eclipse.wazaabi.locationpaths.model.Step;

public class LocationSelector {

	/**
	 * Returns a list of <code>Pointer</code>s built by walking a path from the
	 * context.
	 * 
	 * @param context
	 *            The starting Pointer where to apply the steps.
	 * @param path
	 *            The path to apply.
	 * 
	 * @return A list of <code>Pointer</code>s.
	 * 
	 * @see Pointer
	 */
	public static List<Pointer<?>> select(Object context, String locationPath) {
		LocationPath parsedPath = LocationParser.parse(locationPath);
		return select(context, parsedPath);
	}

	/**
	 * Returns a list of <code>Pointer</code>s built by applying a
	 * <code>LocationPath</code> onto the context.
	 * 
	 * @param context
	 *            The starting Pointer where to apply the steps.
	 * @param path
	 *            A <code>LocationPath</code>
	 * 
	 * @return A list of <code>Pointer</code>s.
	 * 
	 * @see LocationPath
	 * @see Pointer
	 */
	public static List<Pointer<?>> select(Object context, LocationPath path) {
		if (path == null)
			return Collections.emptyList();
		if (path.getInitialContext() != null)
			return select(createPointer((EObject) path.getInitialContext()
					.resolveContext()), path.getSteps());
		return select(createPointer(context), path.getSteps());
	}

	/**
	 * Returns a list of <code>Pointer</code>s built by applying a list of
	 * <code>Step</code>s onto the context.
	 * 
	 * @param context
	 *            The starting Pointer where to apply the steps.
	 * @param steps
	 *            A list of <code>Step</code>s.
	 * 
	 * @return A list of <code>Pointer</code>s.
	 * 
	 * @see Step
	 * @see Pointer
	 */
	protected static List<Pointer<?>> select(Pointer<?> context,
			List<Step> steps) {

		if (context == null)
			return Collections.emptyList();
		List<Object> contextChildren = new ArrayList<Object>(1);
		contextChildren.add(context.getContext());

		for (int i = 0; i < steps.size() - 1; i++) {
			Step step = steps.get(i);
			List<Object> stepChildren = new ArrayList<Object>(
					contextChildren.size() * 3);
			Iterator<?> contextChildrenIterator = contextChildren.iterator();
			while (contextChildrenIterator.hasNext()) {
				Object contextChild = contextChildrenIterator.next();
				Iterator returnedStepChildrenIterator = Evaluator.evaluate(
						contextChild, step).iterator();
				while (returnedStepChildrenIterator.hasNext()) {
					Object stepChild = returnedStepChildrenIterator.next();
					if (!stepChildren.contains(stepChild))
						stepChildren.add(stepChild);
				}
			}
			contextChildren = stepChildren;
		}

		List<Pointer<?>> result = new ArrayList<Pointer<?>>(
				contextChildren.size());
		if (steps.isEmpty()) {
			result.add(context);
			return result;
		}
		Step step = steps.get(steps.size() - 1);
		Iterator contextChildrenIterator = contextChildren.iterator();
		while (contextChildrenIterator.hasNext()) {
			Object contextChild = contextChildrenIterator.next();
			List returnedStepChildren = Collections.emptyList();
			if (Axis.ATTRIBUTE != step.getAxis()
					&& Axis.REFERENCE != step.getAxis())
				returnedStepChildren = Evaluator.evaluate(contextChild, step);
			if (returnedStepChildren.isEmpty()) {
				Pointer pointer = createPointer(contextChild, step);
				if (pointer != null && !result.contains(pointer))
					result.add(pointer);
			} else {
				Iterator returnedStepChildrenIterator = returnedStepChildren
						.iterator();
				while (returnedStepChildrenIterator.hasNext()) {
					Object value = returnedStepChildrenIterator.next();
					Pointer<?> pointer = createPointer(value);

					// TODO : don't know what to do in this case
					if (pointer != null && !result.contains(pointer))
						result.add(pointer);
				}
			}
		}
		return result;
	}

	private static Pointer<?> createPointer(Object context, Step step) {
		if (step == null)
			return createPointer(context);
		Pointer<?> pointer = null;
		if (context instanceof EObject) {
			pointer = new EMFPointer();
			((EMFPointer) pointer).setContext((EObject) context);
		} else if (context != null) {
			pointer = new JavaObjectPointer();
			((JavaObjectPointer) pointer).setContext(context);
		}
		if (pointer != null)
			pointer.setStep(step);
		return pointer;
	}

	private static Pointer<?> createPointer(Object context) {
		Step step = new Step();
		// TODO merge with existing SELF_STEP creation tool
		step.setAxis(Axis.SELF);
		return createPointer(context, step);
	}

}
