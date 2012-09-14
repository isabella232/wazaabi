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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.wazaabi.engine.locationpaths.model.Axis;
import org.eclipse.wazaabi.engine.locationpaths.model.EMFPointer;
import org.eclipse.wazaabi.engine.locationpaths.model.LocationPath;
import org.eclipse.wazaabi.engine.locationpaths.model.Step;

public class LocationSelector {

	/**
	 * Returns a list of <code>EMFPointer</code>s built by walking a path from
	 * the context.
	 * 
	 * @param context
	 *            The starting EMFPointer where to apply the steps.
	 * @param path
	 *            The path to apply.
	 * 
	 * @return A list of <code>EMFPointer</code>s.
	 * 
	 * @see EMFPointer
	 */
	public static List select(EObject context, String locationPath) {
		LocationPath parsedPath = LocationParser.parse(locationPath);
		return select(context, parsedPath);
	}

	/**
	 * Returns a list of <code>EMFPointer</code>s built by applying a
	 * <code>LocationPath</code> onto the context.
	 * 
	 * @param context
	 *            The starting EMFPointer where to apply the steps.
	 * @param path
	 *            A <code>LocationPath</code>
	 * 
	 * @return A list of <code>EMFPointer</code>s.
	 * 
	 * @see LocationPath
	 * @see EMFPointer
	 */
	public static List select(EObject context, LocationPath path) {
		if (path == null)
			return Collections.emptyList();
		if (path.getInitialContext() != null)
			return select(createEMFPointer((EObject) path.getInitialContext()
					.resolveContext()), path.getSteps());
		return select(createEMFPointer(context), path.getSteps());
	}

	/**
	 * Returns a list of <code>EMFPointer</code>s built by applying a list of
	 * <code>Step</code>s onto the context.
	 * 
	 * @param context
	 *            The starting EMFPointer where to apply the steps.
	 * @param steps
	 *            A list of <code>Step</code>s.
	 * 
	 * @return A list of <code>EMFPointer</code>s.
	 * 
	 * @see Step
	 * @see EMFPointer
	 */
	protected static List select(EMFPointer context, List steps) {

		List contextChildren = new ArrayList(1);
		contextChildren.add(context.getContext());

		for (int i = 0; i < steps.size() - 1; i++) {
			Step step = (Step) steps.get(i);
			List stepChildren = new ArrayList(contextChildren.size() * 3);
			Iterator contextChildrenIterator = contextChildren.iterator();
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

		List result = new ArrayList(contextChildren.size());
		if (steps.isEmpty()) {
			result.add(context);
			return result;
		}
		Step step = (Step) steps.get(steps.size() - 1);
		Iterator contextChildrenIterator = contextChildren.iterator();
		while (contextChildrenIterator.hasNext()) {
			Object contextChild = contextChildrenIterator.next();
			List returnedStepChildren = Collections.emptyList();
			if (Axis.ATTRIBUTE != step.getAxis()
					&& Axis.REFERENCE != step.getAxis())
				returnedStepChildren = Evaluator.evaluate(contextChild, step);
			if (returnedStepChildren.isEmpty()) {
				EMFPointer pointer = createEMFPointer((EObject) contextChild,
						step);
				if (pointer != null && !result.contains(pointer))
					result.add(pointer);
			} else {
				Iterator returnedStepChildrenIterator = returnedStepChildren
						.iterator();
				while (returnedStepChildrenIterator.hasNext()) {
					Object value = returnedStepChildrenIterator.next();
					EMFPointer pointer = null;
					if (value instanceof EObject)
						pointer = createEMFPointer((EObject) value);
					else
						throw new RuntimeException();
					// TODO : don't know what to do in this case
					if (pointer != null && !result.contains(pointer))
						result.add(pointer);
				}
			}
		}
		return result;
	}

	private static EMFPointer createEMFPointer(EObject context, Step step) {
		if (step == null)
			return createEMFPointer(context);
		EMFPointer pointer = new EMFPointer();
		pointer.setContext(context);
		pointer.setStep(step);
		return pointer;
	}

	private static EMFPointer createEMFPointer(EObject object) {
		EMFPointer pointer = new EMFPointer();
		pointer.setContext(object);
		Step step = new Step();
		// TODO merge with existing SELF_STEP creation tool
		step.setAxis(Axis.SELF);
		pointer.setStep(step);

		return pointer;
	}

}
