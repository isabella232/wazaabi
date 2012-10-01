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

package org.eclipse.wazaabi.engine.core.annotations.managers;

import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.wazaabi.engine.edp.PathException;
import org.eclipse.wazaabi.mm.core.annotations.AnnotationContent;

public class InitAnnotationManager extends AnnotationManager {

	protected void processAnnotation() {

		if (getAnnotation() == null)
			return;
		EStructuralFeature feature = null;
		String type = null;
		String value = null;

		EObject model = null;
		if (getHost() != null && getHost().getModel() instanceof EObject)
			model = (EObject) getHost().getModel();

		for (AnnotationContent content : getAnnotation().getContents()) {
			if ("feature-name".equals(content.getKey())) { //$NON-NLS-1$
				feature = model.eClass().getEStructuralFeature(
						content.getValue());
				if (feature == null)
					break;
			} else if ("type".equals(content.getKey())) //$NON-NLS-1$
				type = content.getValue();
			else if ("value".equals(content.getKey())) //$NON-NLS-1$
				value = content.getValue();

			if (feature == null)
				return;
		}
		// TODO temporary code
		if ("locationpath".equals(type)) {
			try {
				List<?> pointers = getHost().getPointersEvaluator()
						.selectPointers(model, value);
				if (pointers.size() == 1) {
					Object result = getHost().getPointersEvaluator().getValue(
							pointers.get(0));
					if (result instanceof List<?>) {
						if (((List<?>) result).size() == 0)
							result = null;
						else if (((List<?>) result).size() == 1)
							result = ((List<?>) result).get(0);
					}
					model.eSet(feature, result);
				}
			} catch (PathException e) {
				System.err.println(e.getMessage()); // TODO : log that
			}
		}
	}

}
