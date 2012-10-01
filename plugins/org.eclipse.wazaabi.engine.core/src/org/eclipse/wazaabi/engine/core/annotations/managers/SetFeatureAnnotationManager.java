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
import org.eclipse.wazaabi.engine.core.editparts.AbstractWidgetEditPart;
import org.eclipse.wazaabi.engine.edp.PathException;
import org.eclipse.wazaabi.mm.core.annotations.Annotation;
import org.eclipse.wazaabi.mm.core.annotations.AnnotationContent;

public class SetFeatureAnnotationManager extends AnnotationManager {

	public static final String SET_FEATURE_ANNOTATION_SOURCE = "http://www.wazaabi.org/set-feature"; //$NON-NLS-1$
	protected static final String FEATURE_NAME_KEY = "feature-name"; //$NON-NLS-1$
	protected static final String TYPE_KEY = "type"; //$NON-NLS-1$
	protected static final String VALUE_KEY = "value"; //$NON-NLS-1$
	protected static final String LOCATION_PATH_TYPE = "locationpath"; //$NON-NLS-1$

	public SetFeatureAnnotationManager(Annotation annotation) {
		super(annotation);
	}

	public void processAnnotation(AbstractWidgetEditPart host) {

		if (getAnnotation() == null)
			return;
		EStructuralFeature feature = null;
		String type = null;
		String value = null;

		EObject model = null;

		if (host != null && host.getModel() instanceof EObject)
			model = (EObject) host.getModel();

		for (AnnotationContent content : getAnnotation().getContents()) {
			if (FEATURE_NAME_KEY.equals(content.getKey())) {
				feature = model.eClass().getEStructuralFeature(
						content.getValue());
				if (feature == null)
					break;
			} else if (TYPE_KEY.equals(content.getKey()))
				type = content.getValue();
			else if (VALUE_KEY.equals(content.getKey()))
				value = content.getValue();
			if (feature == null)
				return;
		}

		if (LOCATION_PATH_TYPE.equals(type)) {
			try {
				List<?> pointers = host.getPointersEvaluator().selectPointers(
						model, value);
				if (pointers.size() == 1) {
					Object result = host.getPointersEvaluator().getValue(
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

	protected boolean checkSourceCorrectness(String source) {
		return SET_FEATURE_ANNOTATION_SOURCE.equals(source);
	}

}
