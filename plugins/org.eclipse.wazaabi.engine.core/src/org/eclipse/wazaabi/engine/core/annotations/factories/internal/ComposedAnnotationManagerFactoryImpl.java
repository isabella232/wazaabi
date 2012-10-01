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

package org.eclipse.wazaabi.engine.core.annotations.factories.internal;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.wazaabi.engine.core.annotations.factories.AnnotationManagerFactory;
import org.eclipse.wazaabi.engine.core.annotations.factories.ComposedAnnotationManagerFactory;
import org.eclipse.wazaabi.engine.core.annotations.managers.AnnotationManager;
import org.eclipse.wazaabi.mm.core.annotations.Annotation;

public class ComposedAnnotationManagerFactoryImpl implements
		ComposedAnnotationManagerFactory {

	private List<AnnotationManagerFactory> factories = new ArrayList<AnnotationManagerFactory>();

	public AnnotationManager createAnnotationManager(Annotation annotation) {
		for (AnnotationManagerFactory factory : factories)
			if (factory.isFactoryFor(annotation))
				return factory.createAnnotationManager(annotation);
		return null;
	}

	public boolean isFactoryFor(Annotation annotation) {
		for (AnnotationManagerFactory factory : factories)
			if (factory.isFactoryFor(annotation))
				return true;
		return false;
	}

	public void addAnnotationManagerFactory(AnnotationManagerFactory factory) {
		if (!factories.contains(factory)) {
			System.out.println("(style) adding " + factory);
			factories.add(factory);
		}
	}

	public void removeAnnotationManagerFactory(AnnotationManagerFactory factory) {
		factories.remove(factory);
		System.out.println("(style) removing " + factory);
	}

}
