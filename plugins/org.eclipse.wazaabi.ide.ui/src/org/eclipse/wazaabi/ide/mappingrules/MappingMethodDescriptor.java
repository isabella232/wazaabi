/*******************************************************************************
 * Copyright (c) 2013 Olivier Moises
 * 
 * All rights reserved. This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License v1.0 which
 * accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors: Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.mappingrules;

import java.lang.reflect.Method;

public class MappingMethodDescriptor {

	private final Method method;
	private final Object containingInstance;
	private final Class<?> sourceType;
	private final Class<?> targetType;
	private final Class<?> droppedType;

	public Class<?> getDroppedType() {
		return droppedType;
	}

	public MappingMethodDescriptor(Object containingInstance, Method method,
			Class<?> sourceType, Class<?> targetType, Class<?> droppedType) {
		assert method != null;
		assert containingInstance != null;
		assert sourceType != null;
		assert targetType != null;
		assert droppedType != null;
		this.sourceType = sourceType;
		this.containingInstance = containingInstance;
		this.method = method;
		this.droppedType = droppedType;
		this.targetType = targetType;
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof MappingMethodDescriptor) {
			MappingMethodDescriptor otherDesc = (MappingMethodDescriptor) other;
			return getSourceType().equals(otherDesc.getSourceType())
					&& getTargetType().getName().equals(
							otherDesc.getTargetType().getName())
					&& getDroppedType() == otherDesc.getDroppedType()
					&& getMethod().getName().equals(
							otherDesc.getMethod().getName());
		}
		return false;
	}

	public Object getContainingInstance() {
		return containingInstance;
	}

	public Method getMethod() {
		return method;
	}

	public Class<?> getSourceType() {
		return sourceType;
	}

	public Class<?> getTargetType() {
		return targetType;
	}

	@Override
	public int hashCode() {
		return getSourceType().hashCode()
				+ getTargetType().getName().hashCode()
				+ getDroppedType().hashCode()
				+ getMethod().getName().hashCode();
	}

}
