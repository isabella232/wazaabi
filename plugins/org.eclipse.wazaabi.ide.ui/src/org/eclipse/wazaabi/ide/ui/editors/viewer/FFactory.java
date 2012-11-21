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

package org.eclipse.wazaabi.ide.ui.editors.viewer;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EClassifier;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EcorePackage;

public class FFactory {

	class Descriptor {

		private final Method method;
		private final Object containingInstance;
		private final EClassifier source;
		private final Class<?> target;
		private final Class<?> droppedType;

		public Class<?> getDroppedType() {
			return droppedType;
		}

		public Descriptor(Object containingInstance, Method method,
				EClassifier source, Class<?> target, Class<?> droppedType) {
			assert method != null;
			assert containingInstance != null;
			assert source != null;
			assert target != null;
			assert droppedType != null;
			this.source = source;
			this.containingInstance = containingInstance;
			this.method = method;
			this.droppedType = droppedType;
			this.target = target;
		}

		@Override
		public boolean equals(Object other) {
			if (other instanceof Descriptor) {
				Descriptor otherDesc = (Descriptor) other;
				return getSource().equals(otherDesc.getSource())
						&& getTarget().getName().equals(
								otherDesc.getTarget().getName())
						&& getDroppedType() == otherDesc.getDroppedType();
			}
			return false;
		}

		protected Object getContainingInstance() {
			return containingInstance;
		}

		protected Method getMethod() {
			return method;
		}

		public EClassifier getSource() {
			return source;
		}

		public Class<?> getTarget() {
			return target;
		}

		@Override
		public int hashCode() {
			return getSource().hashCode() + getTarget().getName().hashCode()
					+ getDroppedType().hashCode();
		}

	}

	private HashSet<Descriptor> descriptors = new HashSet<Descriptor>();

	public List<?> get(EObject targetUI, int index, EObject source,
			EClass droppedType, Object context) {
		if (targetUI == null || source == null || droppedType == null)
			return Collections.emptyList();
		Descriptor descriptor = getDescriptor(targetUI, index, source,
				droppedType, context);
		if (descriptor != null)
			try {
				List<?> result = (List<?>) descriptor.getMethod().invoke(
						descriptor.getContainingInstance(),
						new Object[] { targetUI, index, source, context });
				return result != null ? result : Collections.emptyList();
			} catch (IllegalAccessException e) {
				e.printStackTrace();
			} catch (IllegalArgumentException e) {
				e.printStackTrace();
			} catch (InvocationTargetException e) {
				e.printStackTrace();
			}
		return Collections.emptyList();

	}

	protected Descriptor getDescriptor(EObject targetUI, int index,
			EObject source, EClass droppedType, Object context) {
		Object sourceValue = source;
		if (source instanceof EAttribute)
			sourceValue = ((EAttribute) source).getEAttributeType();
		else
			sourceValue = EcorePackage.Literals.ECLASS;
		for (Descriptor descriptor : descriptors) {
			if (sourceValue.equals(descriptor.getSource())
					&& targetUI.eClass().getInstanceClass()
							.equals(descriptor.getTarget())
					&& droppedType.getInstanceClass() == descriptor
							.getDroppedType())
				return descriptor;
		}
		return null;
	}

	public void registerContainingInstance(Object instance) {
		if (instance == null)
			return;
		Method methods[] = instance.getClass().getDeclaredMethods();
		for (Method method : methods)
			registerMethod(method, instance);
	}

	protected void registerMethod(Method method, Object containingInstance) {
		if (method != null && containingInstance != null) {

			EAttributeMappingRule eAttributeAnnotation = (EAttributeMappingRule) method
					.getAnnotation(EAttributeMappingRule.class);
			EClassMappingRule eClassAnnotation = (EClassMappingRule) method
					.getAnnotation(EClassMappingRule.class);
			if (eAttributeAnnotation != null && eClassAnnotation != null)
				return;

			Class<?> target = null;
			Class<?> droppedType = null;
			Class<?> sourceType = null;
			EClassifier source = null;

			if (eAttributeAnnotation != null) {
				sourceType = EAttribute.class;
				target = eAttributeAnnotation.target();
				droppedType = eAttributeAnnotation.droppedType();
				source = getEdataType(eAttributeAnnotation.datatype());
				if (source == null)
					return;
			} else if (eClassAnnotation != null) {
				sourceType = EClass.class;
				target = eClassAnnotation.target();
				droppedType = eClassAnnotation.droppedType();
				source = EcorePackage.Literals.ECLASS;
			} else
				return;

			if (target != null || droppedType != null || sourceType != null
					|| source != null) {
				Class<?> parameterTypes[] = method.getParameterTypes();
				if (parameterTypes.length != 4)
					return;
				if (parameterTypes[0].equals(target)
						&& parameterTypes[1].equals(int.class)
						&& parameterTypes[2].equals(sourceType)
						&& parameterTypes[3].equals(Object.class)
						&& method.getReturnType().equals(List.class)) {
					Type returnType = method.getGenericReturnType();
					if (returnType instanceof ParameterizedType) {
						ParameterizedType type = (ParameterizedType) returnType;
						Type[] typeArguments = type.getActualTypeArguments();
						if (typeArguments.length == 1
								&& typeArguments[0].equals(droppedType)) {
							System.out.println("---> adding "
									+ method.getName() + ", lenght="
									+ descriptors.size());
							descriptors.add(new Descriptor(containingInstance,
									method, source, target, droppedType));
						}
					}
				}
			}
		}
	}

	public EDataType getEdataType(String name) {
		if (name == null || "".equals(name)) //$NON-NLS-1$
			return null;
		int idx = name.lastIndexOf('/');
		if (idx != -1) {
			String packageURI = name.substring(0, idx);
			return getEdataType(
					(EPackage) EPackage.Registry.INSTANCE.get(packageURI),
					name.substring(idx + 1));
		}
		return getEdataType(EcorePackage.eINSTANCE, name);
	}

	protected EDataType getEdataType(EPackage p, String name) {
		if (p == null || name == null || "".equals(name)) //$NON-NLS-1$
			return null;
		for (EClassifier eClassifier : p.getEClassifiers()) {
			if (eClassifier instanceof EDataType
					&& name.equals(((EDataType) eClassifier).getName()))
				return (EDataType) eClassifier;
		}
		return null;
	}

}