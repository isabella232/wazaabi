/*******************************************************************************
 * Copyright (c) 2012 Olivier Moises
 * 
 * All rights reserved. This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License v1.0 which
 * accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors: Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.mapping.rules;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EClassifier;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EcorePackage;
import org.eclipse.wazaabi.ide.mapping.annotations.AbstractComponentMappingRule;
import org.eclipse.wazaabi.ide.mapping.annotations.EAttributeMappingRule;
import org.eclipse.wazaabi.ide.mapping.annotations.EClassMappingRule;
import org.eclipse.wazaabi.ide.mapping.annotations.EReferenceMappingRule;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MappingRuleManager {

	final static Logger logger = LoggerFactory
			.getLogger(MappingRuleManager.class);

	private HashSet<MappingMethodDescriptor> descriptors = new HashSet<MappingMethodDescriptor>();

	public List<?> get(EObject target, int index, EObject source,
			EClass droppedType, Object context) {
		return get(target, index, source, droppedType.getInstanceClass(),
				context);
	}

	public List<?> get(Object target, int index, Object source,
			Class<?> droppedType, Object context) {
		return get(target, null, index, source, droppedType, context);
	}

	public List<?> get(Object target, Class<?> targetType, int index,
			Object source, Class<?> droppedType, Object context) {
		System.out.println(target + " " + targetType + " " + index + " "
				+ source + " " + droppedType);
		if (target == null || source == null || droppedType == null)
			return Collections.emptyList();

		if (targetType == null)
			targetType = getTargetType(target);
		Object sourceType = getSourceType(source);
		if (targetType == null || sourceType == null)
			return Collections.emptyList();

		MappingMethodDescriptor descriptor = getDescriptor(targetType,
				sourceType, droppedType, context);
		if (descriptor != null)
			try {
				List<?> result = (List<?>) descriptor.getMethod().invoke(
						descriptor.getContainingInstance(),
						new Object[] { target, index, source, context });
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

	protected Class<?> getTargetType(Object target) {
		if (target != null) {
			if (target instanceof EObject)
				return ((EObject) target).eClass().getInstanceClass();
			else if (target instanceof Class<?>)
				return (Class<?>) target;
			else if (target instanceof Object)
				return target.getClass();
		}
		return null;

	}

	protected Object getSourceType(Object source) {
		if (source instanceof EAttribute)
			if (((EAttribute) source).getEAttributeType() instanceof EEnum)
				return EEnum.class;
			else
				return ((EAttribute) source).getEAttributeType()
						.getInstanceClass();
		else if (source instanceof EReference)
			return null; // TODO : not supported yet
		else if (source instanceof EClass)
			return EClass.class;
		else if (source instanceof EObject)
			return ((EObject) source).eClass().getInstanceClass();
		return null;
	}

	protected MappingMethodDescriptor getDescriptor(Class<?> target,
			Object sourceType, Class<?> droppedType, Object context) {
		System.out.println("sourceType=" + sourceType);
		for (MappingMethodDescriptor descriptor : descriptors) {
			if (sourceType.equals(descriptor.getSourceType())
					&& target.equals(descriptor.getTargetType())
					&& droppedType.equals(descriptor.getDroppedType()))
				return descriptor;
		}
		return null;
	}

	public List<MappingMethodDescriptor> getDescriptors(EObject targetUI,
			EClassifier sourceValue, EClass droppedType) {
		List<MappingMethodDescriptor> result = new ArrayList<MappingMethodDescriptor>();
		for (MappingMethodDescriptor descriptor : descriptors) {
			if (sourceValue.equals(descriptor.getSourceType())
					&& targetUI.eClass().getInstanceClass()
							.equals(descriptor.getTargetType())
					&& droppedType.getInstanceClass() == descriptor
							.getDroppedType())
				result.add(descriptor);
		}
		return result;
	}

	public void registerContainingInstance(Object instance) {
		if (instance == null)
			return;
		Method methods[] = instance.getClass().getDeclaredMethods();
		for (Method method : methods)
			registerMethod(method, instance);
	}

	protected void registerMethod(Method method, Object containingInstance) {
		if (method == null || containingInstance == null)
			return;

		EAttributeMappingRule eAttributeAnnotation = (EAttributeMappingRule) method
				.getAnnotation(EAttributeMappingRule.class);
		EReferenceMappingRule eReferenceAnnotation = (EReferenceMappingRule) method
				.getAnnotation(EReferenceMappingRule.class);
		EClassMappingRule eClassAnnotation = (EClassMappingRule) method
				.getAnnotation(EClassMappingRule.class);
		AbstractComponentMappingRule abstractComponentAnnotation = (AbstractComponentMappingRule) method
				.getAnnotation(AbstractComponentMappingRule.class);
		if (eAttributeAnnotation != null && eReferenceAnnotation != null
				&& eClassAnnotation != null
				&& abstractComponentAnnotation == null)
			return;

		Class<?> sourceType = null;

		if (abstractComponentAnnotation != null) {
			registerMethodForAbstractComponentMappingRule(method,
					containingInstance);
		} else if (eAttributeAnnotation != null) {
			sourceType = getDataType(eAttributeAnnotation.datatype())
					.getInstanceClass();
			registerMethodForEAttributeMappingRule(method, containingInstance,
					sourceType);
		} else if (eReferenceAnnotation != null) {
			sourceType = EReference.class;
			registerMethodForEReferenceMappingRule(method, containingInstance,
					sourceType);
		} else if (eClassAnnotation != null) {
			sourceType = EClass.class;
			registerMethodForEClassMappingRule(method, containingInstance,
					sourceType);
		}
	}

	protected void registerMethodForEAttributeMappingRule(Method method,
			Object containingInstance, Class<?> sourceType) {
		if (sourceType != null) {
			Class<?> parameterTypes[] = method.getParameterTypes();
			if (parameterTypes.length != 4)
				return;
			if (parameterTypes[1].equals(int.class)
					&& parameterTypes[2].equals(EAttribute.class)
					&& parameterTypes[3].equals(Object.class)
					&& method.getReturnType().equals(List.class)) {
				Type returnType = method.getGenericReturnType();
				if (returnType instanceof ParameterizedType) {
					ParameterizedType type = (ParameterizedType) returnType;
					Type[] typeArguments = type.getActualTypeArguments();
					if (typeArguments.length == 1
							&& typeArguments[0] instanceof Class<?>) {
						logger.debug("Adding {}.{}", new Object[] {
								containingInstance, method.getName() });
						descriptors
								.add(new MappingMethodDescriptor(
										containingInstance, method, sourceType,
										parameterTypes[0],
										(Class<?>) typeArguments[0]));
					}
				}
			}
		}
	}

	protected void registerMethodForEReferenceMappingRule(Method method,
			Object containingInstance, Class<?> sourceType) {
		System.out.println("registerMethod " + method + " "
				+ containingInstance + " ++++++++++++++++++++++++++++++");
		Class<?> parameterTypes[] = method.getParameterTypes();
		if (parameterTypes.length != 4)
			return;
		if (parameterTypes[1].equals(int.class)
				&& parameterTypes[2].equals(EReference.class)
				&& parameterTypes[3].equals(Object.class)
				&& method.getReturnType().equals(List.class)) {
			Type returnType = method.getGenericReturnType();
			if (returnType instanceof ParameterizedType) {
				ParameterizedType type = (ParameterizedType) returnType;
				Type[] typeArguments = type.getActualTypeArguments();
				if (typeArguments.length == 1
						&& typeArguments[0] instanceof Class<?>) {
					logger.debug("Adding {}.{}", new Object[] {
							containingInstance, method.getName() });
					descriptors.add(new MappingMethodDescriptor(
							containingInstance, method, sourceType,
							parameterTypes[0], (Class<?>) typeArguments[0]));
				}
			}
		}
	}

	protected void registerMethodForEClassMappingRule(Method method,
			Object containingInstance, Class<?> sourceType) {
		if (sourceType != null) {
			Class<?> parameterTypes[] = method.getParameterTypes();
			if (parameterTypes.length != 4)
				return;
			if (parameterTypes[1].equals(int.class)
					&& parameterTypes[2].equals(EClass.class)
					&& parameterTypes[3].equals(Object.class)
					&& method.getReturnType().equals(List.class)) {
				Type returnType = method.getGenericReturnType();
				if (returnType instanceof ParameterizedType) {
					ParameterizedType type = (ParameterizedType) returnType;
					Type[] typeArguments = type.getActualTypeArguments();
					if (typeArguments.length == 1
							&& typeArguments[0] instanceof Class<?>) {
						logger.debug("Adding {}.{}", new Object[] {
								containingInstance, method.getName() });
						descriptors
								.add(new MappingMethodDescriptor(
										containingInstance, method, sourceType,
										parameterTypes[0],
										(Class<?>) typeArguments[0]));
					}
				}
			}
		}
	}

	protected void registerMethodForAbstractComponentMappingRule(Method method,
			Object containingInstance) {
		Class<?> parameterTypes[] = method.getParameterTypes();
		if (parameterTypes.length != 4)
			return;

		if (parameterTypes[1].equals(int.class)
				&& AbstractComponent.class.isAssignableFrom(parameterTypes[2])
				&& parameterTypes[2].isInterface()
				&& parameterTypes[3].equals(Object.class)
				&& method.getReturnType().equals(List.class)) {
			Type returnType = method.getGenericReturnType();
			if (returnType instanceof ParameterizedType) {
				ParameterizedType type = (ParameterizedType) returnType;
				Type[] typeArguments = type.getActualTypeArguments();
				if (typeArguments.length == 1
						&& typeArguments[0] instanceof Class<?>) {
					logger.debug("Adding {}.{}", new Object[] {
							containingInstance, method.getName() });
					descriptors.add(new MappingMethodDescriptor(
							containingInstance, method, parameterTypes[2],
							parameterTypes[0], (Class<?>) typeArguments[0]));
				}
			}
		}
	}

	public EClassifier getDataType(String name) {
		if (name == null || "".equals(name)) //$NON-NLS-1$
			return null;
		int idx = name.lastIndexOf('/');
		if (idx != -1) {
			String packageURI = name.substring(0, idx);
			return getDataType(
					(EPackage) EPackage.Registry.INSTANCE.get(packageURI),
					name.substring(idx + 1));
		}
		return getDataType(EcorePackage.eINSTANCE, name);
	}

	protected EClassifier getDataType(EPackage p, String name) {
		if (p == null || name == null || "".equals(name)) //$NON-NLS-1$
			return null;
		for (EClassifier eClassifier : p.getEClassifiers()) {
			if (name.equals(((EClassifier) eClassifier).getName())) {
				if (eClassifier instanceof EDataType
						|| (eClassifier instanceof EClass && EcorePackage.Literals.EDATA_TYPE
								.isSuperTypeOf((EClass) eClassifier)))
					// TODO: not really sure about the second predicate
					return eClassifier;
			}
		}
		return null;
	}

}