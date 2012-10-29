/**
 *  Copyright (c) 2008 Olivier Moises
 * 
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the Eclipse Public License v1.0
 *  which accompanies this distribution, and is available at
 *  http://www.eclipse.org/legal/epl-v10.html
 *  
 *  Contributors:
 *    Olivier Moises- initial API and implementation
 */
package org.eclipse.wazaabi.mm.swt.styles.impl;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.impl.EFactoryImpl;

import org.eclipse.emf.ecore.plugin.EcorePlugin;

import org.eclipse.wazaabi.mm.swt.styles.*;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Factory</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class SWTStylesFactoryImpl extends EFactoryImpl implements SWTStylesFactory {
	/**
	 * Creates the default factory implementation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static SWTStylesFactory init() {
		try {
			SWTStylesFactory theSWTStylesFactory = (SWTStylesFactory)EPackage.Registry.INSTANCE.getEFactory("http://www.wazaabi.org/swt/styles"); 
			if (theSWTStylesFactory != null) {
				return theSWTStylesFactory;
			}
		}
		catch (Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}
		return new SWTStylesFactoryImpl();
	}

	/**
	 * Creates an instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public SWTStylesFactoryImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EObject create(EClass eClass) {
		switch (eClass.getClassifierID()) {
			case SWTStylesPackage.ROW_LAYOUT_RULE: return createRowLayoutRule();
			case SWTStylesPackage.ROW_DATA_RULE: return createRowDataRule();
			case SWTStylesPackage.GRID_LAYOUT_RULE: return createGridLayoutRule();
			case SWTStylesPackage.GRID_DATA_RULE: return createGridDataRule();
			case SWTStylesPackage.FILL_LAYOUT_RULE: return createFillLayoutRule();
			case SWTStylesPackage.ATTACHMENT_TO_SIBLING: return createAttachmentToSibling();
			case SWTStylesPackage.ATTACHMENT_TO_CONTAINER: return createAttachmentToContainer();
			case SWTStylesPackage.FORM_DATA_RULE: return createFormDataRule();
			case SWTStylesPackage.FORM_LAYOUT_RULE: return createFormLayoutRule();
			default:
				throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object createFromString(EDataType eDataType, String initialValue) {
		switch (eDataType.getClassifierID()) {
			case SWTStylesPackage.GRID_DATA_ALIGNMENT:
				return createGridDataAlignmentFromString(eDataType, initialValue);
			case SWTStylesPackage.TO_SIBLING_ALIGNMENT:
				return createToSiblingAlignmentFromString(eDataType, initialValue);
			default:
				throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String convertToString(EDataType eDataType, Object instanceValue) {
		switch (eDataType.getClassifierID()) {
			case SWTStylesPackage.GRID_DATA_ALIGNMENT:
				return convertGridDataAlignmentToString(eDataType, instanceValue);
			case SWTStylesPackage.TO_SIBLING_ALIGNMENT:
				return convertToSiblingAlignmentToString(eDataType, instanceValue);
			default:
				throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public RowLayoutRule createRowLayoutRule() {
		RowLayoutRuleImpl rowLayoutRule = new RowLayoutRuleImpl();
		return rowLayoutRule;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public RowDataRule createRowDataRule() {
		RowDataRuleImpl rowDataRule = new RowDataRuleImpl();
		return rowDataRule;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public GridLayoutRule createGridLayoutRule() {
		GridLayoutRuleImpl gridLayoutRule = new GridLayoutRuleImpl();
		return gridLayoutRule;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public GridDataRule createGridDataRule() {
		GridDataRuleImpl gridDataRule = new GridDataRuleImpl();
		return gridDataRule;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public FillLayoutRule createFillLayoutRule() {
		FillLayoutRuleImpl fillLayoutRule = new FillLayoutRuleImpl();
		return fillLayoutRule;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public AttachmentToSibling createAttachmentToSibling() {
		AttachmentToSiblingImpl attachmentToSibling = new AttachmentToSiblingImpl();
		return attachmentToSibling;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public AttachmentToContainer createAttachmentToContainer() {
		AttachmentToContainerImpl attachmentToContainer = new AttachmentToContainerImpl();
		return attachmentToContainer;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public FormDataRule createFormDataRule() {
		FormDataRuleImpl formDataRule = new FormDataRuleImpl();
		return formDataRule;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public FormLayoutRule createFormLayoutRule() {
		FormLayoutRuleImpl formLayoutRule = new FormLayoutRuleImpl();
		return formLayoutRule;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public GridDataAlignment createGridDataAlignmentFromString(EDataType eDataType, String initialValue) {
		GridDataAlignment result = GridDataAlignment.get(initialValue);
		if (result == null) throw new IllegalArgumentException("The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");
		return result;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String convertGridDataAlignmentToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ToSiblingAlignment createToSiblingAlignmentFromString(EDataType eDataType, String initialValue) {
		ToSiblingAlignment result = ToSiblingAlignment.get(initialValue);
		if (result == null) throw new IllegalArgumentException("The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");
		return result;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String convertToSiblingAlignmentToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public SWTStylesPackage getSWTStylesPackage() {
		return (SWTStylesPackage)getEPackage();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @deprecated
	 * @generated
	 */
	@Deprecated
	public static SWTStylesPackage getPackage() {
		return SWTStylesPackage.eINSTANCE;
	}

} //SWTStylesFactoryImpl
