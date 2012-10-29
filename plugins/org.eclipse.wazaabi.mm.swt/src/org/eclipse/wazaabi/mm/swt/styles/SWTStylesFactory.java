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
package org.eclipse.wazaabi.mm.swt.styles;

import org.eclipse.emf.ecore.EFactory;

/**
 * <!-- begin-user-doc -->
 * The <b>Factory</b> for the model.
 * It provides a create method for each non-abstract class of the model.
 * <!-- end-user-doc -->
 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage
 * @generated
 */
public interface SWTStylesFactory extends EFactory {
	/**
	 * The singleton instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	SWTStylesFactory eINSTANCE = org.eclipse.wazaabi.mm.swt.styles.impl.SWTStylesFactoryImpl.init();

	/**
	 * Returns a new object of class '<em>Row Layout Rule</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Row Layout Rule</em>'.
	 * @generated
	 */
	RowLayoutRule createRowLayoutRule();

	/**
	 * Returns a new object of class '<em>Row Data Rule</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Row Data Rule</em>'.
	 * @generated
	 */
	RowDataRule createRowDataRule();

	/**
	 * Returns a new object of class '<em>Grid Layout Rule</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Grid Layout Rule</em>'.
	 * @generated
	 */
	GridLayoutRule createGridLayoutRule();

	/**
	 * Returns a new object of class '<em>Grid Data Rule</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Grid Data Rule</em>'.
	 * @generated
	 */
	GridDataRule createGridDataRule();

	/**
	 * Returns a new object of class '<em>Fill Layout Rule</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Fill Layout Rule</em>'.
	 * @generated
	 */
	FillLayoutRule createFillLayoutRule();

	/**
	 * Returns a new object of class '<em>Attachment To Sibling</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Attachment To Sibling</em>'.
	 * @generated
	 */
	AttachmentToSibling createAttachmentToSibling();

	/**
	 * Returns a new object of class '<em>Attachment To Container</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Attachment To Container</em>'.
	 * @generated
	 */
	AttachmentToContainer createAttachmentToContainer();

	/**
	 * Returns a new object of class '<em>Form Data Rule</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Form Data Rule</em>'.
	 * @generated
	 */
	FormDataRule createFormDataRule();

	/**
	 * Returns a new object of class '<em>Form Layout Rule</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Form Layout Rule</em>'.
	 * @generated
	 */
	FormLayoutRule createFormLayoutRule();

	/**
	 * Returns the package supported by this factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the package supported by this factory.
	 * @generated
	 */
	SWTStylesPackage getSWTStylesPackage();

} //SWTStylesFactory
