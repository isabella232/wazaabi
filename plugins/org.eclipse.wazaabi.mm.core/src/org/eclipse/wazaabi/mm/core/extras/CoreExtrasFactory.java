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
package org.eclipse.wazaabi.mm.core.extras;

import org.eclipse.emf.ecore.EFactory;

/**
 * <!-- begin-user-doc -->
 * The <b>Factory</b> for the model.
 * It provides a create method for each non-abstract class of the model.
 * <!-- end-user-doc -->
 * @see org.eclipse.wazaabi.mm.core.extras.CoreExtrasPackage
 * @generated
 */
public interface CoreExtrasFactory extends EFactory {
	/**
     * The singleton instance of the factory.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	CoreExtrasFactory eINSTANCE = org.eclipse.wazaabi.mm.core.extras.impl.CoreExtrasFactoryImpl.init();

	/**
     * Returns a new object of class '<em>Text Cell Editor</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Text Cell Editor</em>'.
     * @generated
     */
	TextCellEditor createTextCellEditor();

	/**
     * Returns a new object of class '<em>Checkbox Cell Editor</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Checkbox Cell Editor</em>'.
     * @generated
     */
	CheckboxCellEditor createCheckboxCellEditor();

	/**
     * Returns the package supported by this factory.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the package supported by this factory.
     * @generated
     */
	CoreExtrasPackage getCoreExtrasPackage();

} //CoreExtrasFactory
