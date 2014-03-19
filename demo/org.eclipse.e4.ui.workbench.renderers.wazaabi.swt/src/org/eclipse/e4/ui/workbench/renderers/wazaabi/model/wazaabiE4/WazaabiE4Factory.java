/**
 *  Copyright (c) 2014 Olivier Moises
 * 
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the Eclipse Public License v1.0
 *  which accompanies this distribution, and is available at
 *  http://www.eclipse.org/legal/epl-v10.html
 *  
 *  Contributors:
 *    Olivier Moises- initial API and implementation
 */
package org.eclipse.e4.ui.workbench.renderers.wazaabi.model.wazaabiE4;

import org.eclipse.emf.ecore.EFactory;

/**
 * <!-- begin-user-doc -->
 * The <b>Factory</b> for the model.
 * It provides a create method for each non-abstract class of the model.
 * <!-- end-user-doc -->
 * @see org.eclipse.e4.ui.workbench.renderers.wazaabi.model.wazaabiE4.WazaabiE4Package
 * @generated
 */
public interface WazaabiE4Factory extends EFactory {
	/**
	 * The singleton instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	WazaabiE4Factory eINSTANCE = org.eclipse.e4.ui.workbench.renderers.wazaabi.model.wazaabiE4.impl.WazaabiE4FactoryImpl.init();

	/**
	 * Returns a new object of class '<em>Wazaabi Part</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Wazaabi Part</em>'.
	 * @generated
	 */
	WazaabiPart createWazaabiPart();

	/**
	 * Returns the package supported by this factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the package supported by this factory.
	 * @generated
	 */
	WazaabiE4Package getWazaabiE4Package();

} //WazaabiE4Factory
