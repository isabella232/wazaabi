/*******************************************************************************
 * Copyright (c) 2008 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ui.model.parts;

import org.eclipse.wazaabi.mm.core.widgets.Container;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Page</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.ui.model.parts.Page#getUiSelector <em>Ui Selector</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.ui.model.parts.Page#getSelectionProcessor <em>Selection Processor</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.ui.model.parts.PartsPackage#getPage()
 * @model
 * @generated
 */
public interface Page extends Container {
	/**
	 * Returns the value of the '<em><b>Ui Selector</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Ui Selector</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Ui Selector</em>' attribute.
	 * @see #setUiSelector(String)
	 * @see org.eclipse.wazaabi.ui.model.parts.PartsPackage#getPage_UiSelector()
	 * @model
	 * @generated
	 */
	String getUiSelector();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.ui.model.parts.Page#getUiSelector <em>Ui Selector</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Ui Selector</em>' attribute.
	 * @see #getUiSelector()
	 * @generated
	 */
	void setUiSelector(String value);

	/**
	 * Returns the value of the '<em><b>Selection Processor</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Selection Processor</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Selection Processor</em>' attribute.
	 * @see #setSelectionProcessor(String)
	 * @see org.eclipse.wazaabi.ui.model.parts.PartsPackage#getPage_SelectionProcessor()
	 * @model
	 * @generated
	 */
	String getSelectionProcessor();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.ui.model.parts.Page#getSelectionProcessor <em>Selection Processor</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Selection Processor</em>' attribute.
	 * @see #getSelectionProcessor()
	 * @generated
	 */
	void setSelectionProcessor(String value);

} // Page
