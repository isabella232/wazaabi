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

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;

/**
 * <!-- begin-user-doc -->
 * The <b>Package</b> for the model.
 * It contains accessors for the meta objects to represent
 * <ul>
 *   <li>each class,</li>
 *   <li>each feature of each class,</li>
 *   <li>each enum,</li>
 *   <li>and each data type</li>
 * </ul>
 * <!-- end-user-doc -->
 * @see org.eclipse.wazaabi.mm.core.extras.CoreExtrasFactory
 * @model kind="package"
 * @generated
 */
public interface CoreExtrasPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNAME = "extras";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_URI = "http://www.wazaabi.org/core/extras";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_PREFIX = "wce";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	CoreExtrasPackage eINSTANCE = org.eclipse.wazaabi.mm.core.extras.impl.CoreExtrasPackageImpl.init();

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.extras.impl.CellEditorImpl <em>Cell Editor</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.extras.impl.CellEditorImpl
	 * @see org.eclipse.wazaabi.mm.core.extras.impl.CoreExtrasPackageImpl#getCellEditor()
	 * @generated
	 */
	int CELL_EDITOR = 0;

	/**
	 * The number of structural features of the '<em>Cell Editor</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CELL_EDITOR_FEATURE_COUNT = 0;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.extras.impl.TextCellEditorImpl <em>Text Cell Editor</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.extras.impl.TextCellEditorImpl
	 * @see org.eclipse.wazaabi.mm.core.extras.impl.CoreExtrasPackageImpl#getTextCellEditor()
	 * @generated
	 */
	int TEXT_CELL_EDITOR = 1;

	/**
	 * The number of structural features of the '<em>Text Cell Editor</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TEXT_CELL_EDITOR_FEATURE_COUNT = CELL_EDITOR_FEATURE_COUNT + 0;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.extras.impl.CheckboxCellEditorImpl <em>Checkbox Cell Editor</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.extras.impl.CheckboxCellEditorImpl
	 * @see org.eclipse.wazaabi.mm.core.extras.impl.CoreExtrasPackageImpl#getCheckboxCellEditor()
	 * @generated
	 */
	int CHECKBOX_CELL_EDITOR = 2;

	/**
	 * The number of structural features of the '<em>Checkbox Cell Editor</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CHECKBOX_CELL_EDITOR_FEATURE_COUNT = CELL_EDITOR_FEATURE_COUNT + 0;


	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.extras.CellEditor <em>Cell Editor</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Cell Editor</em>'.
	 * @see org.eclipse.wazaabi.mm.core.extras.CellEditor
	 * @generated
	 */
	EClass getCellEditor();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.extras.TextCellEditor <em>Text Cell Editor</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Text Cell Editor</em>'.
	 * @see org.eclipse.wazaabi.mm.core.extras.TextCellEditor
	 * @generated
	 */
	EClass getTextCellEditor();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.extras.CheckboxCellEditor <em>Checkbox Cell Editor</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Checkbox Cell Editor</em>'.
	 * @see org.eclipse.wazaabi.mm.core.extras.CheckboxCellEditor
	 * @generated
	 */
	EClass getCheckboxCellEditor();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	CoreExtrasFactory getCoreExtrasFactory();

	/**
	 * <!-- begin-user-doc -->
	 * Defines literals for the meta objects that represent
	 * <ul>
	 *   <li>each class,</li>
	 *   <li>each feature of each class,</li>
	 *   <li>each enum,</li>
	 *   <li>and each data type</li>
	 * </ul>
	 * <!-- end-user-doc -->
	 * @generated
	 */
	interface Literals {
		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.extras.impl.CellEditorImpl <em>Cell Editor</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.extras.impl.CellEditorImpl
		 * @see org.eclipse.wazaabi.mm.core.extras.impl.CoreExtrasPackageImpl#getCellEditor()
		 * @generated
		 */
		EClass CELL_EDITOR = eINSTANCE.getCellEditor();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.extras.impl.TextCellEditorImpl <em>Text Cell Editor</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.extras.impl.TextCellEditorImpl
		 * @see org.eclipse.wazaabi.mm.core.extras.impl.CoreExtrasPackageImpl#getTextCellEditor()
		 * @generated
		 */
		EClass TEXT_CELL_EDITOR = eINSTANCE.getTextCellEditor();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.extras.impl.CheckboxCellEditorImpl <em>Checkbox Cell Editor</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.extras.impl.CheckboxCellEditorImpl
		 * @see org.eclipse.wazaabi.mm.core.extras.impl.CoreExtrasPackageImpl#getCheckboxCellEditor()
		 * @generated
		 */
		EClass CHECKBOX_CELL_EDITOR = eINSTANCE.getCheckboxCellEditor();

	}

} //CoreExtrasPackage
