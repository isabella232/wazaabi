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
package org.eclipse.wazaabi.mm.core.styles.collections;

import org.eclipse.wazaabi.mm.core.extras.CellEditor;

import org.eclipse.wazaabi.mm.core.styles.StyleRule;

import org.eclipse.wazaabi.mm.edp.handlers.Parameterized;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Column Descriptor</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.collections.ColumnDescriptor#getLabel <em>Label</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.collections.ColumnDescriptor#getMinimumWidth <em>Minimum Width</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.collections.ColumnDescriptor#getEditingSupport <em>Editing Support</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.collections.ColumnDescriptor#getCellEditor <em>Cell Editor</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage#getColumnDescriptor()
 * @model
 * @generated
 */
public interface ColumnDescriptor extends StyleRule, Parameterized {
	/**
	 * Returns the value of the '<em><b>Label</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Label</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Label</em>' attribute.
	 * @see #setLabel(String)
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage#getColumnDescriptor_Label()
	 * @model
	 * @generated
	 */
	String getLabel();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.collections.ColumnDescriptor#getLabel <em>Label</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Label</em>' attribute.
	 * @see #getLabel()
	 * @generated
	 */
	void setLabel(String value);

	/**
	 * Returns the value of the '<em><b>Minimum Width</b></em>' attribute.
	 * The default value is <code>"20"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Minimum Width</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Minimum Width</em>' attribute.
	 * @see #setMinimumWidth(int)
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage#getColumnDescriptor_MinimumWidth()
	 * @model default="20"
	 * @generated
	 */
	int getMinimumWidth();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.collections.ColumnDescriptor#getMinimumWidth <em>Minimum Width</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Minimum Width</em>' attribute.
	 * @see #getMinimumWidth()
	 * @generated
	 */
	void setMinimumWidth(int value);

	/**
	 * Returns the value of the '<em><b>Editing Support</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Editing Support</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Editing Support</em>' attribute.
	 * @see #setEditingSupport(String)
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage#getColumnDescriptor_EditingSupport()
	 * @model
	 * @generated
	 */
	String getEditingSupport();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.collections.ColumnDescriptor#getEditingSupport <em>Editing Support</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Editing Support</em>' attribute.
	 * @see #getEditingSupport()
	 * @generated
	 */
	void setEditingSupport(String value);

	/**
	 * Returns the value of the '<em><b>Cell Editor</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Cell Editor</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Cell Editor</em>' reference.
	 * @see #setCellEditor(CellEditor)
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage#getColumnDescriptor_CellEditor()
	 * @model
	 * @generated
	 */
	CellEditor getCellEditor();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.collections.ColumnDescriptor#getCellEditor <em>Cell Editor</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Cell Editor</em>' reference.
	 * @see #getCellEditor()
	 * @generated
	 */
	void setCellEditor(CellEditor value);

} // ColumnDescriptor
