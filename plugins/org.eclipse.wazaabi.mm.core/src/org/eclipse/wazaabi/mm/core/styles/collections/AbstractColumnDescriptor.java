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
 * A representation of the model object '<em><b>Abstract Column Descriptor</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.collections.AbstractColumnDescriptor#getLabel <em>Label</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.collections.AbstractColumnDescriptor#getEditingSupport <em>Editing Support</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.collections.AbstractColumnDescriptor#getCellEditor <em>Cell Editor</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.collections.AbstractColumnDescriptor#isResizable <em>Resizable</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage#getAbstractColumnDescriptor()
 * @model abstract="true"
 * @generated
 */
public interface AbstractColumnDescriptor extends StyleRule, Parameterized {
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
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage#getAbstractColumnDescriptor_Label()
	 * @model
	 * @generated
	 */
	String getLabel();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.collections.AbstractColumnDescriptor#getLabel <em>Label</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Label</em>' attribute.
	 * @see #getLabel()
	 * @generated
	 */
	void setLabel(String value);

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
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage#getAbstractColumnDescriptor_EditingSupport()
	 * @model
	 * @generated
	 */
	String getEditingSupport();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.collections.AbstractColumnDescriptor#getEditingSupport <em>Editing Support</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Editing Support</em>' attribute.
	 * @see #getEditingSupport()
	 * @generated
	 */
	void setEditingSupport(String value);

	/**
	 * Returns the value of the '<em><b>Cell Editor</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Cell Editor</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Cell Editor</em>' containment reference.
	 * @see #setCellEditor(CellEditor)
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage#getAbstractColumnDescriptor_CellEditor()
	 * @model containment="true"
	 * @generated
	 */
	CellEditor getCellEditor();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.collections.AbstractColumnDescriptor#getCellEditor <em>Cell Editor</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Cell Editor</em>' containment reference.
	 * @see #getCellEditor()
	 * @generated
	 */
	void setCellEditor(CellEditor value);

	/**
	 * Returns the value of the '<em><b>Resizable</b></em>' attribute.
	 * The default value is <code>"false"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Resizable</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Resizable</em>' attribute.
	 * @see #setResizable(boolean)
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage#getAbstractColumnDescriptor_Resizable()
	 * @model default="false"
	 * @generated
	 */
	boolean isResizable();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.collections.AbstractColumnDescriptor#isResizable <em>Resizable</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Resizable</em>' attribute.
	 * @see #isResizable()
	 * @generated
	 */
	void setResizable(boolean value);

} // AbstractColumnDescriptor
