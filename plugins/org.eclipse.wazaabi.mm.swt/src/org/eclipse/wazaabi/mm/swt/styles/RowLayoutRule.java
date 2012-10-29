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

import org.eclipse.wazaabi.mm.core.Orientation;

import org.eclipse.wazaabi.mm.core.styles.LayoutRule;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Row Layout Rule</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#isCenter <em>Center</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#isFill <em>Fill</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#isJustify <em>Justify</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#getMarginBottom <em>Margin Bottom</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#getMarginHeight <em>Margin Height</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#getMarginLeft <em>Margin Left</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#getMarginRight <em>Margin Right</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#getMarginTop <em>Margin Top</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#getMarginWidth <em>Margin Width</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#isPack <em>Pack</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#getSpacing <em>Spacing</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#getType <em>Type</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#isWrap <em>Wrap</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getRowLayoutRule()
 * @model
 * @generated
 */
public interface RowLayoutRule extends LayoutRule {
	/**
	 * Returns the value of the '<em><b>Center</b></em>' attribute.
	 * The default value is <code>"false"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Center</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Center</em>' attribute.
	 * @see #setCenter(boolean)
	 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getRowLayoutRule_Center()
	 * @model default="false"
	 * @generated
	 */
	boolean isCenter();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#isCenter <em>Center</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Center</em>' attribute.
	 * @see #isCenter()
	 * @generated
	 */
	void setCenter(boolean value);

	/**
	 * Returns the value of the '<em><b>Fill</b></em>' attribute.
	 * The default value is <code>"false"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Fill</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Fill</em>' attribute.
	 * @see #setFill(boolean)
	 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getRowLayoutRule_Fill()
	 * @model default="false"
	 * @generated
	 */
	boolean isFill();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#isFill <em>Fill</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Fill</em>' attribute.
	 * @see #isFill()
	 * @generated
	 */
	void setFill(boolean value);

	/**
	 * Returns the value of the '<em><b>Justify</b></em>' attribute.
	 * The default value is <code>"false"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Justify</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Justify</em>' attribute.
	 * @see #setJustify(boolean)
	 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getRowLayoutRule_Justify()
	 * @model default="false"
	 * @generated
	 */
	boolean isJustify();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#isJustify <em>Justify</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Justify</em>' attribute.
	 * @see #isJustify()
	 * @generated
	 */
	void setJustify(boolean value);

	/**
	 * Returns the value of the '<em><b>Margin Bottom</b></em>' attribute.
	 * The default value is <code>"3"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Margin Bottom</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Margin Bottom</em>' attribute.
	 * @see #setMarginBottom(int)
	 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getRowLayoutRule_MarginBottom()
	 * @model default="3"
	 * @generated
	 */
	int getMarginBottom();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#getMarginBottom <em>Margin Bottom</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Margin Bottom</em>' attribute.
	 * @see #getMarginBottom()
	 * @generated
	 */
	void setMarginBottom(int value);

	/**
	 * Returns the value of the '<em><b>Margin Height</b></em>' attribute.
	 * The default value is <code>"0"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Margin Height</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Margin Height</em>' attribute.
	 * @see #setMarginHeight(int)
	 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getRowLayoutRule_MarginHeight()
	 * @model default="0"
	 * @generated
	 */
	int getMarginHeight();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#getMarginHeight <em>Margin Height</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Margin Height</em>' attribute.
	 * @see #getMarginHeight()
	 * @generated
	 */
	void setMarginHeight(int value);

	/**
	 * Returns the value of the '<em><b>Margin Left</b></em>' attribute.
	 * The default value is <code>"3"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Margin Left</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Margin Left</em>' attribute.
	 * @see #setMarginLeft(int)
	 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getRowLayoutRule_MarginLeft()
	 * @model default="3"
	 * @generated
	 */
	int getMarginLeft();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#getMarginLeft <em>Margin Left</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Margin Left</em>' attribute.
	 * @see #getMarginLeft()
	 * @generated
	 */
	void setMarginLeft(int value);

	/**
	 * Returns the value of the '<em><b>Margin Right</b></em>' attribute.
	 * The default value is <code>"3"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Margin Right</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Margin Right</em>' attribute.
	 * @see #setMarginRight(int)
	 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getRowLayoutRule_MarginRight()
	 * @model default="3"
	 * @generated
	 */
	int getMarginRight();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#getMarginRight <em>Margin Right</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Margin Right</em>' attribute.
	 * @see #getMarginRight()
	 * @generated
	 */
	void setMarginRight(int value);

	/**
	 * Returns the value of the '<em><b>Margin Top</b></em>' attribute.
	 * The default value is <code>"3"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Margin Top</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Margin Top</em>' attribute.
	 * @see #setMarginTop(int)
	 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getRowLayoutRule_MarginTop()
	 * @model default="3"
	 * @generated
	 */
	int getMarginTop();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#getMarginTop <em>Margin Top</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Margin Top</em>' attribute.
	 * @see #getMarginTop()
	 * @generated
	 */
	void setMarginTop(int value);

	/**
	 * Returns the value of the '<em><b>Margin Width</b></em>' attribute.
	 * The default value is <code>"0"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Margin Width</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Margin Width</em>' attribute.
	 * @see #setMarginWidth(int)
	 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getRowLayoutRule_MarginWidth()
	 * @model default="0"
	 * @generated
	 */
	int getMarginWidth();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#getMarginWidth <em>Margin Width</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Margin Width</em>' attribute.
	 * @see #getMarginWidth()
	 * @generated
	 */
	void setMarginWidth(int value);

	/**
	 * Returns the value of the '<em><b>Pack</b></em>' attribute.
	 * The default value is <code>"true"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Pack</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Pack</em>' attribute.
	 * @see #setPack(boolean)
	 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getRowLayoutRule_Pack()
	 * @model default="true"
	 * @generated
	 */
	boolean isPack();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#isPack <em>Pack</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Pack</em>' attribute.
	 * @see #isPack()
	 * @generated
	 */
	void setPack(boolean value);

	/**
	 * Returns the value of the '<em><b>Spacing</b></em>' attribute.
	 * The default value is <code>"3"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Spacing</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Spacing</em>' attribute.
	 * @see #setSpacing(int)
	 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getRowLayoutRule_Spacing()
	 * @model default="3"
	 * @generated
	 */
	int getSpacing();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#getSpacing <em>Spacing</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Spacing</em>' attribute.
	 * @see #getSpacing()
	 * @generated
	 */
	void setSpacing(int value);

	/**
	 * Returns the value of the '<em><b>Type</b></em>' attribute.
	 * The default value is <code>"HORIZONTAL"</code>.
	 * The literals are from the enumeration {@link org.eclipse.wazaabi.mm.core.Orientation}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Type</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Type</em>' attribute.
	 * @see org.eclipse.wazaabi.mm.core.Orientation
	 * @see #setType(Orientation)
	 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getRowLayoutRule_Type()
	 * @model default="HORIZONTAL"
	 * @generated
	 */
	Orientation getType();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#getType <em>Type</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Type</em>' attribute.
	 * @see org.eclipse.wazaabi.mm.core.Orientation
	 * @see #getType()
	 * @generated
	 */
	void setType(Orientation value);

	/**
	 * Returns the value of the '<em><b>Wrap</b></em>' attribute.
	 * The default value is <code>"true"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Wrap</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Wrap</em>' attribute.
	 * @see #setWrap(boolean)
	 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getRowLayoutRule_Wrap()
	 * @model default="true"
	 * @generated
	 */
	boolean isWrap();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#isWrap <em>Wrap</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Wrap</em>' attribute.
	 * @see #isWrap()
	 * @generated
	 */
	void setWrap(boolean value);

} // RowLayoutRule
