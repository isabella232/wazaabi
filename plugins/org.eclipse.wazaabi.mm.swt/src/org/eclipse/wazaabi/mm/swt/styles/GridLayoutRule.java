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

import org.eclipse.wazaabi.mm.core.styles.LayoutRule;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Grid Layout Rule</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getHorizontalSpacing <em>Horizontal Spacing</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#isMakeColumnsEqualWidth <em>Make Columns Equal Width</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getMarginBottom <em>Margin Bottom</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getMarginHeight <em>Margin Height</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getMarginLeft <em>Margin Left</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getMarginRight <em>Margin Right</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getMarginTop <em>Margin Top</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getMarginWidth <em>Margin Width</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getNumColumns <em>Num Columns</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getVerticalSpacing <em>Vertical Spacing</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getGridLayoutRule()
 * @model
 * @generated
 */
public interface GridLayoutRule extends LayoutRule
{
  /**
   * Returns the value of the '<em><b>Horizontal Spacing</b></em>' attribute.
   * The default value is <code>"5"</code>.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Horizontal Spacing</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Horizontal Spacing</em>' attribute.
   * @see #setHorizontalSpacing(int)
   * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getGridLayoutRule_HorizontalSpacing()
   * @model default="5"
   * @generated
   */
  int getHorizontalSpacing();

  /**
   * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getHorizontalSpacing <em>Horizontal Spacing</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Horizontal Spacing</em>' attribute.
   * @see #getHorizontalSpacing()
   * @generated
   */
  void setHorizontalSpacing(int value);

  /**
   * Returns the value of the '<em><b>Make Columns Equal Width</b></em>' attribute.
   * The default value is <code>"false"</code>.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Make Columns Equal Width</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Make Columns Equal Width</em>' attribute.
   * @see #setMakeColumnsEqualWidth(boolean)
   * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getGridLayoutRule_MakeColumnsEqualWidth()
   * @model default="false"
   * @generated
   */
  boolean isMakeColumnsEqualWidth();

  /**
   * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#isMakeColumnsEqualWidth <em>Make Columns Equal Width</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Make Columns Equal Width</em>' attribute.
   * @see #isMakeColumnsEqualWidth()
   * @generated
   */
  void setMakeColumnsEqualWidth(boolean value);

  /**
   * Returns the value of the '<em><b>Margin Bottom</b></em>' attribute.
   * The default value is <code>"0"</code>.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Margin Bottom</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Margin Bottom</em>' attribute.
   * @see #setMarginBottom(int)
   * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getGridLayoutRule_MarginBottom()
   * @model default="0"
   * @generated
   */
  int getMarginBottom();

  /**
   * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getMarginBottom <em>Margin Bottom</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Margin Bottom</em>' attribute.
   * @see #getMarginBottom()
   * @generated
   */
  void setMarginBottom(int value);

  /**
   * Returns the value of the '<em><b>Margin Height</b></em>' attribute.
   * The default value is <code>"5"</code>.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Margin Height</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Margin Height</em>' attribute.
   * @see #setMarginHeight(int)
   * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getGridLayoutRule_MarginHeight()
   * @model default="5"
   * @generated
   */
  int getMarginHeight();

  /**
   * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getMarginHeight <em>Margin Height</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Margin Height</em>' attribute.
   * @see #getMarginHeight()
   * @generated
   */
  void setMarginHeight(int value);

  /**
   * Returns the value of the '<em><b>Margin Left</b></em>' attribute.
   * The default value is <code>"0"</code>.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Margin Left</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Margin Left</em>' attribute.
   * @see #setMarginLeft(int)
   * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getGridLayoutRule_MarginLeft()
   * @model default="0"
   * @generated
   */
  int getMarginLeft();

  /**
   * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getMarginLeft <em>Margin Left</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Margin Left</em>' attribute.
   * @see #getMarginLeft()
   * @generated
   */
  void setMarginLeft(int value);

  /**
   * Returns the value of the '<em><b>Margin Right</b></em>' attribute.
   * The default value is <code>"0"</code>.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Margin Right</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Margin Right</em>' attribute.
   * @see #setMarginRight(int)
   * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getGridLayoutRule_MarginRight()
   * @model default="0"
   * @generated
   */
  int getMarginRight();

  /**
   * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getMarginRight <em>Margin Right</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Margin Right</em>' attribute.
   * @see #getMarginRight()
   * @generated
   */
  void setMarginRight(int value);

  /**
   * Returns the value of the '<em><b>Margin Top</b></em>' attribute.
   * The default value is <code>"0"</code>.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Margin Top</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Margin Top</em>' attribute.
   * @see #setMarginTop(int)
   * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getGridLayoutRule_MarginTop()
   * @model default="0"
   * @generated
   */
  int getMarginTop();

  /**
   * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getMarginTop <em>Margin Top</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Margin Top</em>' attribute.
   * @see #getMarginTop()
   * @generated
   */
  void setMarginTop(int value);

  /**
   * Returns the value of the '<em><b>Margin Width</b></em>' attribute.
   * The default value is <code>"5"</code>.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Margin Width</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Margin Width</em>' attribute.
   * @see #setMarginWidth(int)
   * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getGridLayoutRule_MarginWidth()
   * @model default="5"
   * @generated
   */
  int getMarginWidth();

  /**
   * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getMarginWidth <em>Margin Width</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Margin Width</em>' attribute.
   * @see #getMarginWidth()
   * @generated
   */
  void setMarginWidth(int value);

  /**
   * Returns the value of the '<em><b>Num Columns</b></em>' attribute.
   * The default value is <code>"1"</code>.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Num Columns</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Num Columns</em>' attribute.
   * @see #setNumColumns(int)
   * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getGridLayoutRule_NumColumns()
   * @model default="1"
   * @generated
   */
  int getNumColumns();

  /**
   * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getNumColumns <em>Num Columns</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Num Columns</em>' attribute.
   * @see #getNumColumns()
   * @generated
   */
  void setNumColumns(int value);

  /**
   * Returns the value of the '<em><b>Vertical Spacing</b></em>' attribute.
   * The default value is <code>"5"</code>.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Vertical Spacing</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Vertical Spacing</em>' attribute.
   * @see #setVerticalSpacing(int)
   * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getGridLayoutRule_VerticalSpacing()
   * @model default="5"
   * @generated
   */
  int getVerticalSpacing();

  /**
   * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getVerticalSpacing <em>Vertical Spacing</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Vertical Spacing</em>' attribute.
   * @see #getVerticalSpacing()
   * @generated
   */
  void setVerticalSpacing(int value);

} // GridLayoutRule
