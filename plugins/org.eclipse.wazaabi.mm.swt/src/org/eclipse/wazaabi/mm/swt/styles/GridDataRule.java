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

import org.eclipse.wazaabi.mm.core.styles.LayoutDataRule;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Grid Data Rule</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#isExclude <em>Exclude</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#isGrabExcessHorizontalSpace <em>Grab Excess Horizontal Space</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#isGrabExcessVerticalSpace <em>Grab Excess Vertical Space</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getHeightHint <em>Height Hint</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getHorizontalAlignement <em>Horizontal Alignement</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getHorizontalIndent <em>Horizontal Indent</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getHorizontalSpan <em>Horizontal Span</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getMinimumHeight <em>Minimum Height</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getMinimumWidth <em>Minimum Width</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getVerticalAlignement <em>Vertical Alignement</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getVerticalIndent <em>Vertical Indent</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getVerticalSpan <em>Vertical Span</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getWidthHint <em>Width Hint</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getGridDataRule()
 * @model
 * @generated
 */
public interface GridDataRule extends LayoutDataRule
{
  /**
   * Returns the value of the '<em><b>Exclude</b></em>' attribute.
   * The default value is <code>"false"</code>.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Exclude</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Exclude</em>' attribute.
   * @see #setExclude(boolean)
   * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getGridDataRule_Exclude()
   * @model default="false"
   * @generated
   */
  boolean isExclude();

  /**
   * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#isExclude <em>Exclude</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Exclude</em>' attribute.
   * @see #isExclude()
   * @generated
   */
  void setExclude(boolean value);

  /**
   * Returns the value of the '<em><b>Grab Excess Horizontal Space</b></em>' attribute.
   * The default value is <code>"false"</code>.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Grab Excess Horizontal Space</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Grab Excess Horizontal Space</em>' attribute.
   * @see #setGrabExcessHorizontalSpace(boolean)
   * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getGridDataRule_GrabExcessHorizontalSpace()
   * @model default="false"
   * @generated
   */
  boolean isGrabExcessHorizontalSpace();

  /**
   * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#isGrabExcessHorizontalSpace <em>Grab Excess Horizontal Space</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Grab Excess Horizontal Space</em>' attribute.
   * @see #isGrabExcessHorizontalSpace()
   * @generated
   */
  void setGrabExcessHorizontalSpace(boolean value);

  /**
   * Returns the value of the '<em><b>Grab Excess Vertical Space</b></em>' attribute.
   * The default value is <code>"false"</code>.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Grab Excess Vertical Space</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Grab Excess Vertical Space</em>' attribute.
   * @see #setGrabExcessVerticalSpace(boolean)
   * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getGridDataRule_GrabExcessVerticalSpace()
   * @model default="false"
   * @generated
   */
  boolean isGrabExcessVerticalSpace();

  /**
   * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#isGrabExcessVerticalSpace <em>Grab Excess Vertical Space</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Grab Excess Vertical Space</em>' attribute.
   * @see #isGrabExcessVerticalSpace()
   * @generated
   */
  void setGrabExcessVerticalSpace(boolean value);

  /**
   * Returns the value of the '<em><b>Height Hint</b></em>' attribute.
   * The default value is <code>"-1"</code>.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Height Hint</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Height Hint</em>' attribute.
   * @see #setHeightHint(int)
   * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getGridDataRule_HeightHint()
   * @model default="-1"
   * @generated
   */
  int getHeightHint();

  /**
   * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getHeightHint <em>Height Hint</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Height Hint</em>' attribute.
   * @see #getHeightHint()
   * @generated
   */
  void setHeightHint(int value);

  /**
   * Returns the value of the '<em><b>Horizontal Alignement</b></em>' attribute.
   * The literals are from the enumeration {@link org.eclipse.wazaabi.mm.swt.styles.GridDataAlignment}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Horizontal Alignement</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Horizontal Alignement</em>' attribute.
   * @see org.eclipse.wazaabi.mm.swt.styles.GridDataAlignment
   * @see #setHorizontalAlignement(GridDataAlignment)
   * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getGridDataRule_HorizontalAlignement()
   * @model
   * @generated
   */
  GridDataAlignment getHorizontalAlignement();

  /**
   * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getHorizontalAlignement <em>Horizontal Alignement</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Horizontal Alignement</em>' attribute.
   * @see org.eclipse.wazaabi.mm.swt.styles.GridDataAlignment
   * @see #getHorizontalAlignement()
   * @generated
   */
  void setHorizontalAlignement(GridDataAlignment value);

  /**
   * Returns the value of the '<em><b>Horizontal Indent</b></em>' attribute.
   * The default value is <code>"0"</code>.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Horizontal Indent</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Horizontal Indent</em>' attribute.
   * @see #setHorizontalIndent(int)
   * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getGridDataRule_HorizontalIndent()
   * @model default="0"
   * @generated
   */
  int getHorizontalIndent();

  /**
   * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getHorizontalIndent <em>Horizontal Indent</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Horizontal Indent</em>' attribute.
   * @see #getHorizontalIndent()
   * @generated
   */
  void setHorizontalIndent(int value);

  /**
   * Returns the value of the '<em><b>Horizontal Span</b></em>' attribute.
   * The default value is <code>"1"</code>.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Horizontal Span</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Horizontal Span</em>' attribute.
   * @see #setHorizontalSpan(int)
   * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getGridDataRule_HorizontalSpan()
   * @model default="1"
   * @generated
   */
  int getHorizontalSpan();

  /**
   * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getHorizontalSpan <em>Horizontal Span</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Horizontal Span</em>' attribute.
   * @see #getHorizontalSpan()
   * @generated
   */
  void setHorizontalSpan(int value);

  /**
   * Returns the value of the '<em><b>Minimum Height</b></em>' attribute.
   * The default value is <code>"-1"</code>.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Minimum Height</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Minimum Height</em>' attribute.
   * @see #setMinimumHeight(int)
   * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getGridDataRule_MinimumHeight()
   * @model default="-1"
   * @generated
   */
  int getMinimumHeight();

  /**
   * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getMinimumHeight <em>Minimum Height</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Minimum Height</em>' attribute.
   * @see #getMinimumHeight()
   * @generated
   */
  void setMinimumHeight(int value);

  /**
   * Returns the value of the '<em><b>Minimum Width</b></em>' attribute.
   * The default value is <code>"-1"</code>.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Minimum Width</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Minimum Width</em>' attribute.
   * @see #setMinimumWidth(int)
   * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getGridDataRule_MinimumWidth()
   * @model default="-1"
   * @generated
   */
  int getMinimumWidth();

  /**
   * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getMinimumWidth <em>Minimum Width</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Minimum Width</em>' attribute.
   * @see #getMinimumWidth()
   * @generated
   */
  void setMinimumWidth(int value);

  /**
   * Returns the value of the '<em><b>Vertical Alignement</b></em>' attribute.
   * The literals are from the enumeration {@link org.eclipse.wazaabi.mm.swt.styles.GridDataAlignment}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Vertical Alignement</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Vertical Alignement</em>' attribute.
   * @see org.eclipse.wazaabi.mm.swt.styles.GridDataAlignment
   * @see #setVerticalAlignement(GridDataAlignment)
   * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getGridDataRule_VerticalAlignement()
   * @model
   * @generated
   */
  GridDataAlignment getVerticalAlignement();

  /**
   * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getVerticalAlignement <em>Vertical Alignement</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Vertical Alignement</em>' attribute.
   * @see org.eclipse.wazaabi.mm.swt.styles.GridDataAlignment
   * @see #getVerticalAlignement()
   * @generated
   */
  void setVerticalAlignement(GridDataAlignment value);

  /**
   * Returns the value of the '<em><b>Vertical Indent</b></em>' attribute.
   * The default value is <code>"0"</code>.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Vertical Indent</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Vertical Indent</em>' attribute.
   * @see #setVerticalIndent(int)
   * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getGridDataRule_VerticalIndent()
   * @model default="0"
   * @generated
   */
  int getVerticalIndent();

  /**
   * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getVerticalIndent <em>Vertical Indent</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Vertical Indent</em>' attribute.
   * @see #getVerticalIndent()
   * @generated
   */
  void setVerticalIndent(int value);

  /**
   * Returns the value of the '<em><b>Vertical Span</b></em>' attribute.
   * The default value is <code>"1"</code>.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Vertical Span</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Vertical Span</em>' attribute.
   * @see #setVerticalSpan(int)
   * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getGridDataRule_VerticalSpan()
   * @model default="1"
   * @generated
   */
  int getVerticalSpan();

  /**
   * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getVerticalSpan <em>Vertical Span</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Vertical Span</em>' attribute.
   * @see #getVerticalSpan()
   * @generated
   */
  void setVerticalSpan(int value);

  /**
   * Returns the value of the '<em><b>Width Hint</b></em>' attribute.
   * The default value is <code>"-1"</code>.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Width Hint</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Width Hint</em>' attribute.
   * @see #setWidthHint(int)
   * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getGridDataRule_WidthHint()
   * @model default="-1"
   * @generated
   */
  int getWidthHint();

  /**
   * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getWidthHint <em>Width Hint</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Width Hint</em>' attribute.
   * @see #getWidthHint()
   * @generated
   */
  void setWidthHint(int value);

} // GridDataRule
