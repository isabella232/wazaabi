/**
 */
package org.eclipse.wazaabi.mm.fx.styles;

import org.eclipse.wazaabi.mm.core.styles.LayoutDataRule;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Border Layout Data</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.fx.styles.BorderLayoutData#getPosition <em>Position</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.fx.styles.FXStylesPackage#getBorderLayoutData()
 * @model
 * @generated
 */
public interface BorderLayoutData extends LayoutDataRule {
    /**
     * Returns the value of the '<em><b>Position</b></em>' attribute.
     * The default value is <code>"CENTER"</code>.
     * The literals are from the enumeration {@link org.eclipse.wazaabi.mm.fx.styles.BorderLayoutPosition}.
     * <!-- begin-user-doc -->
     * <p>
     * If the meaning of the '<em>Position</em>' attribute isn't clear,
     * there really should be more of a description here...
     * </p>
     * <!-- end-user-doc -->
     * @return the value of the '<em>Position</em>' attribute.
     * @see org.eclipse.wazaabi.mm.fx.styles.BorderLayoutPosition
     * @see #setPosition(BorderLayoutPosition)
     * @see org.eclipse.wazaabi.mm.fx.styles.FXStylesPackage#getBorderLayoutData_Position()
     * @model default="CENTER"
     * @generated
     */
    BorderLayoutPosition getPosition();

    /**
     * Sets the value of the '{@link org.eclipse.wazaabi.mm.fx.styles.BorderLayoutData#getPosition <em>Position</em>}' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @param value the new value of the '<em>Position</em>' attribute.
     * @see org.eclipse.wazaabi.mm.fx.styles.BorderLayoutPosition
     * @see #getPosition()
     * @generated
     */
    void setPosition(BorderLayoutPosition value);

} // BorderLayoutData
