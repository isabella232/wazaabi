/**
 */
package org.eclipse.wazaabi.demo.ecna2014.core.model;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Winnie</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.demo.ecna2014.core.model.Winnie#getFname <em>Fname</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.demo.ecna2014.core.model.Winnie#getLname <em>Lname</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.demo.ecna2014.core.model.ModelPackage#getWinnie()
 * @model
 * @generated
 */
public interface Winnie extends EObject
{
  /**
   * Returns the value of the '<em><b>Fname</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Fname</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Fname</em>' attribute.
   * @see #setFname(String)
   * @see org.eclipse.wazaabi.demo.ecna2014.core.model.ModelPackage#getWinnie_Fname()
   * @model unique="false"
   * @generated
   */
  String getFname();

  /**
   * Sets the value of the '{@link org.eclipse.wazaabi.demo.ecna2014.core.model.Winnie#getFname <em>Fname</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Fname</em>' attribute.
   * @see #getFname()
   * @generated
   */
  void setFname(String value);

  /**
   * Returns the value of the '<em><b>Lname</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Lname</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Lname</em>' attribute.
   * @see #setLname(String)
   * @see org.eclipse.wazaabi.demo.ecna2014.core.model.ModelPackage#getWinnie_Lname()
   * @model unique="false"
   * @generated
   */
  String getLname();

  /**
   * Sets the value of the '{@link org.eclipse.wazaabi.demo.ecna2014.core.model.Winnie#getLname <em>Lname</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Lname</em>' attribute.
   * @see #getLname()
   * @generated
   */
  void setLname(String value);

} // Winnie
