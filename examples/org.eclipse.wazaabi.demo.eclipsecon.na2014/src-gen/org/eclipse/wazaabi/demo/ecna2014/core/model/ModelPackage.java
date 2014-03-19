/**
 */
package org.eclipse.wazaabi.demo.ecna2014.core.model;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;

/**
 * <!-- begin-user-doc -->
 * The <b>Package</b> for the model.
 * It contains accessors for the meta objects to represent
 * <ul>
 *   <li>each class,</li>
 *   <li>each feature of each class,</li>
 *   <li>each operation of each class,</li>
 *   <li>each enum,</li>
 *   <li>and each data type</li>
 * </ul>
 * <!-- end-user-doc -->
 * @see org.eclipse.wazaabi.demo.ecna2014.core.model.ModelFactory
 * @model kind="package"
 *        annotation="http://www.eclipse.org/emf/2002/GenModel basePackage='org.eclipse.wazaabi.demo.ecna2014.core'"
 * @generated
 */
public interface ModelPackage extends EPackage
{
  /**
   * The package name.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  String eNAME = "model";

  /**
   * The package namespace URI.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  String eNS_URI = "org.eclipse.wazaabi.demo.ecna2014.core.model";

  /**
   * The package namespace name.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  String eNS_PREFIX = "model";

  /**
   * The singleton instance of the package.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  ModelPackage eINSTANCE = org.eclipse.wazaabi.demo.ecna2014.core.model.impl.ModelPackageImpl.init();

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.demo.ecna2014.core.model.impl.WinnieImpl <em>Winnie</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.demo.ecna2014.core.model.impl.WinnieImpl
   * @see org.eclipse.wazaabi.demo.ecna2014.core.model.impl.ModelPackageImpl#getWinnie()
   * @generated
   */
  int WINNIE = 0;

  /**
   * The feature id for the '<em><b>Fname</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int WINNIE__FNAME = 0;

  /**
   * The feature id for the '<em><b>Lname</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int WINNIE__LNAME = 1;

  /**
   * The number of structural features of the '<em>Winnie</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int WINNIE_FEATURE_COUNT = 2;

  /**
   * The number of operations of the '<em>Winnie</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int WINNIE_OPERATION_COUNT = 0;


  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.demo.ecna2014.core.model.Winnie <em>Winnie</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Winnie</em>'.
   * @see org.eclipse.wazaabi.demo.ecna2014.core.model.Winnie
   * @generated
   */
  EClass getWinnie();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.demo.ecna2014.core.model.Winnie#getFname <em>Fname</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Fname</em>'.
   * @see org.eclipse.wazaabi.demo.ecna2014.core.model.Winnie#getFname()
   * @see #getWinnie()
   * @generated
   */
  EAttribute getWinnie_Fname();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.demo.ecna2014.core.model.Winnie#getLname <em>Lname</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Lname</em>'.
   * @see org.eclipse.wazaabi.demo.ecna2014.core.model.Winnie#getLname()
   * @see #getWinnie()
   * @generated
   */
  EAttribute getWinnie_Lname();

  /**
   * Returns the factory that creates the instances of the model.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the factory that creates the instances of the model.
   * @generated
   */
  ModelFactory getModelFactory();

  /**
   * <!-- begin-user-doc -->
   * Defines literals for the meta objects that represent
   * <ul>
   *   <li>each class,</li>
   *   <li>each feature of each class,</li>
   *   <li>each operation of each class,</li>
   *   <li>each enum,</li>
   *   <li>and each data type</li>
   * </ul>
   * <!-- end-user-doc -->
   * @generated
   */
  interface Literals
  {
    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.demo.ecna2014.core.model.impl.WinnieImpl <em>Winnie</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.demo.ecna2014.core.model.impl.WinnieImpl
     * @see org.eclipse.wazaabi.demo.ecna2014.core.model.impl.ModelPackageImpl#getWinnie()
     * @generated
     */
    EClass WINNIE = eINSTANCE.getWinnie();

    /**
     * The meta object literal for the '<em><b>Fname</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute WINNIE__FNAME = eINSTANCE.getWinnie_Fname();

    /**
     * The meta object literal for the '<em><b>Lname</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute WINNIE__LNAME = eINSTANCE.getWinnie_Lname();

  }

} //ModelPackage
