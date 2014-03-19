/**
 */
package org.eclipse.wazaabi.demo.ecna2014.core.model.impl;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.wazaabi.demo.ecna2014.core.model.ModelPackage;
import org.eclipse.wazaabi.demo.ecna2014.core.model.Winnie;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Winnie</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.demo.ecna2014.core.model.impl.WinnieImpl#getFname <em>Fname</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.demo.ecna2014.core.model.impl.WinnieImpl#getLname <em>Lname</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class WinnieImpl extends MinimalEObjectImpl.Container implements Winnie
{
  /**
   * The default value of the '{@link #getFname() <em>Fname</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getFname()
   * @generated
   * @ordered
   */
  protected static final String FNAME_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getFname() <em>Fname</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getFname()
   * @generated
   * @ordered
   */
  protected String fname = FNAME_EDEFAULT;

  /**
   * The default value of the '{@link #getLname() <em>Lname</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getLname()
   * @generated
   * @ordered
   */
  protected static final String LNAME_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getLname() <em>Lname</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getLname()
   * @generated
   * @ordered
   */
  protected String lname = LNAME_EDEFAULT;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected WinnieImpl()
  {
    super();
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  protected EClass eStaticClass()
  {
    return ModelPackage.Literals.WINNIE;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getFname()
  {
    return fname;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setFname(String newFname)
  {
    String oldFname = fname;
    fname = newFname;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, ModelPackage.WINNIE__FNAME, oldFname, fname));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getLname()
  {
    return lname;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setLname(String newLname)
  {
    String oldLname = lname;
    lname = newLname;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, ModelPackage.WINNIE__LNAME, oldLname, lname));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public Object eGet(int featureID, boolean resolve, boolean coreType)
  {
    switch (featureID)
    {
      case ModelPackage.WINNIE__FNAME:
        return getFname();
      case ModelPackage.WINNIE__LNAME:
        return getLname();
    }
    return super.eGet(featureID, resolve, coreType);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public void eSet(int featureID, Object newValue)
  {
    switch (featureID)
    {
      case ModelPackage.WINNIE__FNAME:
        setFname((String)newValue);
        return;
      case ModelPackage.WINNIE__LNAME:
        setLname((String)newValue);
        return;
    }
    super.eSet(featureID, newValue);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public void eUnset(int featureID)
  {
    switch (featureID)
    {
      case ModelPackage.WINNIE__FNAME:
        setFname(FNAME_EDEFAULT);
        return;
      case ModelPackage.WINNIE__LNAME:
        setLname(LNAME_EDEFAULT);
        return;
    }
    super.eUnset(featureID);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public boolean eIsSet(int featureID)
  {
    switch (featureID)
    {
      case ModelPackage.WINNIE__FNAME:
        return FNAME_EDEFAULT == null ? fname != null : !FNAME_EDEFAULT.equals(fname);
      case ModelPackage.WINNIE__LNAME:
        return LNAME_EDEFAULT == null ? lname != null : !LNAME_EDEFAULT.equals(lname);
    }
    return super.eIsSet(featureID);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public String toString()
  {
    if (eIsProxy()) return super.toString();

    StringBuffer result = new StringBuffer(super.toString());
    result.append(" (fname: ");
    result.append(fname);
    result.append(", lname: ");
    result.append(lname);
    result.append(')');
    return result.toString();
  }

} //WinnieImpl
