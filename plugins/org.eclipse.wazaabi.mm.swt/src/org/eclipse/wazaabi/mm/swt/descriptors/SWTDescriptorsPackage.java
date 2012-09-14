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
package org.eclipse.wazaabi.mm.swt.descriptors;

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
 * @see org.eclipse.wazaabi.mm.swt.descriptors.SWTDescriptorsFactory
 * @model kind="package"
 * @generated
 */
public interface SWTDescriptorsPackage extends EPackage
{
  /**
   * The package name.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  String eNAME = "descriptors";

  /**
   * The package namespace URI.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  String eNS_URI = "http://www.wazaabi.org/swt/descriptors";

  /**
   * The package namespace name.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  String eNS_PREFIX = "swtdesc";

  /**
   * The singleton instance of the package.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  SWTDescriptorsPackage eINSTANCE = org.eclipse.wazaabi.mm.swt.descriptors.impl.SWTDescriptorsPackageImpl.init();

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.swt.descriptors.Control <em>Control</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.swt.descriptors.Control
   * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.SWTDescriptorsPackageImpl#getControl()
   * @generated
   */
  int CONTROL = 0;

  /**
   * The number of structural features of the '<em>Control</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONTROL_FEATURE_COUNT = 0;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.swt.descriptors.impl.ProgressBarImpl <em>Progress Bar</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.ProgressBarImpl
   * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.SWTDescriptorsPackageImpl#getProgressBar()
   * @generated
   */
  int PROGRESS_BAR = 1;

  /**
   * The number of structural features of the '<em>Progress Bar</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PROGRESS_BAR_FEATURE_COUNT = CONTROL_FEATURE_COUNT + 0;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.swt.descriptors.Composite <em>Composite</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.swt.descriptors.Composite
   * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.SWTDescriptorsPackageImpl#getComposite()
   * @generated
   */
  int COMPOSITE = 2;

  /**
   * The number of structural features of the '<em>Composite</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int COMPOSITE_FEATURE_COUNT = CONTROL_FEATURE_COUNT + 0;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.swt.descriptors.impl.TextImpl <em>Text</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.TextImpl
   * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.SWTDescriptorsPackageImpl#getText()
   * @generated
   */
  int TEXT = 3;

  /**
   * The number of structural features of the '<em>Text</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int TEXT_FEATURE_COUNT = CONTROL_FEATURE_COUNT + 0;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.swt.descriptors.impl.PushButtonImpl <em>Push Button</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.PushButtonImpl
   * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.SWTDescriptorsPackageImpl#getPushButton()
   * @generated
   */
  int PUSH_BUTTON = 4;

  /**
   * The number of structural features of the '<em>Push Button</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PUSH_BUTTON_FEATURE_COUNT = CONTROL_FEATURE_COUNT + 0;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.swt.descriptors.impl.LabelImpl <em>Label</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.LabelImpl
   * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.SWTDescriptorsPackageImpl#getLabel()
   * @generated
   */
  int LABEL = 5;

  /**
   * The number of structural features of the '<em>Label</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int LABEL_FEATURE_COUNT = CONTROL_FEATURE_COUNT + 0;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.swt.descriptors.impl.RadioButtonImpl <em>Radio Button</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.RadioButtonImpl
   * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.SWTDescriptorsPackageImpl#getRadioButton()
   * @generated
   */
  int RADIO_BUTTON = 6;

  /**
   * The number of structural features of the '<em>Radio Button</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int RADIO_BUTTON_FEATURE_COUNT = CONTROL_FEATURE_COUNT + 0;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.swt.descriptors.impl.CheckBoxImpl <em>Check Box</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.CheckBoxImpl
   * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.SWTDescriptorsPackageImpl#getCheckBox()
   * @generated
   */
  int CHECK_BOX = 7;

  /**
   * The number of structural features of the '<em>Check Box</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CHECK_BOX_FEATURE_COUNT = CONTROL_FEATURE_COUNT + 0;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.swt.descriptors.impl.SliderImpl <em>Slider</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.SliderImpl
   * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.SWTDescriptorsPackageImpl#getSlider()
   * @generated
   */
  int SLIDER = 8;

  /**
   * The number of structural features of the '<em>Slider</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SLIDER_FEATURE_COUNT = CONTROL_FEATURE_COUNT + 0;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.swt.descriptors.impl.ScaleImpl <em>Scale</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.ScaleImpl
   * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.SWTDescriptorsPackageImpl#getScale()
   * @generated
   */
  int SCALE = 9;

  /**
   * The number of structural features of the '<em>Scale</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SCALE_FEATURE_COUNT = CONTROL_FEATURE_COUNT + 0;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.swt.descriptors.impl.SpinnerImpl <em>Spinner</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.SpinnerImpl
   * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.SWTDescriptorsPackageImpl#getSpinner()
   * @generated
   */
  int SPINNER = 10;

  /**
   * The number of structural features of the '<em>Spinner</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SPINNER_FEATURE_COUNT = CONTROL_FEATURE_COUNT + 0;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.swt.descriptors.impl.CollectionImpl <em>Collection</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.CollectionImpl
   * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.SWTDescriptorsPackageImpl#getCollection()
   * @generated
   */
  int COLLECTION = 11;

  /**
   * The number of structural features of the '<em>Collection</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int COLLECTION_FEATURE_COUNT = CONTROL_FEATURE_COUNT + 0;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.swt.descriptors.impl.MenuComponentImpl <em>Menu Component</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.MenuComponentImpl
   * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.SWTDescriptorsPackageImpl#getMenuComponent()
   * @generated
   */
  int MENU_COMPONENT = 12;

  /**
   * The number of structural features of the '<em>Menu Component</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MENU_COMPONENT_FEATURE_COUNT = 0;


  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.swt.descriptors.Control <em>Control</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Control</em>'.
   * @see org.eclipse.wazaabi.mm.swt.descriptors.Control
   * @generated
   */
  EClass getControl();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.swt.descriptors.ProgressBar <em>Progress Bar</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Progress Bar</em>'.
   * @see org.eclipse.wazaabi.mm.swt.descriptors.ProgressBar
   * @generated
   */
  EClass getProgressBar();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.swt.descriptors.Composite <em>Composite</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Composite</em>'.
   * @see org.eclipse.wazaabi.mm.swt.descriptors.Composite
   * @generated
   */
  EClass getComposite();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.swt.descriptors.Text <em>Text</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Text</em>'.
   * @see org.eclipse.wazaabi.mm.swt.descriptors.Text
   * @generated
   */
  EClass getText();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.swt.descriptors.PushButton <em>Push Button</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Push Button</em>'.
   * @see org.eclipse.wazaabi.mm.swt.descriptors.PushButton
   * @generated
   */
  EClass getPushButton();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.swt.descriptors.Label <em>Label</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Label</em>'.
   * @see org.eclipse.wazaabi.mm.swt.descriptors.Label
   * @generated
   */
  EClass getLabel();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.swt.descriptors.RadioButton <em>Radio Button</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Radio Button</em>'.
   * @see org.eclipse.wazaabi.mm.swt.descriptors.RadioButton
   * @generated
   */
  EClass getRadioButton();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.swt.descriptors.CheckBox <em>Check Box</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Check Box</em>'.
   * @see org.eclipse.wazaabi.mm.swt.descriptors.CheckBox
   * @generated
   */
  EClass getCheckBox();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.swt.descriptors.Slider <em>Slider</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Slider</em>'.
   * @see org.eclipse.wazaabi.mm.swt.descriptors.Slider
   * @generated
   */
  EClass getSlider();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.swt.descriptors.Scale <em>Scale</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Scale</em>'.
   * @see org.eclipse.wazaabi.mm.swt.descriptors.Scale
   * @generated
   */
  EClass getScale();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.swt.descriptors.Spinner <em>Spinner</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Spinner</em>'.
   * @see org.eclipse.wazaabi.mm.swt.descriptors.Spinner
   * @generated
   */
  EClass getSpinner();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.swt.descriptors.Collection <em>Collection</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Collection</em>'.
   * @see org.eclipse.wazaabi.mm.swt.descriptors.Collection
   * @generated
   */
  EClass getCollection();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.swt.descriptors.MenuComponent <em>Menu Component</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Menu Component</em>'.
   * @see org.eclipse.wazaabi.mm.swt.descriptors.MenuComponent
   * @generated
   */
  EClass getMenuComponent();

  /**
   * Returns the factory that creates the instances of the model.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the factory that creates the instances of the model.
   * @generated
   */
  SWTDescriptorsFactory getSWTDescriptorsFactory();

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
  interface Literals
  {
    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.swt.descriptors.Control <em>Control</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.swt.descriptors.Control
     * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.SWTDescriptorsPackageImpl#getControl()
     * @generated
     */
    EClass CONTROL = eINSTANCE.getControl();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.swt.descriptors.impl.ProgressBarImpl <em>Progress Bar</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.ProgressBarImpl
     * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.SWTDescriptorsPackageImpl#getProgressBar()
     * @generated
     */
    EClass PROGRESS_BAR = eINSTANCE.getProgressBar();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.swt.descriptors.Composite <em>Composite</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.swt.descriptors.Composite
     * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.SWTDescriptorsPackageImpl#getComposite()
     * @generated
     */
    EClass COMPOSITE = eINSTANCE.getComposite();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.swt.descriptors.impl.TextImpl <em>Text</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.TextImpl
     * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.SWTDescriptorsPackageImpl#getText()
     * @generated
     */
    EClass TEXT = eINSTANCE.getText();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.swt.descriptors.impl.PushButtonImpl <em>Push Button</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.PushButtonImpl
     * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.SWTDescriptorsPackageImpl#getPushButton()
     * @generated
     */
    EClass PUSH_BUTTON = eINSTANCE.getPushButton();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.swt.descriptors.impl.LabelImpl <em>Label</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.LabelImpl
     * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.SWTDescriptorsPackageImpl#getLabel()
     * @generated
     */
    EClass LABEL = eINSTANCE.getLabel();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.swt.descriptors.impl.RadioButtonImpl <em>Radio Button</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.RadioButtonImpl
     * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.SWTDescriptorsPackageImpl#getRadioButton()
     * @generated
     */
    EClass RADIO_BUTTON = eINSTANCE.getRadioButton();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.swt.descriptors.impl.CheckBoxImpl <em>Check Box</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.CheckBoxImpl
     * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.SWTDescriptorsPackageImpl#getCheckBox()
     * @generated
     */
    EClass CHECK_BOX = eINSTANCE.getCheckBox();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.swt.descriptors.impl.SliderImpl <em>Slider</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.SliderImpl
     * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.SWTDescriptorsPackageImpl#getSlider()
     * @generated
     */
    EClass SLIDER = eINSTANCE.getSlider();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.swt.descriptors.impl.ScaleImpl <em>Scale</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.ScaleImpl
     * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.SWTDescriptorsPackageImpl#getScale()
     * @generated
     */
    EClass SCALE = eINSTANCE.getScale();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.swt.descriptors.impl.SpinnerImpl <em>Spinner</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.SpinnerImpl
     * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.SWTDescriptorsPackageImpl#getSpinner()
     * @generated
     */
    EClass SPINNER = eINSTANCE.getSpinner();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.swt.descriptors.impl.CollectionImpl <em>Collection</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.CollectionImpl
     * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.SWTDescriptorsPackageImpl#getCollection()
     * @generated
     */
    EClass COLLECTION = eINSTANCE.getCollection();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.swt.descriptors.impl.MenuComponentImpl <em>Menu Component</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.MenuComponentImpl
     * @see org.eclipse.wazaabi.mm.swt.descriptors.impl.SWTDescriptorsPackageImpl#getMenuComponent()
     * @generated
     */
    EClass MENU_COMPONENT = eINSTANCE.getMenuComponent();

  }

} //SWTDescriptorsPackage
