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
package org.eclipse.wazaabi.mm.swt.descriptors.impl;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.impl.EPackageImpl;

import org.eclipse.wazaabi.mm.core.CorePackage;

import org.eclipse.wazaabi.mm.swt.descriptors.CheckBox;
import org.eclipse.wazaabi.mm.swt.descriptors.Collection;
import org.eclipse.wazaabi.mm.swt.descriptors.Composite;
import org.eclipse.wazaabi.mm.swt.descriptors.Control;
import org.eclipse.wazaabi.mm.swt.descriptors.Label;
import org.eclipse.wazaabi.mm.swt.descriptors.MenuComponent;
import org.eclipse.wazaabi.mm.swt.descriptors.ProgressBar;
import org.eclipse.wazaabi.mm.swt.descriptors.PushButton;
import org.eclipse.wazaabi.mm.swt.descriptors.RadioButton;
import org.eclipse.wazaabi.mm.swt.descriptors.SWTDescriptorsFactory;
import org.eclipse.wazaabi.mm.swt.descriptors.SWTDescriptorsPackage;
import org.eclipse.wazaabi.mm.swt.descriptors.Scale;
import org.eclipse.wazaabi.mm.swt.descriptors.Slider;
import org.eclipse.wazaabi.mm.swt.descriptors.Spinner;
import org.eclipse.wazaabi.mm.swt.descriptors.Text;

import org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage;

import org.eclipse.wazaabi.mm.swt.styles.impl.SWTStylesPackageImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Package</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class SWTDescriptorsPackageImpl extends EPackageImpl implements SWTDescriptorsPackage
{
  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass controlEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass progressBarEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass compositeEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass textEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass pushButtonEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass labelEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass radioButtonEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass checkBoxEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass sliderEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass scaleEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass spinnerEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass collectionEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass menuComponentEClass = null;

  /**
   * Creates an instance of the model <b>Package</b>, registered with
   * {@link org.eclipse.emf.ecore.EPackage.Registry EPackage.Registry} by the package
   * package URI value.
   * <p>Note: the correct way to create the package is via the static
   * factory method {@link #init init()}, which also performs
   * initialization of the package, or returns the registered package,
   * if one already exists.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.emf.ecore.EPackage.Registry
   * @see org.eclipse.wazaabi.mm.swt.descriptors.SWTDescriptorsPackage#eNS_URI
   * @see #init()
   * @generated
   */
  private SWTDescriptorsPackageImpl()
  {
    super(eNS_URI, SWTDescriptorsFactory.eINSTANCE);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private static boolean isInited = false;

  /**
   * Creates, registers, and initializes the <b>Package</b> for this model, and for any others upon which it depends.
   * 
   * <p>This method is used to initialize {@link SWTDescriptorsPackage#eINSTANCE} when that field is accessed.
   * Clients should not invoke it directly. Instead, they should simply access that field to obtain the package.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #eNS_URI
   * @see #createPackageContents()
   * @see #initializePackageContents()
   * @generated
   */
  public static SWTDescriptorsPackage init()
  {
    if (isInited) return (SWTDescriptorsPackage)EPackage.Registry.INSTANCE.getEPackage(SWTDescriptorsPackage.eNS_URI);

    // Obtain or create and register package
    SWTDescriptorsPackageImpl theSWTDescriptorsPackage = (SWTDescriptorsPackageImpl)(EPackage.Registry.INSTANCE.get(eNS_URI) instanceof SWTDescriptorsPackageImpl ? EPackage.Registry.INSTANCE.get(eNS_URI) : new SWTDescriptorsPackageImpl());

    isInited = true;

    // Initialize simple dependencies
    CorePackage.eINSTANCE.eClass();

    // Obtain or create and register interdependencies
    SWTStylesPackageImpl theSWTStylesPackage = (SWTStylesPackageImpl)(EPackage.Registry.INSTANCE.getEPackage(SWTStylesPackage.eNS_URI) instanceof SWTStylesPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(SWTStylesPackage.eNS_URI) : SWTStylesPackage.eINSTANCE);

    // Create package meta-data objects
    theSWTDescriptorsPackage.createPackageContents();
    theSWTStylesPackage.createPackageContents();

    // Initialize created meta-data
    theSWTDescriptorsPackage.initializePackageContents();
    theSWTStylesPackage.initializePackageContents();

    // Mark meta-data to indicate it can't be changed
    theSWTDescriptorsPackage.freeze();

  
    // Update the registry and return the package
    EPackage.Registry.INSTANCE.put(SWTDescriptorsPackage.eNS_URI, theSWTDescriptorsPackage);
    return theSWTDescriptorsPackage;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getControl()
  {
    return controlEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getProgressBar()
  {
    return progressBarEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getComposite()
  {
    return compositeEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getText()
  {
    return textEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getPushButton()
  {
    return pushButtonEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getLabel()
  {
    return labelEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getRadioButton()
  {
    return radioButtonEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getCheckBox()
  {
    return checkBoxEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getSlider()
  {
    return sliderEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getScale()
  {
    return scaleEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getSpinner()
  {
    return spinnerEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getCollection()
  {
    return collectionEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getMenuComponent()
  {
    return menuComponentEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SWTDescriptorsFactory getSWTDescriptorsFactory()
  {
    return (SWTDescriptorsFactory)getEFactoryInstance();
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private boolean isCreated = false;

  /**
   * Creates the meta-model objects for the package.  This method is
   * guarded to have no affect on any invocation but its first.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void createPackageContents()
  {
    if (isCreated) return;
    isCreated = true;

    // Create classes and their features
    controlEClass = createEClass(CONTROL);

    progressBarEClass = createEClass(PROGRESS_BAR);

    compositeEClass = createEClass(COMPOSITE);

    textEClass = createEClass(TEXT);

    pushButtonEClass = createEClass(PUSH_BUTTON);

    labelEClass = createEClass(LABEL);

    radioButtonEClass = createEClass(RADIO_BUTTON);

    checkBoxEClass = createEClass(CHECK_BOX);

    sliderEClass = createEClass(SLIDER);

    scaleEClass = createEClass(SCALE);

    spinnerEClass = createEClass(SPINNER);

    collectionEClass = createEClass(COLLECTION);

    menuComponentEClass = createEClass(MENU_COMPONENT);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private boolean isInitialized = false;

  /**
   * Complete the initialization of the package and its meta-model.  This
   * method is guarded to have no affect on any invocation but its first.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void initializePackageContents()
  {
    if (isInitialized) return;
    isInitialized = true;

    // Initialize package
    setName(eNAME);
    setNsPrefix(eNS_PREFIX);
    setNsURI(eNS_URI);

    // Create type parameters

    // Set bounds for type parameters

    // Add supertypes to classes
    progressBarEClass.getESuperTypes().add(this.getControl());
    compositeEClass.getESuperTypes().add(this.getControl());
    textEClass.getESuperTypes().add(this.getControl());
    pushButtonEClass.getESuperTypes().add(this.getControl());
    labelEClass.getESuperTypes().add(this.getControl());
    radioButtonEClass.getESuperTypes().add(this.getControl());
    checkBoxEClass.getESuperTypes().add(this.getControl());
    sliderEClass.getESuperTypes().add(this.getControl());
    scaleEClass.getESuperTypes().add(this.getControl());
    spinnerEClass.getESuperTypes().add(this.getControl());
    collectionEClass.getESuperTypes().add(this.getControl());

    // Initialize classes and features; add operations and parameters
    initEClass(controlEClass, Control.class, "Control", IS_ABSTRACT, IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(progressBarEClass, ProgressBar.class, "ProgressBar", IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(compositeEClass, Composite.class, "Composite", IS_ABSTRACT, IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(textEClass, Text.class, "Text", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(pushButtonEClass, PushButton.class, "PushButton", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(labelEClass, Label.class, "Label", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(radioButtonEClass, RadioButton.class, "RadioButton", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(checkBoxEClass, CheckBox.class, "CheckBox", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(sliderEClass, Slider.class, "Slider", IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(scaleEClass, Scale.class, "Scale", IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(spinnerEClass, Spinner.class, "Spinner", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(collectionEClass, Collection.class, "Collection", IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(menuComponentEClass, MenuComponent.class, "MenuComponent", IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    // Create resource
    createResource(eNS_URI);

    // Create annotations
    // http://www.wazaabi.org/style/property/definition
    createDefinitionAnnotations();
  }

  /**
   * Initializes the annotations for <b>http://www.wazaabi.org/style/property/definition</b>.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected void createDefinitionAnnotations()
  {
    String source = "http://www.wazaabi.org/style/property/definition";		
    addAnnotation
      (controlEClass, 
       source, 
       new String[] 
       {
       "name", "border",
       "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=BooleanRule",
       "default", "value=false"
       });		
    addAnnotation
      (compositeEClass, 
       source, 
       new String[] 
       {
       "name", "row-layout",
       "type", "package=http://www.wazaabi.org/swt/styles\r\nEClass=RowLayoutRule"
       });
  }

} //SWTDescriptorsPackageImpl
