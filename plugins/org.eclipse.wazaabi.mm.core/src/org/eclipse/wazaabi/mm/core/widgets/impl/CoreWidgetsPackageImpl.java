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
package org.eclipse.wazaabi.mm.core.widgets.impl;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EOperation;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

import org.eclipse.emf.ecore.impl.EPackageImpl;

import org.eclipse.wazaabi.mm.core.CorePackage;

import org.eclipse.wazaabi.mm.core.annotations.CoreAnnotationsPackage;

import org.eclipse.wazaabi.mm.core.annotations.impl.CoreAnnotationsPackageImpl;

import org.eclipse.wazaabi.mm.core.extras.CoreExtrasPackage;

import org.eclipse.wazaabi.mm.core.extras.impl.CoreExtrasPackageImpl;

import org.eclipse.wazaabi.mm.core.handlers.CoreHandlersPackage;

import org.eclipse.wazaabi.mm.core.handlers.impl.CoreHandlersPackageImpl;

import org.eclipse.wazaabi.mm.core.impl.CorePackageImpl;

import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;

import org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage;

import org.eclipse.wazaabi.mm.core.styles.collections.impl.CoreCollectionsStylesPackageImpl;

import org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl;

import org.eclipse.wazaabi.mm.core.widgets.AbstractButton;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.core.widgets.CheckBox;
import org.eclipse.wazaabi.mm.core.widgets.Collection;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;
import org.eclipse.wazaabi.mm.core.widgets.Label;
import org.eclipse.wazaabi.mm.core.widgets.MenuComponent;
import org.eclipse.wazaabi.mm.core.widgets.ProgressBar;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;
import org.eclipse.wazaabi.mm.core.widgets.RadioButton;
import org.eclipse.wazaabi.mm.core.widgets.Scale;
import org.eclipse.wazaabi.mm.core.widgets.Slider;
import org.eclipse.wazaabi.mm.core.widgets.Spinner;
import org.eclipse.wazaabi.mm.core.widgets.TextComponent;
import org.eclipse.wazaabi.mm.core.widgets.Widget;

import org.eclipse.wazaabi.mm.edp.EdpPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Package</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class CoreWidgetsPackageImpl extends EPackageImpl implements CoreWidgetsPackage {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass widgetEClass = null;

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
	private EClass containerEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass textComponentEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass abstractComponentEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass abstractButtonEClass = null;

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
	private EClass spinnerEClass = null;

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
	 * @see org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private CoreWidgetsPackageImpl() {
		super(eNS_URI, CoreWidgetsFactory.eINSTANCE);
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
	 * <p>This method is used to initialize {@link CoreWidgetsPackage#eINSTANCE} when that field is accessed.
	 * Clients should not invoke it directly. Instead, they should simply access that field to obtain the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @generated
	 */
	public static CoreWidgetsPackage init() {
		if (isInited) return (CoreWidgetsPackage)EPackage.Registry.INSTANCE.getEPackage(CoreWidgetsPackage.eNS_URI);

		// Obtain or create and register package
		CoreWidgetsPackageImpl theCoreWidgetsPackage = (CoreWidgetsPackageImpl)(EPackage.Registry.INSTANCE.get(eNS_URI) instanceof CoreWidgetsPackageImpl ? EPackage.Registry.INSTANCE.get(eNS_URI) : new CoreWidgetsPackageImpl());

		isInited = true;

		// Initialize simple dependencies
		EdpPackage.eINSTANCE.eClass();

		// Obtain or create and register interdependencies
		CorePackageImpl theCorePackage = (CorePackageImpl)(EPackage.Registry.INSTANCE.getEPackage(CorePackage.eNS_URI) instanceof CorePackageImpl ? EPackage.Registry.INSTANCE.getEPackage(CorePackage.eNS_URI) : CorePackage.eINSTANCE);
		CoreStylesPackageImpl theCoreStylesPackage = (CoreStylesPackageImpl)(EPackage.Registry.INSTANCE.getEPackage(CoreStylesPackage.eNS_URI) instanceof CoreStylesPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(CoreStylesPackage.eNS_URI) : CoreStylesPackage.eINSTANCE);
		CoreCollectionsStylesPackageImpl theCoreCollectionsStylesPackage = (CoreCollectionsStylesPackageImpl)(EPackage.Registry.INSTANCE.getEPackage(CoreCollectionsStylesPackage.eNS_URI) instanceof CoreCollectionsStylesPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(CoreCollectionsStylesPackage.eNS_URI) : CoreCollectionsStylesPackage.eINSTANCE);
		CoreAnnotationsPackageImpl theCoreAnnotationsPackage = (CoreAnnotationsPackageImpl)(EPackage.Registry.INSTANCE.getEPackage(CoreAnnotationsPackage.eNS_URI) instanceof CoreAnnotationsPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(CoreAnnotationsPackage.eNS_URI) : CoreAnnotationsPackage.eINSTANCE);
		CoreHandlersPackageImpl theCoreHandlersPackage = (CoreHandlersPackageImpl)(EPackage.Registry.INSTANCE.getEPackage(CoreHandlersPackage.eNS_URI) instanceof CoreHandlersPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(CoreHandlersPackage.eNS_URI) : CoreHandlersPackage.eINSTANCE);
		CoreExtrasPackageImpl theCoreExtrasPackage = (CoreExtrasPackageImpl)(EPackage.Registry.INSTANCE.getEPackage(CoreExtrasPackage.eNS_URI) instanceof CoreExtrasPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(CoreExtrasPackage.eNS_URI) : CoreExtrasPackage.eINSTANCE);

		// Create package meta-data objects
		theCoreWidgetsPackage.createPackageContents();
		theCorePackage.createPackageContents();
		theCoreStylesPackage.createPackageContents();
		theCoreCollectionsStylesPackage.createPackageContents();
		theCoreAnnotationsPackage.createPackageContents();
		theCoreHandlersPackage.createPackageContents();
		theCoreExtrasPackage.createPackageContents();

		// Initialize created meta-data
		theCoreWidgetsPackage.initializePackageContents();
		theCorePackage.initializePackageContents();
		theCoreStylesPackage.initializePackageContents();
		theCoreCollectionsStylesPackage.initializePackageContents();
		theCoreAnnotationsPackage.initializePackageContents();
		theCoreHandlersPackage.initializePackageContents();
		theCoreExtrasPackage.initializePackageContents();

		// Mark meta-data to indicate it can't be changed
		theCoreWidgetsPackage.freeze();

  
		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(CoreWidgetsPackage.eNS_URI, theCoreWidgetsPackage);
		return theCoreWidgetsPackage;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getWidget() {
		return widgetEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getProgressBar() {
		return progressBarEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getProgressBar_Value() {
		return (EAttribute)progressBarEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getContainer() {
		return containerEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getContainer_Children() {
		return (EReference)containerEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getTextComponent() {
		return textComponentEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getTextComponent_Text() {
		return (EAttribute)textComponentEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getAbstractComponent() {
		return abstractComponentEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getAbstractComponent_Id() {
		return (EAttribute)abstractComponentEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getAbstractComponent_Focus() {
		return (EAttribute)abstractComponentEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getAbstractButton() {
		return abstractButtonEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getPushButton() {
		return pushButtonEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getLabel() {
		return labelEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getRadioButton() {
		return radioButtonEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getRadioButton_Selected() {
		return (EAttribute)radioButtonEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getCheckBox() {
		return checkBoxEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getCheckBox_Selected() {
		return (EAttribute)checkBoxEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getSlider() {
		return sliderEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getSlider_Value() {
		return (EAttribute)sliderEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getSpinner() {
		return spinnerEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getSpinner_Value() {
		return (EAttribute)spinnerEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getScale() {
		return scaleEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getScale_Value() {
		return (EAttribute)scaleEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getCollection() {
		return collectionEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getCollection_Selection() {
		return (EAttribute)collectionEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getCollection_Input() {
		return (EAttribute)collectionEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getMenuComponent() {
		return menuComponentEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getMenuComponent_Children() {
		return (EReference)menuComponentEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getMenuComponent_Text() {
		return (EAttribute)menuComponentEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getMenuComponent_Enabled() {
		return (EAttribute)menuComponentEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CoreWidgetsFactory getCoreWidgetsFactory() {
		return (CoreWidgetsFactory)getEFactoryInstance();
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
	public void createPackageContents() {
		if (isCreated) return;
		isCreated = true;

		// Create classes and their features
		widgetEClass = createEClass(WIDGET);

		progressBarEClass = createEClass(PROGRESS_BAR);
		createEAttribute(progressBarEClass, PROGRESS_BAR__VALUE);

		containerEClass = createEClass(CONTAINER);
		createEReference(containerEClass, CONTAINER__CHILDREN);

		textComponentEClass = createEClass(TEXT_COMPONENT);
		createEAttribute(textComponentEClass, TEXT_COMPONENT__TEXT);

		abstractComponentEClass = createEClass(ABSTRACT_COMPONENT);
		createEAttribute(abstractComponentEClass, ABSTRACT_COMPONENT__ID);
		createEAttribute(abstractComponentEClass, ABSTRACT_COMPONENT__FOCUS);

		abstractButtonEClass = createEClass(ABSTRACT_BUTTON);

		pushButtonEClass = createEClass(PUSH_BUTTON);

		labelEClass = createEClass(LABEL);

		radioButtonEClass = createEClass(RADIO_BUTTON);
		createEAttribute(radioButtonEClass, RADIO_BUTTON__SELECTED);

		checkBoxEClass = createEClass(CHECK_BOX);
		createEAttribute(checkBoxEClass, CHECK_BOX__SELECTED);

		sliderEClass = createEClass(SLIDER);
		createEAttribute(sliderEClass, SLIDER__VALUE);

		spinnerEClass = createEClass(SPINNER);
		createEAttribute(spinnerEClass, SPINNER__VALUE);

		scaleEClass = createEClass(SCALE);
		createEAttribute(scaleEClass, SCALE__VALUE);

		collectionEClass = createEClass(COLLECTION);
		createEAttribute(collectionEClass, COLLECTION__SELECTION);
		createEAttribute(collectionEClass, COLLECTION__INPUT);

		menuComponentEClass = createEClass(MENU_COMPONENT);
		createEReference(menuComponentEClass, MENU_COMPONENT__CHILDREN);
		createEAttribute(menuComponentEClass, MENU_COMPONENT__TEXT);
		createEAttribute(menuComponentEClass, MENU_COMPONENT__ENABLED);
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
	public void initializePackageContents() {
		if (isInitialized) return;
		isInitialized = true;

		// Initialize package
		setName(eNAME);
		setNsPrefix(eNS_PREFIX);
		setNsURI(eNS_URI);

		// Obtain other dependent packages
		CoreAnnotationsPackage theCoreAnnotationsPackage = (CoreAnnotationsPackage)EPackage.Registry.INSTANCE.getEPackage(CoreAnnotationsPackage.eNS_URI);
		EdpPackage theEdpPackage = (EdpPackage)EPackage.Registry.INSTANCE.getEPackage(EdpPackage.eNS_URI);
		CoreStylesPackage theCoreStylesPackage = (CoreStylesPackage)EPackage.Registry.INSTANCE.getEPackage(CoreStylesPackage.eNS_URI);
		CorePackage theCorePackage = (CorePackage)EPackage.Registry.INSTANCE.getEPackage(CorePackage.eNS_URI);

		// Create type parameters

		// Set bounds for type parameters

		// Add supertypes to classes
		widgetEClass.getESuperTypes().add(theCoreAnnotationsPackage.getAnnotatedElement());
		widgetEClass.getESuperTypes().add(theEdpPackage.getEventDispatcher());
		widgetEClass.getESuperTypes().add(theCoreStylesPackage.getStyledElement());
		progressBarEClass.getESuperTypes().add(this.getAbstractComponent());
		containerEClass.getESuperTypes().add(this.getAbstractComponent());
		textComponentEClass.getESuperTypes().add(this.getAbstractComponent());
		abstractComponentEClass.getESuperTypes().add(this.getWidget());
		abstractButtonEClass.getESuperTypes().add(this.getAbstractComponent());
		pushButtonEClass.getESuperTypes().add(this.getAbstractButton());
		labelEClass.getESuperTypes().add(this.getAbstractComponent());
		radioButtonEClass.getESuperTypes().add(this.getAbstractButton());
		checkBoxEClass.getESuperTypes().add(this.getAbstractButton());
		sliderEClass.getESuperTypes().add(this.getAbstractComponent());
		spinnerEClass.getESuperTypes().add(this.getAbstractComponent());
		scaleEClass.getESuperTypes().add(this.getAbstractComponent());
		collectionEClass.getESuperTypes().add(this.getAbstractComponent());
		menuComponentEClass.getESuperTypes().add(this.getWidget());

		// Initialize classes and features; add operations and parameters
		initEClass(widgetEClass, Widget.class, "Widget", IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

		initEClass(progressBarEClass, ProgressBar.class, "ProgressBar", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getProgressBar_Value(), ecorePackage.getEInt(), "value", null, 0, 1, ProgressBar.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		EOperation op = addEOperation(progressBarEClass, null, "setMaximum", 0, 1, IS_UNIQUE, IS_ORDERED);
		addEParameter(op, ecorePackage.getEInt(), "maximum", 0, 1, IS_UNIQUE, IS_ORDERED);

		addEOperation(progressBarEClass, ecorePackage.getEInt(), "getMaximum", 0, 1, IS_UNIQUE, IS_ORDERED);

		op = addEOperation(progressBarEClass, null, "setMinimum", 0, 1, IS_UNIQUE, IS_ORDERED);
		addEParameter(op, ecorePackage.getEInt(), "minimum", 0, 1, IS_UNIQUE, IS_ORDERED);

		addEOperation(progressBarEClass, ecorePackage.getEInt(), "getMinimum", 0, 1, IS_UNIQUE, IS_ORDERED);

		initEClass(containerEClass, org.eclipse.wazaabi.mm.core.widgets.Container.class, "Container", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getContainer_Children(), this.getAbstractComponent(), null, "children", null, 0, -1, org.eclipse.wazaabi.mm.core.widgets.Container.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		op = addEOperation(containerEClass, this.getAbstractComponent(), "getElementsById", 0, -1, IS_UNIQUE, IS_ORDERED);
		addEParameter(op, ecorePackage.getEString(), "id", 0, 1, IS_UNIQUE, IS_ORDERED);

		initEClass(textComponentEClass, TextComponent.class, "TextComponent", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getTextComponent_Text(), ecorePackage.getEString(), "text", null, 0, 1, TextComponent.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(abstractComponentEClass, AbstractComponent.class, "AbstractComponent", IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getAbstractComponent_Id(), ecorePackage.getEString(), "id", null, 0, 1, AbstractComponent.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getAbstractComponent_Focus(), ecorePackage.getEBoolean(), "focus", "false", 1, 1, AbstractComponent.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		addEOperation(abstractComponentEClass, theCorePackage.getDirection(), "getDirection", 0, 1, IS_UNIQUE, IS_ORDERED);

		op = addEOperation(abstractComponentEClass, null, "setDirection", 0, 1, IS_UNIQUE, IS_ORDERED);
		addEParameter(op, theCorePackage.getDirection(), "direction", 0, 1, IS_UNIQUE, IS_ORDERED);

		addEOperation(abstractComponentEClass, ecorePackage.getEString(), "getToolTipText", 0, 1, IS_UNIQUE, IS_ORDERED);

		op = addEOperation(abstractComponentEClass, null, "setToolTipText", 0, 1, IS_UNIQUE, IS_ORDERED);
		addEParameter(op, ecorePackage.getEString(), "text", 0, 1, IS_UNIQUE, IS_ORDERED);

		addEOperation(abstractComponentEClass, ecorePackage.getEString(), "getErrorText", 0, 1, IS_UNIQUE, IS_ORDERED);

		op = addEOperation(abstractComponentEClass, null, "setErrorText", 0, 1, IS_UNIQUE, IS_ORDERED);
		addEParameter(op, ecorePackage.getEString(), "text", 0, 1, IS_UNIQUE, IS_ORDERED);

		op = addEOperation(abstractComponentEClass, null, "setEnabled", 0, 1, IS_UNIQUE, IS_ORDERED);
		addEParameter(op, ecorePackage.getEBoolean(), "enabled", 0, 1, IS_UNIQUE, IS_ORDERED);

		initEClass(abstractButtonEClass, AbstractButton.class, "AbstractButton", IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

		addEOperation(abstractButtonEClass, ecorePackage.getEString(), "getImage", 0, 1, IS_UNIQUE, IS_ORDERED);

		addEOperation(abstractButtonEClass, ecorePackage.getEString(), "getText", 0, 1, IS_UNIQUE, IS_ORDERED);

		op = addEOperation(abstractButtonEClass, null, "setImage", 0, 1, IS_UNIQUE, IS_ORDERED);
		addEParameter(op, ecorePackage.getEString(), "imageUri", 0, 1, IS_UNIQUE, IS_ORDERED);

		op = addEOperation(abstractButtonEClass, null, "setText", 0, 1, IS_UNIQUE, IS_ORDERED);
		addEParameter(op, ecorePackage.getEString(), "text", 0, 1, IS_UNIQUE, IS_ORDERED);

		initEClass(pushButtonEClass, PushButton.class, "PushButton", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

		initEClass(labelEClass, Label.class, "Label", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

		addEOperation(labelEClass, ecorePackage.getEString(), "getImage", 0, 1, IS_UNIQUE, IS_ORDERED);

		addEOperation(labelEClass, ecorePackage.getEString(), "getText", 0, 1, IS_UNIQUE, IS_ORDERED);

		op = addEOperation(labelEClass, null, "setImage", 0, 1, IS_UNIQUE, IS_ORDERED);
		addEParameter(op, ecorePackage.getEString(), "imageUri", 0, 1, IS_UNIQUE, IS_ORDERED);

		op = addEOperation(labelEClass, null, "setText", 0, 1, IS_UNIQUE, IS_ORDERED);
		addEParameter(op, ecorePackage.getEString(), "text", 0, 1, IS_UNIQUE, IS_ORDERED);

		initEClass(radioButtonEClass, RadioButton.class, "RadioButton", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getRadioButton_Selected(), ecorePackage.getEBoolean(), "selected", null, 0, 1, RadioButton.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(checkBoxEClass, CheckBox.class, "CheckBox", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getCheckBox_Selected(), ecorePackage.getEBoolean(), "selected", null, 0, 1, CheckBox.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(sliderEClass, Slider.class, "Slider", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getSlider_Value(), ecorePackage.getEInt(), "value", null, 0, 1, Slider.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		addEOperation(sliderEClass, theCorePackage.getOrientation(), "getOrientation", 0, 1, IS_UNIQUE, IS_ORDERED);

		op = addEOperation(sliderEClass, null, "setOrientation", 0, 1, IS_UNIQUE, IS_ORDERED);
		addEParameter(op, theCorePackage.getOrientation(), "orientation", 0, 1, IS_UNIQUE, IS_ORDERED);

		op = addEOperation(sliderEClass, null, "setMaximum", 0, 1, IS_UNIQUE, IS_ORDERED);
		addEParameter(op, ecorePackage.getEInt(), "maximum", 0, 1, IS_UNIQUE, IS_ORDERED);

		addEOperation(sliderEClass, ecorePackage.getEInt(), "getMaximum", 0, 1, IS_UNIQUE, IS_ORDERED);

		op = addEOperation(sliderEClass, null, "setMinimum", 0, 1, IS_UNIQUE, IS_ORDERED);
		addEParameter(op, ecorePackage.getEInt(), "minimum", 0, 1, IS_UNIQUE, IS_ORDERED);

		addEOperation(sliderEClass, ecorePackage.getEInt(), "getMinimum", 0, 1, IS_UNIQUE, IS_ORDERED);

		addEOperation(sliderEClass, ecorePackage.getEInt(), "getPageIncrement", 0, 1, IS_UNIQUE, IS_ORDERED);

		op = addEOperation(sliderEClass, null, "setPageIncrement", 0, 1, IS_UNIQUE, IS_ORDERED);
		addEParameter(op, ecorePackage.getEInt(), "pageIncrement", 0, 1, IS_UNIQUE, IS_ORDERED);

		addEOperation(sliderEClass, ecorePackage.getEInt(), "getIncrement", 0, 1, IS_UNIQUE, IS_ORDERED);

		op = addEOperation(sliderEClass, null, "setIncrement", 0, 1, IS_UNIQUE, IS_ORDERED);
		addEParameter(op, ecorePackage.getEInt(), "increment", 0, 1, IS_UNIQUE, IS_ORDERED);

		initEClass(spinnerEClass, Spinner.class, "Spinner", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getSpinner_Value(), ecorePackage.getEInt(), "value", null, 0, 1, Spinner.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		op = addEOperation(spinnerEClass, null, "setMaximum", 0, 1, IS_UNIQUE, IS_ORDERED);
		addEParameter(op, ecorePackage.getEInt(), "maximum", 0, 1, IS_UNIQUE, IS_ORDERED);

		addEOperation(spinnerEClass, ecorePackage.getEInt(), "getMaximum", 0, 1, IS_UNIQUE, IS_ORDERED);

		op = addEOperation(spinnerEClass, null, "setMinimum", 0, 1, IS_UNIQUE, IS_ORDERED);
		addEParameter(op, ecorePackage.getEInt(), "minimum", 0, 1, IS_UNIQUE, IS_ORDERED);

		addEOperation(spinnerEClass, ecorePackage.getEInt(), "getMinimum", 0, 1, IS_UNIQUE, IS_ORDERED);

		op = addEOperation(spinnerEClass, null, "setIncrement", 0, 1, IS_UNIQUE, IS_ORDERED);
		addEParameter(op, ecorePackage.getEInt(), "increment", 0, 1, IS_UNIQUE, IS_ORDERED);

		addEOperation(spinnerEClass, ecorePackage.getEInt(), "getIncrement", 0, 1, IS_UNIQUE, IS_ORDERED);

		op = addEOperation(spinnerEClass, null, "setDigits", 0, 1, IS_UNIQUE, IS_ORDERED);
		addEParameter(op, ecorePackage.getEInt(), "digits", 0, 1, IS_UNIQUE, IS_ORDERED);

		addEOperation(spinnerEClass, ecorePackage.getEInt(), "getDigits", 0, 1, IS_UNIQUE, IS_ORDERED);

		op = addEOperation(spinnerEClass, null, "setTextLimit", 0, 1, IS_UNIQUE, IS_ORDERED);
		addEParameter(op, ecorePackage.getEInt(), "textlimit", 0, 1, IS_UNIQUE, IS_ORDERED);

		addEOperation(spinnerEClass, ecorePackage.getEInt(), "getTextLimit", 0, 1, IS_UNIQUE, IS_ORDERED);

		initEClass(scaleEClass, Scale.class, "Scale", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getScale_Value(), ecorePackage.getEInt(), "value", null, 0, 1, Scale.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		addEOperation(scaleEClass, theCorePackage.getOrientation(), "getOrientation", 0, 1, IS_UNIQUE, IS_ORDERED);

		op = addEOperation(scaleEClass, null, "setOrientation", 0, 1, IS_UNIQUE, IS_ORDERED);
		addEParameter(op, theCorePackage.getOrientation(), "orientation", 0, 1, IS_UNIQUE, IS_ORDERED);

		addEOperation(scaleEClass, ecorePackage.getEInt(), "getMaximum", 0, 1, IS_UNIQUE, IS_ORDERED);

		op = addEOperation(scaleEClass, null, "setMaximum", 0, 1, IS_UNIQUE, IS_ORDERED);
		addEParameter(op, ecorePackage.getEInt(), "maximum", 0, 1, IS_UNIQUE, IS_ORDERED);

		addEOperation(scaleEClass, ecorePackage.getEInt(), "getMinimum", 0, 1, IS_UNIQUE, IS_ORDERED);

		op = addEOperation(scaleEClass, null, "setMinimum", 0, 1, IS_UNIQUE, IS_ORDERED);
		addEParameter(op, ecorePackage.getEInt(), "minimum", 0, 1, IS_UNIQUE, IS_ORDERED);

		addEOperation(scaleEClass, ecorePackage.getEInt(), "getIncrement", 0, 1, IS_UNIQUE, IS_ORDERED);

		op = addEOperation(scaleEClass, null, "setIncrement", 0, 1, IS_UNIQUE, IS_ORDERED);
		addEParameter(op, ecorePackage.getEInt(), "increment", 0, 1, IS_UNIQUE, IS_ORDERED);

		addEOperation(scaleEClass, ecorePackage.getEInt(), "getPageIncrement", 0, 1, IS_UNIQUE, IS_ORDERED);

		op = addEOperation(scaleEClass, null, "setPageIncrement", 0, 1, IS_UNIQUE, IS_ORDERED);
		addEParameter(op, ecorePackage.getEInt(), "pageIncrement", 0, 1, IS_UNIQUE, IS_ORDERED);

		initEClass(collectionEClass, Collection.class, "Collection", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getCollection_Selection(), ecorePackage.getEJavaObject(), "selection", null, 0, -1, Collection.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getCollection_Input(), ecorePackage.getEJavaObject(), "input", null, 0, 1, Collection.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(menuComponentEClass, MenuComponent.class, "MenuComponent", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getMenuComponent_Children(), this.getMenuComponent(), null, "children", null, 0, -1, MenuComponent.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getMenuComponent_Text(), ecorePackage.getEString(), "text", null, 0, 1, MenuComponent.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getMenuComponent_Enabled(), ecorePackage.getEBoolean(), "enabled", null, 0, 1, MenuComponent.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		op = addEOperation(menuComponentEClass, null, "setType", 0, 1, IS_UNIQUE, IS_ORDERED);
		addEParameter(op, ecorePackage.getEString(), "type", 0, 1, IS_UNIQUE, IS_ORDERED);

		addEOperation(menuComponentEClass, ecorePackage.getEString(), "getType", 0, 1, IS_UNIQUE, IS_ORDERED);

		// Create annotations
		// http://www.wazaabi.org/style/property/definition
		createDefinitionAnnotations();
		// http://www.wazaabi.org/Annotation
		createAnnotationAnnotations();
	}

	/**
	 * Initializes the annotations for <b>http://www.wazaabi.org/style/property/definition</b>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void createDefinitionAnnotations() {
		String source = "http://www.wazaabi.org/style/property/definition";		
		addAnnotation
		  (progressBarEClass, 
		   source, 
		   new String[] {
			 "name", "orientation",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=OrientationRule",
			 "default", "value=HORIZONTAL"
		   });		
		addAnnotation
		  (progressBarEClass, 
		   source, 
		   new String[] {
			 "name", "maximum",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=IntRule",
			 "default", "value=100"
		   });		
		addAnnotation
		  (progressBarEClass, 
		   source, 
		   new String[] {
			 "name", "minimum",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=IntRule",
			 "default", "value=0"
		   });								
		addAnnotation
		  (textComponentEClass, 
		   source, 
		   new String[] {
			 "name", "horizontal-scrollbar",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=ScrollBarRule"
		   });		
		addAnnotation
		  (textComponentEClass, 
		   source, 
		   new String[] {
			 "name", "vertical-scrollbar",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=ScrollBarRule"
		   });		
		addAnnotation
		  (textComponentEClass, 
		   source, 
		   new String[] {
			 "name", "multi-line",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=BooleanRule"
		   });		
		addAnnotation
		  (abstractComponentEClass, 
		   source, 
		   new String[] {
			 "name", "tooltip-text",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=StringRule"
		   });		
		addAnnotation
		  (abstractComponentEClass, 
		   source, 
		   new String[] {
			 "name", "background-color",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=ColorRule"
		   });		
		addAnnotation
		  (abstractComponentEClass, 
		   source, 
		   new String[] {
			 "name", "foreground-color",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=ColorRule"
		   });		
		addAnnotation
		  (abstractComponentEClass, 
		   source, 
		   new String[] {
			 "name", "font",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=FontRule"
		   });		
		addAnnotation
		  (abstractComponentEClass, 
		   source, 
		   new String[] {
			 "name", "direction",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=DirectionRule",
			 "default", "value=LEFT_TO_RIGHT"
		   });		
		addAnnotation
		  (abstractComponentEClass, 
		   source, 
		   new String[] {
			 "name", "enabled",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=BooleanRule",
			 "default", "value=true"
		   });		
		addAnnotation
		  (abstractComponentEClass, 
		   source, 
		   new String[] {
			 "name", "error-text",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=StringRule"
		   });		
		addAnnotation
		  (abstractComponentEClass, 
		   source, 
		   new String[] {
			 "name", "visible",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=BooleanRule",
			 "default", "value=true"
		   });									
		addAnnotation
		  (abstractButtonEClass, 
		   source, 
		   new String[] {
			 "name", "text",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=StringRule"
		   });		
		addAnnotation
		  (abstractButtonEClass, 
		   source, 
		   new String[] {
			 "name", "image",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=ImageRule"
		   });						
		addAnnotation
		  (labelEClass, 
		   source, 
		   new String[] {
			 "name", "text",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=StringRule"
		   });		
		addAnnotation
		  (labelEClass, 
		   source, 
		   new String[] {
			 "name", "image",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=ImageRule"
		   });		
		addAnnotation
		  (labelEClass, 
		   source, 
		   new String[] {
			 "name", "lookandfeel",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=HyperlinkRule"
		   });						
		addAnnotation
		  (sliderEClass, 
		   source, 
		   new String[] {
			 "name", "orientation",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=OrientationRule",
			 "default", "value=HORIZONTAL"
		   });		
		addAnnotation
		  (sliderEClass, 
		   source, 
		   new String[] {
			 "name", "maximum",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=IntRule",
			 "default", "value=100"
		   });		
		addAnnotation
		  (sliderEClass, 
		   source, 
		   new String[] {
			 "name", "minimum",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=IntRule",
			 "default", "value=0"
		   });		
		addAnnotation
		  (sliderEClass, 
		   source, 
		   new String[] {
			 "name", "increment",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=IntRule",
			 "default", "value=1"
		   });		
		addAnnotation
		  (sliderEClass, 
		   source, 
		   new String[] {
			 "name", "pageIncrement",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=IntRule",
			 "default", "value=10"
		   });												
		addAnnotation
		  (spinnerEClass, 
		   source, 
		   new String[] {
			 "name", "increment",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=IntRule",
			 "default", "value=1"
		   });		
		addAnnotation
		  (spinnerEClass, 
		   source, 
		   new String[] {
			 "name", "minimum",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=IntRule",
			 "default", "value=0"
		   });		
		addAnnotation
		  (spinnerEClass, 
		   source, 
		   new String[] {
			 "name", "maximum",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=IntRule",
			 "default", "value=100"
		   });		
		addAnnotation
		  (spinnerEClass, 
		   source, 
		   new String[] {
			 "name", "digits",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=IntRule",
			 "default", "value=0"
		   });		
		addAnnotation
		  (spinnerEClass, 
		   source, 
		   new String[] {
			 "name", "textlimit",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=IntRule",
			 "default", "value=100"
		   });												
		addAnnotation
		  (scaleEClass, 
		   source, 
		   new String[] {
			 "name", "orientation",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=OrientationRule",
			 "default", "value=HORIZONTAL"
		   });		
		addAnnotation
		  (scaleEClass, 
		   source, 
		   new String[] {
			 "name", "maximum",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=IntRule",
			 "default", "value=100"
		   });		
		addAnnotation
		  (scaleEClass, 
		   source, 
		   new String[] {
			 "name", "minimum",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=IntRule",
			 "default", "value=0"
		   });		
		addAnnotation
		  (scaleEClass, 
		   source, 
		   new String[] {
			 "name", "increment",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=IntRule",
			 "default", "value=1"
		   });		
		addAnnotation
		  (scaleEClass, 
		   source, 
		   new String[] {
			 "name", "pageIncrement",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=IntRule",
			 "default", "value=10"
		   });												
		addAnnotation
		  (collectionEClass, 
		   source, 
		   new String[] {
			 "name", "lookandfeel",
			 "type", "package=http://www.wazaabi.org/core/styles/collections\r\nEClass=LookAndFeelRule",
			 "default", "value=TABLE"
		   });		
		addAnnotation
		  (collectionEClass, 
		   source, 
		   new String[] {
			 "name", "allow-row-selection",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=BooleanRule",
			 "default", "value=true"
		   });		
		addAnnotation
		  (collectionEClass, 
		   source, 
		   new String[] {
			 "name", "show-horizontal-lines",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=BooleanRule"
		   });		
		addAnnotation
		  (collectionEClass, 
		   source, 
		   new String[] {
			 "name", "header-visible",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=BooleanRule"
		   });		
		addAnnotation
		  (menuComponentEClass, 
		   source, 
		   new String[] {
			 "name", "direction",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=DirectionRule",
			 "default", "value=LEFT_TO_RIGHT"
		   });		
		addAnnotation
		  (menuComponentEClass, 
		   source, 
		   new String[] {
			 "name", "image",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=ImageRule"
		   });		
		addAnnotation
		  (menuComponentEClass, 
		   source, 
		   new String[] {
			 "name", "type",
			 "type", "package=http://www.wazaabi.org/core/styles\r\nEClass=StringRule",
			 "default", "value=push"
		   });			
	}

	/**
	 * Initializes the annotations for <b>http://www.wazaabi.org/Annotation</b>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void createAnnotationAnnotations() {
		String source = "http://www.wazaabi.org/Annotation";										
		addAnnotation
		  (getContainer_Children(), 
		   source, 
		   new String[] {
			 "doc", "Children about .... .... "
		   });																																																																																							
		addAnnotation
		  (getMenuComponent_Children(), 
		   source, 
		   new String[] {
			 "doc", "Children about .... .... "
		   });
	}

} //CoreWidgetsPackageImpl
