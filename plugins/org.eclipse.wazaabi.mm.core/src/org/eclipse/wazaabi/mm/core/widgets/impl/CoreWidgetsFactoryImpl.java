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

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.impl.EFactoryImpl;

import org.eclipse.emf.ecore.plugin.EcorePlugin;

import org.eclipse.wazaabi.mm.core.widgets.*;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Factory</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class CoreWidgetsFactoryImpl extends EFactoryImpl implements CoreWidgetsFactory {
	/**
	 * Creates the default factory implementation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static CoreWidgetsFactory init() {
		try {
			CoreWidgetsFactory theCoreWidgetsFactory = (CoreWidgetsFactory)EPackage.Registry.INSTANCE.getEFactory("http://www.wazaabi.org/core/widgets"); 
			if (theCoreWidgetsFactory != null) {
				return theCoreWidgetsFactory;
			}
		}
		catch (Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}
		return new CoreWidgetsFactoryImpl();
	}

	/**
	 * Creates an instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CoreWidgetsFactoryImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EObject create(EClass eClass) {
		switch (eClass.getClassifierID()) {
			case CoreWidgetsPackage.PROGRESS_BAR: return createProgressBar();
			case CoreWidgetsPackage.CONTAINER: return createContainer();
			case CoreWidgetsPackage.TEXT_COMPONENT: return createTextComponent();
			case CoreWidgetsPackage.PUSH_BUTTON: return createPushButton();
			case CoreWidgetsPackage.LABEL: return createLabel();
			case CoreWidgetsPackage.RADIO_BUTTON: return createRadioButton();
			case CoreWidgetsPackage.CHECK_BOX: return createCheckBox();
			case CoreWidgetsPackage.SLIDER: return createSlider();
			case CoreWidgetsPackage.SPINNER: return createSpinner();
			case CoreWidgetsPackage.SCALE: return createScale();
			case CoreWidgetsPackage.COLLECTION: return createCollection();
			case CoreWidgetsPackage.MENU_COMPONENT: return createMenuComponent();
			case CoreWidgetsPackage.SEPARATOR: return createSeparator();
			default:
				throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ProgressBar createProgressBar() {
		ProgressBarImpl progressBar = new ProgressBarImpl();
		return progressBar;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public org.eclipse.wazaabi.mm.core.widgets.Container createContainer() {
		ContainerImpl container = new ContainerImpl();
		return container;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public TextComponent createTextComponent() {
		TextComponentImpl textComponent = new TextComponentImpl();
		return textComponent;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public PushButton createPushButton() {
		PushButtonImpl pushButton = new PushButtonImpl();
		return pushButton;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Label createLabel() {
		LabelImpl label = new LabelImpl();
		return label;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public RadioButton createRadioButton() {
		RadioButtonImpl radioButton = new RadioButtonImpl();
		return radioButton;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CheckBox createCheckBox() {
		CheckBoxImpl checkBox = new CheckBoxImpl();
		return checkBox;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Slider createSlider() {
		SliderImpl slider = new SliderImpl();
		return slider;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Spinner createSpinner() {
		SpinnerImpl spinner = new SpinnerImpl();
		return spinner;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Scale createScale() {
		ScaleImpl scale = new ScaleImpl();
		return scale;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Collection createCollection() {
		CollectionImpl collection = new CollectionImpl();
		return collection;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public MenuComponent createMenuComponent() {
		MenuComponentImpl menuComponent = new MenuComponentImpl();
		return menuComponent;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Separator createSeparator() {
		SeparatorImpl separator = new SeparatorImpl();
		return separator;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CoreWidgetsPackage getCoreWidgetsPackage() {
		return (CoreWidgetsPackage)getEPackage();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @deprecated
	 * @generated
	 */
	@Deprecated
	public static CoreWidgetsPackage getPackage() {
		return CoreWidgetsPackage.eINSTANCE;
	}

} //CoreWidgetsFactoryImpl
