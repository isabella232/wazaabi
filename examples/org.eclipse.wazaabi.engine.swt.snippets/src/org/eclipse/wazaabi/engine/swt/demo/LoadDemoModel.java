/*******************************************************************************
 * Copyright (c) 2008 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.engine.swt.demo;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.emf.ecore.xmi.impl.XMIResourceFactoryImpl;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.engine.swt.commons.nonosgi.SWTHelper;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.locator.urn.java.nonosgi.URNJavaLocatorHelper;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;

/**
 * This example shows how to load a UI model from a resource file in a
 * standalone java application.
 * 
 * @author Olivier
 * 
 */
public class LoadDemoModel {

	public static void main(String[] args) {

		// init SWT Engine in standalone mode
		SWTHelper.init();
		URNJavaLocatorHelper.init();

		// create the shell
		Display display = new Display();
		Shell mainShell = new Shell(display, SWT.SHELL_TRIM);
		mainShell.setLayout(new FillLayout());
		mainShell.setSize(400, 550);
		SWTControlViewer viewer = new SWTControlViewer(mainShell);

		// EMF requirements for standalone applications
		Resource.Factory.Registry.INSTANCE.getExtensionToFactoryMap().put(
				"ui", new XMIResourceFactoryImpl());
		CoreWidgetsPackage.eINSTANCE.eClass();

		// load the EMF resource
		Resource resource = new ResourceSetImpl().getResource(
				URI.createURI("models/DemoModel.ui"), true); //$NON-NLS-1$

		// inject the resource's root into the viewer
		viewer.setContents(resource.getEObject("/"));

		mainShell.open();

		while (!mainShell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();
	}
}
