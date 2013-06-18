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

package org.eclipse.wazaabi.engine.swt.snippets;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.engine.swt.nonosgi.SWTHelper;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.mm.core.styles.BooleanRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;
import org.eclipse.wazaabi.mm.core.widgets.TextComponent;

public class SingleTextComponent {

	public static void main(String[] args) {

		// create the shell
		Display display = new Display();
		Shell mainShell = new Shell(display, SWT.SHELL_TRIM);
		mainShell.setLayout(new FillLayout());
		mainShell.setSize(300, 300);

		// create the viewer
		SWTControlViewer viewer = new SWTControlViewer(mainShell);
		// init SWT Engine in standalone mode
		SWTHelper.init(viewer);
		
		// create a PushButton
		PushButton pushButton = CoreWidgetsFactory.eINSTANCE.createPushButton();
		pushButton.setText("Hello World"); //$NON-NLS-1$
		
		TextComponent text = CoreWidgetsFactory.eINSTANCE.createTextComponent();
		//StringRule stringRule = StylesFactory.eINSTANCE.createStringRule();
		//stringRule.setPropertyName("text");
		//stringRule.setValue("Hello World");
		//text.getStyleRules().add(stringRule);
		
		BooleanRule booleanRule = CoreStylesFactory.eINSTANCE.createBooleanRule();
		booleanRule.setPropertyName("multi-line");
		booleanRule.setValue(false);
		text.getStyleRules().add(booleanRule);
		
		text.setText("testisfg,rkgjrlksgrekkkkkkkkkkkkkkkkkkkk \n"+"kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkvnfdkjnvbjefkdnbjdsfnvbjfkdsbndjfkwbndjfkbnjfqdkbnefqd");
		

		// inject the button into the viewer
		//viewer.setContents(pushButton);
		viewer.setContents(text);

		mainShell.open();

		while (!mainShell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();
	}
}
