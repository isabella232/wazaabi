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

package org.eclipse.wazaabi.engine.swt.tests.widgets;

import org.eclipse.swt.widgets.Button;
import org.eclipse.wazaabi.engine.swt.tests.SWTUtils;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;
import org.junit.Assert;
import org.junit.Test;

/**
 * on teste le texte affiché sur par un bouton en 'settant' la property dans le modèle
 * puis en vérifiant l'affichage par introspection du SWT Button
 * @author greg
 *
 */
public class TestPushButton extends AbstractTestWidget {

	private static final String TEXT = "Hello World"; //$NON-NLS-1$
	private PushButton pushButton;

	@Override
	public void before() {
		super.before();
		// create the PushButton
		pushButton = CoreWidgetsFactory.eINSTANCE.createPushButton();
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}
	
	
	/**
	 * la mise à jour avant le rendu (setContents() ) par le viewer
	 * on vérifie que le viewer prenne bien en compte les données d'initialisation
	 * @throws Exception
	 */
	@Test
	public void testModelButtonTextSetBeforeViewerSetContentsEqualsSWTButtonText() {

		// set the PushButton's text
		StringRule textRule = CoreStylesFactory.eINSTANCE.createStringRule();
		textRule.setPropertyName("text"); //$NON-NLS-1$
		textRule.setValue(TEXT);
		pushButton.getStyleRules().add(textRule);

		// render the content
		viewer.setContents(pushButton);

		Button swtButton = (Button) SWTUtils.getWidget(viewer, pushButton);
		Assert.assertEquals(TEXT, swtButton.getText());
	}

	/**
	 * la mise à jour pendant le rendu par le viewer.
	 * on vérifie que le viewer détecte le changement et modifie bien le SWT affiché.
	 * @throws Exception
	 */
	@Test
	public void testModelButtonTextSetAfterViewerSetContentsEqualsSWTButtonText() {

		// Set the content
		viewer.setContents(pushButton);

		Button swtButton = (Button) SWTUtils.getWidget(viewer, pushButton);

		// set the PushButton's text
		StringRule textRule = CoreStylesFactory.eINSTANCE.createStringRule();
		textRule.setPropertyName("text"); //$NON-NLS-1$
		textRule.setValue(TEXT);
		pushButton.getStyleRules().add(textRule);

		Assert.assertEquals(TEXT, swtButton.getText());

	}

}
