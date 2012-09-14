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

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Text;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.junit.Assert;
import org.junit.Test;

public class TestBooleanRuleMultiLineTextComponent extends
		AbstractTestBooleanRuleMultiLine {

	private org.eclipse.wazaabi.mm.core.widgets.TextComponent widget;

	@Override
	public void before() {
		super.before();
		widget = CoreWidgetsFactory.eINSTANCE.createTextComponent();
	}

	@Override
	public void after() {
		mainShell.open();
		super.after();
	}

	@Test
	public void testModelSetMultiLineBeforeViewerSetContentsEqualsSWTMultiLine() {
		Text swtText = (Text) ModelSetRule(true, widget, booleanRuleMultiLine);
		Assert.assertEquals(SWT.MULTI, swtText.getStyle() & SWT.MULTI);
	}

	@Test
	public void testModelSetMultiLineAfterViewerSetContentsEqualsSWTMultiLine() {
		Text swtText = (Text) ModelSetRule(false, widget, booleanRuleMultiLine);
		Assert.assertEquals(SWT.MULTI, swtText.getStyle() & SWT.MULTI);
	}

	@Test
	public void testModelSetMultipleMultiLineBeforeViewerSetContentsEqualsSWTMultiLine() {
		Text swtText = (Text) ModelSetMultipleRule(true, widget,
				booleanRuleMultiLine, booleanRuleMultiLine2);
		Assert.assertEquals(SWT.MULTI, swtText.getStyle() & SWT.MULTI);
	}

	@Test
	public void testModelSetMultipleMultiLineAfterViewerSetContentsEqualsSWTMultiLine() {
		Text swtText = (Text) ModelSetMultipleRule(false, widget,
				booleanRuleMultiLine, booleanRuleMultiLine2);
		Assert.assertEquals(SWT.MULTI, swtText.getStyle() & SWT.MULTI);
	}

	@Test
	public void testModelMoveMultiLineBeforeViewerSetContentsEqualsSWTMultiLine() {
		Text swtText = (Text) ModelMoveRule(true, widget, booleanRuleMultiLine,
				booleanRuleMultiLine2, "multi-line");
		Assert.assertEquals(SWT.SINGLE, swtText.getStyle() & SWT.SINGLE);
	}

	@Test
	public void testModelMoveMultiLineAfterViewerSetContentsEqualsSWTMultiLine() {
		Text swtText = (Text) ModelMoveRule(false, widget,
				booleanRuleMultiLine, booleanRuleMultiLine2, "multi-line");
		Assert.assertEquals(SWT.SINGLE, swtText.getStyle() & SWT.SINGLE);
	}

	@Test
	public void testModelRemoveMultiLineByRemoveBeforeViewerSetContentsEqualsSWTMultiLine() {
		Text swtText = (Text) ModelRemoveRuleByRemove(true, widget,
				booleanRuleMultiLine, booleanRuleMultiLine2, "multi-line");
		Assert.assertEquals(SWT.SINGLE, swtText.getStyle() & SWT.SINGLE);
	}

	@Test
	public void testModelRemoveMultiLineByRemoveAfterViewerSetContentsEqualsSWTMultiLine() {
		Text swtText = (Text) ModelRemoveRuleByRemove(false, widget,
				booleanRuleMultiLine, booleanRuleMultiLine2, "multi-line");
		Assert.assertEquals(SWT.SINGLE, swtText.getStyle() & SWT.SINGLE);
	}

	@Test
	public void testModelRemoveMultiLineByRenameBeforeViewerSetContentsEqualsSWTMultiLine() {
		Text swtText = (Text) ModelRemoveRuleByRename(true, widget,
				booleanRuleMultiLine, booleanRuleMultiLine2, "multi-line");
		Assert.assertEquals(SWT.SINGLE, swtText.getStyle() & SWT.SINGLE);
	}

	@Test
	public void testModelRemoveMultiLineByRenameAfterViewerSetContentsEqualsSWTMultiLine() {
		Text swtText = (Text) ModelRemoveRuleByRename(false, widget,
				booleanRuleMultiLine, booleanRuleMultiLine2, "multi-line");
		Assert.assertEquals(SWT.SINGLE, swtText.getStyle() & SWT.SINGLE);
	}

	@Test
	public void testModelModifyMultiLineBeforeSetContentsEqualsSWTMultiLine() {
		Text swtText = (Text) ModelModifyRule(true, widget);
		Assert.assertEquals(SWT.SINGLE, swtText.getStyle() & SWT.SINGLE);
	}

	@Test
	public void testModelModifyMultiLineAfterSetContentsEqualsSWTMultiLine() {
		Text swtText = (Text) ModelModifyRule(false, widget);
		Assert.assertEquals(SWT.SINGLE, swtText.getStyle() & SWT.SINGLE);
	}

}
