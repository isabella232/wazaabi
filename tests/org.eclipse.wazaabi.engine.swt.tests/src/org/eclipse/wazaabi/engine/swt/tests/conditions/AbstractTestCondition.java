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

package org.eclipse.wazaabi.engine.swt.tests.conditions;

import org.eclipse.wazaabi.engine.swt.tests.widgets.AbstractTestWidget;
import org.eclipse.wazaabi.mm.core.widgets.Widget;
import org.eclipse.wazaabi.mm.edp.handlers.Condition;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersFactory;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class AbstractTestCondition extends AbstractTestWidget {
	
	protected Condition condition;
	protected Condition condition2;
	
	protected String id = "Condition";
	protected String id2 = "Condition2";			
	
	protected String uri = "urn:java:org.eclipse.wazaabi.engine.swt.tests.conditions.TrueCondition";
	protected String uri2 = "urn:java:org.eclipse.wazaabi.engine.swt.tests.conditions.FalseCondition";
	
	@Override
	public void before() {
		condition = EDPHandlersFactory.eINSTANCE.createCondition();
		condition.setUri(uri);
		//condition.setId(id);
		condition2 = EDPHandlersFactory.eINSTANCE.createCondition();
		condition2.setUri(uri2);
		//condition2.setId(id2);
		super.before();
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}
	
	protected Widget ModelSetActionAndCondition(boolean before, Widget widget, EventHandler eventHandler, Condition condition) {
		if(!before)
			viewer.setContents(widget);
		
		widget.getHandlers().add(eventHandler);
		widget.getHandlers().get(0).getConditions().add(condition);

		if(before)
			viewer.setContents(widget);
		
		return widget;
		//Button swtButton = (Button) SWTUtils.getWidget(viewer,widget); 
		//Assert.assertEquals(expected, swtButton.getText());
	}
	
	protected Widget ModelSetActionAndMultipleConditions(boolean before, Widget widget, EventHandler eventHandler, Condition condition, Condition condition2) {
		if(!before)
			viewer.setContents(widget);
		
		widget.getHandlers().add(eventHandler);
		widget.getHandlers().get(0).getConditions().add(condition);
		widget.getHandlers().get(0).getConditions().add(condition2);

		if(before)
			viewer.setContents(widget);
		
		return widget; 
		
	}
	
	protected Widget ModelSetActionAndConditionAndRemoveCondition(boolean before, Widget widget, EventHandler eventHandler, Condition condition){
		if(!before)
			viewer.setContents(widget);
		
		widget.getHandlers().add(eventHandler);
		widget.getHandlers().get(0).getConditions().add(condition);
		widget.getHandlers().get(0).getConditions().remove(condition);
		
		if(before)
			viewer.setContents(widget);
		
		return widget;
		
	}
	
}
