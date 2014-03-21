/*******************************************************************************
 * Copyright (c) 2014 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.propertysheets.forms.complexcelleditors;

import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.wazaabi.ide.propertysheets.TargetChangeListener;
import org.eclipse.wazaabi.ide.propertysheets.complexcelleditors.AbstractUIContentsDescriptor;

public abstract class AbstractDetailsSection extends
		AbstractUIContentsDescriptor {

	private FormToolkit formToolkit = null;

	@Override
	public final Control createContents(Control parent,
			TargetChangeListener targetChangeListener) {
		assert parent instanceof Section;
		formToolkit = new FormToolkit(parent.getDisplay());
		Control control = createSection((Section) parent, targetChangeListener);
		if (control != null && !control.isDisposed())
			control.addDisposeListener(new DisposeListener() {

				public void widgetDisposed(DisposeEvent e) {
					if (formToolkit != null)
						formToolkit.dispose();
				}
			});
		return control;
	}

	abstract protected Control createSection(Section parent,
			TargetChangeListener targetChangeListener);

	public FormToolkit getFormToolkit() {
		return formToolkit;
	}

}
