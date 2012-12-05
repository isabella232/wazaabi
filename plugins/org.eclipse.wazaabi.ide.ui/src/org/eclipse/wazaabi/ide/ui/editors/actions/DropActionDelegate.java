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

package org.eclipse.wazaabi.ide.ui.editors.actions;

import org.eclipse.core.resources.IContainer;
import org.eclipse.ui.part.IDropActionDelegate;
import org.eclipse.wazaabi.ide.ui.editors.viewer.ModelDescriptor;
import org.eclipse.wazaabi.ide.ui.editors.viewer.ModelDescriptorTransfert;


public class DropActionDelegate implements IDropActionDelegate {

    public boolean run(Object source, Object target) {
        if (target instanceof IContainer) {
            ModelDescriptor modelDescriptors[] = ModelDescriptorTransfert
                    .getInstance().fromByteArray((byte[]) source);
//            IContainer parent = (IContainer) target;
            for (ModelDescriptor modelDescriptor : modelDescriptors)
                System.out.println(ModelDescriptor.getEObject(modelDescriptor));
            return true;
        }
        return false;

    }
}