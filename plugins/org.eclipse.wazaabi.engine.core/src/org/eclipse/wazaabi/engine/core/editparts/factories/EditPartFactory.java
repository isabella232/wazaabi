/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.eclipse.wazaabi.engine.core.editparts.factories;

import org.eclipse.wazaabi.engine.core.gef.EditPartViewer;
import org.eclipse.wazaabi.engine.edp.ComponentFactory;

/**
 * A factory for creating new EditParts. {@link EditPartViewer EditPartViewers}
 * can be configured with an <code>EditPartFactory</code>. Whenever an
 * <code>EditPart</code> in that viewer needs to create another EditPart, it can
 * use the Viewer's factory. The factory is also used by the viewer whenever
 * {@link EditPartViewer#setContents(Object)} is called.
 * 
 * @since 2.0
 */
public interface EditPartFactory extends ComponentFactory {

}
