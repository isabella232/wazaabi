/*******************************************************************************
 * Copyright (c) 2012 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.ui.editors.viewer.bindingrules;

import org.eclipse.wazaabi.ide.ui.editors.viewer.FFactory;
import org.eclipse.wazaabi.mm.edp.events.EDPEventsFactory;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.events.PropertyChangedEvent;
import org.eclipse.wazaabi.mm.edp.handlers.Binding;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersFactory;
import org.eclipse.wazaabi.mm.edp.handlers.StringParameter;

public class MappingUtils {

    private static FFactory ff = null;

    protected static void initializeFFactory() {
        ff.registerContainingInstance(new OnContainerMappingRules());
        ff.registerContainingInstance(new OnTextComponentMapping());
    }

    public static FFactory getFFactory() {
        if (ff == null) {
            ff = new FFactory();
            initializeFFactory();
        }
        return ff;
    }

    public static Binding createBinding(String sourcePath, String targetPath) {
        Binding binding = EDPHandlersFactory.eINSTANCE.createBinding();
        StringParameter source = EDPHandlersFactory.eINSTANCE
                .createStringParameter();
        source.setName("source");
        StringParameter target = EDPHandlersFactory.eINSTANCE
                .createStringParameter();
        target.setName("target");
        source.setValue(sourcePath);
        target.setValue(targetPath);
        binding.getParameters().add(source);
        binding.getParameters().add(target);
        return binding;
    }

    public static void addPropertyChangedEvent(Binding binding, String path) {
        PropertyChangedEvent event = EDPEventsFactory.eINSTANCE
                .createPropertyChangedEvent();
        event.setPath(path);
        binding.getEvents().add(event);
    }

    public static void addEvent(Binding binding, String id) {
        Event event = EDPEventsFactory.eINSTANCE.createEvent();
        event.setId(id);
        binding.getEvents().add(event);
    }

}