package org.eclipse.wazaabi.demo.ecna2014.core;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.wazaabi.demo.ecna2014.core.model.ModelFactory;
import org.eclipse.wazaabi.demo.ecna2014.core.model.Winnie;
import org.eclipse.wazaabi.engine.core.gef.EditPartViewer;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.TextComponent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class DemoUI {

    private static final Logger log = LoggerFactory.getLogger(DemoUI.class);


    public static Container create(EditPartViewer viewer, String layoutActionPrefix) {
        Container container = CoreWidgetsFactory.eINSTANCE.createContainer();
        container.set("input", createBusinessModel());

        container.getChildren().add(Utils.createButton("Change Layout", "ChangeLayoutAction", layoutActionPrefix));
        container.getChildren().add(Utils.createLabel("Enter some text into the first field:"));

        TextComponent text1 = CoreWidgetsFactory.eINSTANCE.createTextComponent();
        container.getChildren().add(text1);
        text1.getHandlers().add(Utils.createBinding(false, "$input/@name"));

        container.getChildren().add(Utils.createLabel("Which is bound to the second field:"));

        TextComponent text2 = CoreWidgetsFactory.eINSTANCE.createTextComponent();
        container.getChildren().add(text2);
        text2.getHandlers().add(Utils.createBinding(true, "$input/@name"));

        container.getChildren().add(Utils.createButton("Say Hello", "SayHelloAction"));
        container.getChildren().add(Utils.createButton("Save text", "ReplaceTextWithLabelAction"));

        viewer.setCodeLocatorBaseUri("urn:java:");

        return container;
    }

    private static Winnie createBusinessModel() {
        final Winnie w = ModelFactory.eINSTANCE.createWinnie();
        w.setName("Pooh");

        w.eAdapters().add(new AdapterImpl() {
            @Override
            public void notifyChanged(Notification notification) {
                log.info(" > Winnie model changed: {}", w);
            }
        });
        return w;
    }
}
