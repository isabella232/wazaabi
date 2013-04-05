package org.eclipse.wazaabi.ide.ui.palette;

import java.util.List;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.gef.palette.CreationToolEntry;
import org.eclipse.wazaabi.ide.ui.editors.EMFPaletteCreationFactory;
import org.eclipse.wazaabi.ide.ui.internal.Activator;

public class PaletteFactoryUtils {

	public static void addCreationToolEntry(List<CreationToolEntry> entries,
			String label, String shortDesc, EClass eClass) {
		CreationToolEntry entry = new CreationToolEntry(label, shortDesc,
				new EMFPaletteCreationFactory(eClass), Activator.getDefault()
						.getImageRegistry().getDescriptor(eClass.getName()),
				null);
		entries.add(entry);
	}
}
