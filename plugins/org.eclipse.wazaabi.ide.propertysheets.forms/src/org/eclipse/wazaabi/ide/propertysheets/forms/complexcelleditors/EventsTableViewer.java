package org.eclipse.wazaabi.ide.propertysheets.forms.complexcelleditors;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.wazaabi.ide.propertysheets.TargetChangeListener;
import org.eclipse.wazaabi.ide.propertysheets.descriptors.EventDescriptorFactory;
import org.eclipse.wazaabi.ide.propertysheets.viewers.DescriptorLabelColumn;
import org.eclipse.wazaabi.ide.propertysheets.viewers.DescriptorLabelColumn.LabelPrinter;

public class EventsTableViewer extends TableViewer {

	private final TargetChangeListener listener;

	public EventsTableViewer(Composite parent, TargetChangeListener listener) {
		super(parent);
		this.listener = listener;
	}

	public EventsTableViewer(Composite parent, int style,
			TargetChangeListener listener) {
		super(parent, style);
		this.listener = listener;
	}

	public EventsTableViewer(Table table, TargetChangeListener listener) {
		super(table);
		this.listener = listener;
	}

	protected void createColumn() {
		new DescriptorLabelColumn(this, listener, new EventDescriptorFactory(),
				null, new LabelPrinter() {

					@Override
					public String getLabel(EObject item) {
						// TODO Auto-generated method stub
						return null;
					}
				});
	}
}
