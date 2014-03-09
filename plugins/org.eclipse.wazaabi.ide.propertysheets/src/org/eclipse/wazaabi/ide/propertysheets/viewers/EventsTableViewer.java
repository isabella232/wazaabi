package org.eclipse.wazaabi.ide.propertysheets.viewers;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.wazaabi.ide.propertysheets.TargetChangeListener;
import org.eclipse.wazaabi.ide.propertysheets.descriptors.EventDescriptorFactory;
import org.eclipse.wazaabi.ide.propertysheets.viewers.DescriptorLabelColumn.LabelPrinter;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.events.impl.EventImpl;

public class EventsTableViewer extends TableViewer {

	private final EventImpl EVENT_FOR_INSERTION = new EventImpl() {
	};
	private final TargetChangeListener listener;

	public EventsTableViewer(Composite parent, TargetChangeListener listener) {
		this(parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER,
				listener);
	}

	public EventsTableViewer(Composite parent, int style,
			TargetChangeListener listener) {
		this(new Table(parent, style), listener);
	}

	public EventsTableViewer(Table table, TargetChangeListener listener) {
		super(table);
		this.listener = listener;
		setContentProvider(new EventContentProvider(EVENT_FOR_INSERTION));
		createColumn();
	}

	protected void createColumn() {
		new DescriptorLabelColumn(this, listener, new EventDescriptorFactory(),
				EVENT_FOR_INSERTION, new LabelPrinter() {

					@Override
					public String getLabel(EObject item) {
						return ((Event) item).getId();
					}
				});
	}
}
