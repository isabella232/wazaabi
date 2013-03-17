package org.eclipse.wazaabi.ide.ui.editors.viewer;

import org.eclipse.gef.EditPartViewer;
import org.eclipse.gef.dnd.SimpleObjectTransfer;

/**
 * Used to move EditParts around in a single viewer.
 */
public class TreeViewerTransfer extends SimpleObjectTransfer {

	private static final TreeViewerTransfer INSTANCE = new TreeViewerTransfer();
	private static final String TYPE_NAME = "Local Transfer"//$NON-NLS-1$
			+ System.currentTimeMillis() + ":" + INSTANCE.hashCode();//$NON-NLS-1$
	private static final int TYPEID = registerType(TYPE_NAME);

	private static EditPartViewer viewer;

	/**
	 * Returns the singleton instance.
	 * 
	 * @return The singleton instance
	 */
	public static TreeViewerTransfer getInstance() {
		return INSTANCE;
	}

	private TreeViewerTransfer() {
	}

	/**
	 * @see org.eclipse.swt.dnd.Transfer#getTypeIds()
	 */
	protected int[] getTypeIds() {
		return new int[] { TYPEID };
	}

	/**
	 * @see org.eclipse.swt.dnd.Transfer#getTypeNames()
	 */
	protected String[] getTypeNames() {
		return new String[] { TYPE_NAME };
	}

	/**
	 * Returns the viewer where the drag started.
	 * 
	 * @return The viewer where the drag started
	 */
	public EditPartViewer getViewer() {
		return viewer;
	}

	/**
	 * Sets the viewer where the drag started.
	 * 
	 * @param epv
	 *            The viewer
	 */
	public void setViewer(EditPartViewer epv) {
		viewer = epv;
	}

}
