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

package org.eclipse.wazaabi.engine.fx.views.updman;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.wazaabi.engine.core.views.WidgetView;


/** An UpdateManager that asynchronously updates the affected figures. */
public class DeferredUpdateManager extends UpdateManager {

    /** Calls {@link DeferredUpdateManager#performUpdate()}. */
    protected class UpdateRequest implements Runnable {

        public UpdateRequest() {
            super();
        }

        /** Calls {@link DeferredUpdateManager#performUpdate()}. */
        public void run() {
            performUpdate();
        }
    }

    private List<WidgetView> invalidFigures = new ArrayList<WidgetView>();
    private boolean updateQueued;

    private boolean updating;
    private boolean validating;
    private RunnableChain afterUpdate;

    private static class RunnableChain {
        RunnableChain next;
        Runnable run;

        RunnableChain(Runnable run, RunnableChain next) {
            this.run = run;
            this.next = next;
        }

        void run() {
            if (next != null)
                next.run();
            run.run();
        }
    }

    /** Empty constructor. */
    public DeferredUpdateManager() {
    }

    /** Adds the given figure to the update queue. Invalid figures will be validated before the damaged regions are
     * repainted.
     * 
     * @param f
     *            the invalid figure */
    public synchronized void addInvalidFigure(WidgetView f) {
        if (invalidFigures.contains(f))
            return;
        queueWork();
        invalidFigures.add(f);
    }

    /** Performs the update. Validates the invalid figures and then repaints the dirty regions.
     * 
     * @see #validateFigures()
     * @see #repairDamage() */
    public synchronized void performUpdate() {
        if (isDisposed() || updating)
            return;
        updating = true;
        try {
            performValidation();
            updateQueued = false;
            if (afterUpdate != null) {
                RunnableChain chain = afterUpdate;
                afterUpdate = null;
                chain.run(); // chain may queue additional Runnable.
                if (afterUpdate != null)
                    queueWork();
            }
        } finally {
            updating = false;
        }
    }

    /** @see UpdateManager#performValidation() */
    public void performValidation() {
        if (invalidFigures.isEmpty() || validating)
            return;
        try {
            WidgetView fig;
            validating = true;
            fireValidating();
            for (int i = 0; i < invalidFigures.size(); i++) {
                fig = (WidgetView) invalidFigures.get(i);
                invalidFigures.set(i, null);
                fig.validate();
            }
        } finally {
            invalidFigures.clear();
            validating = false;
        }
    }

    /** Posts an {@link UpdateRequest} using {@link Display#asyncExec(Runnable)}. If work has already been queued, a new
     * request is not needed. */
    protected void queueWork() {
        if (!updateQueued) {
            sendUpdateRequest();
            updateQueued = true;
        }
    }

    /** Fires the <code>UpdateRequest</code> to the current display asynchronously.
     * 
     * @since 3.2 */
    protected void sendUpdateRequest() {
//        Display display = Display.getCurrent();
//        if (display == null) {
//            throw new SWTException(SWT.ERROR_THREAD_INVALID_ACCESS);
//        }
//        display.asyncExec(new UpdateRequest());
    }

    /** Adds the given runnable and queues an update if an update is not under progress.
     * 
     * @param runnable
     *            the runnable */
    public synchronized void runWithUpdate(Runnable runnable) {
        afterUpdate = new RunnableChain(runnable, afterUpdate);
        if (!updating)
            queueWork();
    }

    /** Validates all invalid figures on the update queue and calls {@link UpdateManager#fireValidating()} unless there
     * are no invalid figures. */
    protected void validateFigures() {
        performValidation();
    }

}
