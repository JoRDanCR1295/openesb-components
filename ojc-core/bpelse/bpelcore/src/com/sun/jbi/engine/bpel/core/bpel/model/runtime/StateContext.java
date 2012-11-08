package com.sun.jbi.engine.bpel.core.bpel.model.runtime;

import com.sun.jbi.engine.bpel.core.bpel.persist.MutableState;

/** Primarily used for persistent state context
 * @author Sun Inc
 * Aug 28, 2007
 */
public interface StateContext {
    /**
     * @return String, a unique Id that is either an instanceId or an eventhandler execution id.
     */
    MutableState getState();
}
