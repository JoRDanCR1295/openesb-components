/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)StateManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
/******************************************************************************
 * Copyright © 2006 Sun Microsystems, Inc., 4150 Network Circle, Santa Clara,
 * California 95054, U.S.A. All rights reserved. Sun Microsystems, Inc. has
 * intellectual property rights relating to technology embodied in the product
 * that is described in this document. In particular, and without limitation,
 * these intellectual property rights may include one or more of the U.S. patents
 * listed at http://www.sun.com/patents and one or more additional patents or
 * pending patent applications in the U.S. and in other countries. THIS PRODUCT
 * CONTAINS CONFIDENTIAL INFORMATION AND TRADE SECRETS OF SUN MICROSYSTEMS, INC.
 * USE, DISCLOSURE OR REPRODUCTION IS PROHIBITED WITHOUT THE PRIOR EXPRESS WRITTEN
 * PERMISSION OF SUN MICROSYSTEMS, INC.U.S. Government Rights - Commercial
 * software.  Government users are subject to the Sun Microsystems, Inc. standard
 * license agreement and applicable provisions of the FAR and its supplements.
 * Use is subject to license terms.  This distribution may include materials
 * developed by third parties. Sun, Sun Microsystems, the Sun logo, Java
 * Composite Application Platform Suite,  SeeBeyond, eGate, eInsight, eVision, eTL,
 * eXchange, eView, eIndex, eBAM and  eWay are trademarks or registered trademarks of
 * Sun Microsystems, Inc. in the U.S. and other countries. All SPARC trademarks are
 * used under license and are trademarks or registered trademarks of SPARC
 * International, Inc. in the U.S. and other countries. Products bearing SPARC
 * trademarks are based upon architecture developed by Sun Microsystems, Inc.
 * UNIX is a registered trademark in the U.S. and other countries, exclusively
 * licensed through X/Open Company, Ltd. This product is covered and controlled by
 * U.S. Export Control laws and may be subject to the export or import laws in
 * other countries.  Nuclear, missile, chemical biological weapons or nuclear
 * maritime end uses or end users, whether direct or indirect, are strictly
 * prohibited.  Export or reexport to countries subject to U.S. embargo or to
 * entities identified on U.S. export exclusion lists, including, but not limited
 * to, the denied persons and specially designated nationals lists is strictly
 * prohibited.
 **/
package com.sun.jbi.ftpbc.ftp.statemanager;

import java.io.Serializable;

/**
 * State manager for managing state persistence in non-transactional mode.
 * For use in non-transactional mode.
 */
public class StateManager {

    protected Serializable state = null;
    protected StatePersistenceAdapter storeAdapter = null;

    /**
     * Constructs a non-transactional state manager.
     *
     */
    protected StateManager() {
    }

    /**
     * Constructs a non-transactional state manager with a non-transactional
     * state persistence adapter.
     *
     * @param       storeAdapter    A non-transactional state persistent store adapter.
     *
     */
    public StateManager(StatePersistenceAdapter storeAdapter) {
        this.storeAdapter = storeAdapter;
    }

    /**
     * Close the StateManager. Once closed the StateManager
     * can not be used again.
     *
     * @throws      StatePersistenceException upon error.
     */
    public void close() throws StatePersistenceException {
        this.storeAdapter.close();
    }

    /**
     * Sets the Serializable state object. The state is set in memory
     * only. Note that calling load after calling setState may replace
     * the in memory state object with the stored state object.
     *
     * @param       state    A Serializable state object.
     *
     * @see         #getState
     *
     */
    public void setState(Serializable state) {
        this.state = state;
    }

    /**
     * Gets the Serializable state object. Gets the in memory state object.
     *
     * @returns       A Serializable state object previously set or
     *                null if not previously set.
     *
     * @see           #setState
     */
    public Serializable getState() {
        return state;
    }

    /**
     * Stores the Serializable state object using the registered
     * persistent store adapter.
     *
     * @see           #setState
     * @see           #load
     *
     * @throws        StateManagerException upon error.
     */
    public void store() throws StateManagerException {
        if (state != null) {
            try {
                storeAdapter.save(state);
            } catch (StatePersistenceException ex) {
                throw new StateManagerException("StateManager - Failed to store state object. " + ex.toString(), ex);
            }
        } else {
            throw new StateManagerException("StateManager - A Serializable state object was not set. Nothing to store.");
        }
    }

    /**
     * Loads the Serializable state object, using the registered
     * persistent store adapter, previously set using store.  Calling
     * this method will place the stored state object into memory.
     *
     * @see           #store
     *
     * @throws        StateManagerException upon error.
     */
    public void load() throws StateManagerException {
        state = null;
        try {
            state = storeAdapter.restore();
        } catch (StatePersistenceException ex) {
            throw new StateManagerException("StateManager - Failed to load state object. " + ex.toString(), ex);
        }
    }
}
