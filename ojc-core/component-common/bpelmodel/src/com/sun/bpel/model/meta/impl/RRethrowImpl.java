/* *************************************************************************
 *
 *          Copyright (c) 2002, Sun Microsystems, Inc.
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems, Inc.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems, Inc.
 *
 ***************************************************************************/
package com.sun.bpel.model.meta.impl;

import com.sun.bpel.model.impl.RethrowImpl;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.xml.common.model.XMLDocument;

/**
 * @author Sun Inc
 * Jul 31, 2007
 */
public class RRethrowImpl extends RethrowImpl implements RActivity {
    private long mUniqueID;

    /**
     * @param d
     * @param id
     */
    public RRethrowImpl(XMLDocument d, long id) {
        super(d);
        mUniqueID = id;
    }

    /** @see com.sun.bpel.model.meta.RActivity#getNextActivity()
     */
    public RActivity getNextActivity() {
        //There is no next activity for rethrow
        return null;
    }

    /** @see com.sun.bpel.model.meta.RActivity#setNextActivity(com.sun.bpel.model.meta.RActivity)
     */
    public void setNextActivity(RActivity act) {
        //There is no next activity for rethrow 
        // No-Op
    }

    /** @see com.sun.bpel.model.meta.Common#getUniqueId()
     */
    public long getUniqueId() {
        return mUniqueID;
    }

}
