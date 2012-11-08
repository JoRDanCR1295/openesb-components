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
package com.sun.bpel.model.meta.impl;

import com.sun.bpel.model.impl.ValidateImpl;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RValidate;
import com.sun.bpel.model.meta.RVariable;
import com.sun.bpel.model.parser.impl.RBPELDocumentImpl;
import java.util.Set;

/**
 *
 * Validate Implementation
 * @author Vitaly Bychkov
 */
public class RValidateImpl extends ValidateImpl implements RValidate, RActivity {
    private long mUniqueID;
    private RActivity mNextAct;
    private Set<RVariable> mRVariables;

    /**
     * Creates a new instance of RValidateImpl
     *
     * @param bpeldoc runtime BPEL document
     * @param id unique ID
     */
    public RValidateImpl(RBPELDocumentImpl bpeldoc, long id) {
        super(bpeldoc);
        mUniqueID = id;
    }

    /**
     * gets next activity
     *
     * @return RActivity next activity
     */
    public RActivity getNextActivity() {
        return mNextAct;
    }

    /**
     * gets unique ID
     *
     * @return long unique ID
     */
    public long getUniqueId() {
        return mUniqueID;
    }

    /**
     * sets next activity
     *
     * @param act next activity
     */
    public void setNextActivity(RActivity act) {
        mNextAct = act;
    }

    /**
     * @see com.sun.bpel.model.meta.RValidate#getRVariables()
     */
    public Set<RVariable> getRVariables() {
        return mRVariables;
    }

    /**
     * @see com.sun.bpel.model.meta.RValidate#setRVariables(java.util.Set<RVariables>)
     */
    public void setRVariables(Set<RVariable> rVars) {
        mRVariables = rVars;
    }


}
