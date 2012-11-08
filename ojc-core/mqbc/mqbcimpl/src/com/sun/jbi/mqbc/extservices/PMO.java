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
 * @(#)PMO.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.extservices;


import java.util.logging.Level;
import java.util.logging.Logger;

import com.ibm.mq.MQPutMessageOptions;
import com.sun.jbi.internationalization.Messages;

public class PMO
        extends MQPutMessageOptions {

    /**
     * Create a PMO object with no options set.
     * 
     * @see com.ibm.mq.MQPutMessageOptions#MQPutMessageOptions()
     */
    public PMO() {
        super();

        optionsClearAll();

        m_Logging.log(Level.FINER,
                mMessages.getString("DEBUG_NEW_OBJECT", PMO.class.getName()));
    }

    /**
     * Clear all option flags set so far and reset options to MQPMO_NONE.
     * 
     * @see com.ibm.mq.MQPutMessageOptions#options
     */
    public void optionsClearAll() {
        options = com.ibm.mq.MQC.MQPMO_NONE;
        m_Logging.log(Level.INFO, "DEBUG_MGC_PMO_NONE");
    }

    /**
     * Set or unset the MGC.MQPMO_SYNCPOINT option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQPutMessageOptions#options
     */
    public void setMQPMO_SYNCPOINT(boolean bSet) {
        if (bSet) {
            options |= com.ibm.mq.MQC.MQPMO_SYNCPOINT;
            options &= ~com.ibm.mq.MQC.MQPMO_NO_SYNCPOINT;
            m_Logging.log(Level.INFO, "DEBUG_MGC_PMO_SYNCPOINT_SET");
        } else {
            options &= ~com.ibm.mq.MQC.MQPMO_SYNCPOINT;
            options |= com.ibm.mq.MQC.MQPMO_NO_SYNCPOINT;
            m_Logging.log(Level.INFO, "DEBUG_MGC_PMO_SYNCPOINT_UNSET");
        }
    }


    /**
     * Set or unset the MGC.MQPMO_NO_CONTEXT option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQPutMessageOptions#options
     */
    public void setMQPMO_NO_CONTEXT(boolean bSet) {
        if (bSet) {
            options |= com.ibm.mq.MQC.MQPMO_NO_CONTEXT;
            m_Logging.log(Level.INFO, "DEBUG_MGC_PMO_NOCONTEXT_SET");
        } else {
            options &= ~com.ibm.mq.MQC.MQPMO_NO_CONTEXT;
            m_Logging.log(Level.INFO, "DEBUG_MGC_PMO_NOCONTEXT_UNSET");
        }
    }

    /**
     * Set or unset the MGC.MQPMO_DEFAULT_CONTEXT option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQPutMessageOptions#options
     */
    public void setMQPMO_DEFAULT_CONTEXT(boolean bSet) {
        if (bSet) {
            options |= com.ibm.mq.MQC.MQPMO_DEFAULT_CONTEXT;
            m_Logging.log(Level.INFO, "DEBUG_MGC_PMO_DEFAULTCONTEXT_SET");
        } else {
            options &= ~com.ibm.mq.MQC.MQPMO_DEFAULT_CONTEXT;
            m_Logging.log(Level.INFO, "DEBUG_MGC_PMO_DEFAULTCONTEXT_UNSET");
        }
    }

    /**
     * Set or unset the MGC.MQPMO_SET_IDENTITY_CONTEXT option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQPutMessageOptions#options
     */
    public void setMQPMO_SET_IDENTITY_CONTEXT(boolean bSet) {
        if (bSet) {
            options |= com.ibm.mq.MQC.MQPMO_SET_IDENTITY_CONTEXT;
            m_Logging.log(Level.INFO,
                          "DEBUG_MGC_PMO_SETIDENTITYCONTEXT_SET");
        } else {
            options &= ~com.ibm.mq.MQC.MQPMO_SET_IDENTITY_CONTEXT;
            m_Logging.log(Level.INFO,
                          "DEBUG_MGC_PMO_SETIDENTITYCONTEXT_UNSET");
        }
    }

    /**
     * Set or unset the MGC.MQPMO_SET_ALL_CONTEXT option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQPutMessageOptions#options
     */
    public void setMQPMO_SET_ALL_CONTEXT(boolean bSet) {
        if (bSet) {
            options |= com.ibm.mq.MQC.MQPMO_SET_ALL_CONTEXT;
            m_Logging.log(Level.INFO, "DEBUG_MGC_PMO_SETALLCONTEXT_SET");
        } else {
            options &= ~com.ibm.mq.MQC.MQPMO_SET_ALL_CONTEXT;
            m_Logging.log(Level.INFO, "DEBUG_MGC_PMO_SETALLCONTEXT_UNSET");
        }
    }

    /**
     * Set or unset the MGC.MQPMO_FAIL_IF_QUIESCING option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQPutMessageOptions#options
     */
    public void setMQPMO_FAIL_IF_QUIESCING(boolean bSet) {
        if (bSet) {
            options |= com.ibm.mq.MQC.MQPMO_FAIL_IF_QUIESCING;
            m_Logging.log(Level.INFO, "DEBUG_MGC_PMO_FAILIFQUIESCING_SET");
        } else {
            options &= ~com.ibm.mq.MQC.MQPMO_FAIL_IF_QUIESCING;
            m_Logging.log(Level.INFO,
                          "DEBUG_MGC_PMO_FAILIFQUIESCING_UNSET");
        }
    }

    /**
     * Set or unset the MGC.MQPMO_NEW_MSG_ID option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQPutMessageOptions#options
     */
    public void setMQPMO_NEW_MSG_ID(boolean bSet) {
        if (bSet) {
            options |= com.ibm.mq.MQC.MQPMO_NEW_MSG_ID;
            m_Logging.log(Level.INFO, "DEBUG_MGC_PMO_NEWMSGID_SET");
        } else {
            options &= ~com.ibm.mq.MQC.MQPMO_NEW_MSG_ID;
            m_Logging.log(Level.INFO, "DEBUG_MGC_PMO_NEWMSGID_UNSET");
        }
    }

    /**
     * Set or unset the MGC.MQPMO_NEW_CORREL_ID option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQPutMessageOptions#options
     */
    public void setMQPMO_NEW_CORREL_ID(boolean bSet) {
        if (bSet) {
            options |= com.ibm.mq.MQC.MQPMO_NEW_CORREL_ID;
            m_Logging.log(Level.INFO, "DEBUG_MGC_PMO_NEWCORRELID_SET");
        } else {
            options &= ~com.ibm.mq.MQC.MQPMO_NEW_CORREL_ID;
            m_Logging.log(Level.INFO, "DEBUG_MGC_PMO_NEWCORRELID_UNSET");
        }
    }

    /**
     * Set or unset the MGC.MQPMO_LOGICAL_ORDER option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQPutMessageOptions#options
     */
    public void setMQPMO_LOGICAL_ORDER(boolean bSet) {
        if (bSet) {
            options |= com.ibm.mq.MQC.MQPMO_LOGICAL_ORDER;
            m_Logging.log(Level.INFO, "DEBUG_MGC_PMO_LOGICALORDER_SET");
        } else {
            options &= ~com.ibm.mq.MQC.MQPMO_LOGICAL_ORDER;
            m_Logging.log(Level.INFO, "DEBUG_MGC_PMO_LOGICALORDER_UNSET");
        }
    }

    /**
     * Set or unset the MGC.MQPMO_NONE option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQPutMessageOptions#options
     */
    public void setMQPMO_NONE(boolean bSet) {
        if (bSet) {
            options |= com.ibm.mq.MQC.MQPMO_NONE;
            m_Logging.log(Level.INFO, "DEBUG_MGC_PMO_NONE_SET");
        } else {
            options &= ~com.ibm.mq.MQC.MQPMO_NONE;
            m_Logging.log(Level.INFO, "DEBUG_MGC_PMO_NONE_UNSET");
        }
    }

    /**
     * Set or unset the MGC.MQPMO_PASS_IDENTITY_CONTEXT option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQPutMessageOptions#options
     */
    public void setMQPMO_PASS_IDENTITY_CONTEXT(boolean bSet) {
        if (bSet) {
            options |= com.ibm.mq.MQC.MQPMO_PASS_IDENTITY_CONTEXT;
            m_Logging.log(Level.INFO,
                          "DEBUG_MGC_PMO_PASSIDENTITYCONTEXT_SET");
        } else {
            options &= ~com.ibm.mq.MQC.MQPMO_PASS_IDENTITY_CONTEXT;
            m_Logging.log(Level.INFO,
                          "DEBUG_MGC_PMO_PASSIDENTITYCONTEXT_UNSET");
        }
    }

    /**
     * Set or unset the MGC.MQPMO_PASS_ALL_CONTEXT option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQPutMessageOptions#options
     */
    public void setMQPMO_PASS_ALL_CONTEXT(boolean bSet) {
        if (bSet) {
            options |= com.ibm.mq.MQC.MQPMO_PASS_ALL_CONTEXT;
            m_Logging.log(Level.INFO, "DEBUG_MGC_PMO_PASSALLCONTEXT_SET");
        } else {
            options &= ~com.ibm.mq.MQC.MQPMO_PASS_ALL_CONTEXT;
            m_Logging.log(Level.INFO, "DEBUG_MGC_PMO_PASSALLCONTEXT_UNSET");
        }
    }

    /**
     * Event m_Logger
     */
     private static final Messages mMessages = Messages.getMessages( PMO.class );
     private static Logger m_Logging = Messages.getLogger( PMO.class );
     
}
