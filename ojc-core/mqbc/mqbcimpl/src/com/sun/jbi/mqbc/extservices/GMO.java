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
 * @(#)GMO.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.extservices;

/**
 */

import java.util.logging.Level;
import java.util.logging.Logger;

import com.ibm.mq.MQGetMessageOptions;
import com.sun.jbi.internationalization.Messages;


/**
 * This class subclasses {@link com.ibm.mq.MQGetMessageOptions} to provide
 * mutators to MQGetMessageOption's public fields, so that they can be
 * manipulated in the eGate Java Collaboration Editor (which does not permit
 * direct access to public fields). This class is exposed as part of the eWay
 * OTD(s).
 * 
 * <p>Also, unlike MQGetMessageOptions, a new GMO object's options defaults to
 * {@link com.ibm.mq.MQC#MQGMO_NONE MQGMO_NONE}, not {@link
 * com.ibm.mq.MQC#MQGMO_NO_WAIT MQGMO_NO_WAIT}.</p>
 * 
 */
public class GMO
        extends MQGetMessageOptions
         {
     private static final Messages mMessages =
        Messages.getMessages(GMO.class);
    private static final Logger m_Logging =
        Messages.getLogger(GMO.class);

    /**
     * Create a GMO object.
     * 
     * @see com.ibm.mq.MQGetMessageOptions#MQGetMessageOptions()
     */
    public GMO() {
        super();
        
        // Defaults
        optionsClearAll();
        matchOptionsClearAll();

        m_Logging.log(Level.FINER, mMessages.getString("DEBUG_NEW_OBJECT",
                GMO.class.getName()));
    }
    
             /**
     * Affects the {@link com.ibm.mq.MQGetMessageOptions#waitInterval} variable.
     * The maximum time (in milliseconds) that an MQQueue.get call waits for a
     * suitable message to arrive (used in conjunction with MQC.MQGMO_WAIT). A
     * value of MQC.MQWI_UNLIMITED indicates that an unlimited wait is
     * required.
     * 
     * @param ms # of milliseconds for a get call to wait.
     * 
     * @see com.ibm.mq.MQGetMessageOptions#waitInterval
     */
    public void setWaitValue(int ms) {
        waitInterval = ms;
        m_Logging.log(Level.FINE,
                      "GMO_DEBUG_MQC_WAITINTERVAL",
                      String.valueOf(ms));
    }

    /**
     * Retrieve the value of the {@link com.ibm.mq.MQGetMessageOptions#waitInterval}
     * variable.
     */ 
    public int getWaitValue() {
        return waitInterval;
    }
    
    /**
     * Specify an unlimited time for a get call to wait for a suitable message
     * to arrive.
     * 
     * @see com.ibm.mq.MQGetMessageOptions#waitInterval
     */
    public void setUnlimitedWait() {
        waitInterval = com.ibm.mq.MQC.MQWI_UNLIMITED;
        m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_WAITIINTERVAL_UNLIMITED");
    }

    /**
     * Clear all option flags set so far and reset options to MQGMO_NONE.
     * 
     * @see com.ibm.mq.MQGetMessageOptions#options
     */
    public void optionsClearAll() {
        options = com.ibm.mq.MQC.MQGMO_NONE;
        m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_GMO_NONE");
    }

    /**
     * Clear all match options set so far and set match options to MQMO_NONE.
     * 
     * @see com.ibm.mq.MQGetMessageOptions#matchOptions
     */
    public void matchOptionsClearAll() {
        matchOptions = com.ibm.mq.MQC.MQMO_NONE;
        m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_MO_NONE");
    }

    /**
     * Set or unset the MGC.MQGMO_NONE option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQGetMessageOptions#options
     */
    public void setMQGMO_NONE(boolean bSet) {
        if (bSet) {
            options |= com.ibm.mq.MQC.MQGMO_NONE;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_GMO_NONE_SET");
        } else {
            options &= ~com.ibm.mq.MQC.MQGMO_NONE;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_GMO_NONE_UNSET");
        }
    }

    /**
     * Set or unset the MGC.MQGMO_WAIT option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQGetMessageOptions#options
     */
    public void setMQGMO_WAIT(boolean bSet) {
        if (bSet) {
            options |= com.ibm.mq.MQC.MQGMO_WAIT;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_GMO_WAIT_SET");
        } else {
            options &= ~com.ibm.mq.MQC.MQGMO_WAIT;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_GMO_WAIT_UNSET");
        }
    }

    /**
     * Set or unset the MGC.MQGMO_NO_WAIT option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQGetMessageOptions#options
     */
    public void setMQGMO_NO_WAIT(boolean bSet) {
        if (bSet) {
            options |= com.ibm.mq.MQC.MQGMO_NO_WAIT;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_GMO_NOWAIT_SET");
        } else {
            options &= ~com.ibm.mq.MQC.MQGMO_NO_WAIT;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_GMO_NOWAIT_UNSET");
        }
    }

    /**
     * Set or unset the MGC.MQGMO_SYNCPOINT option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQGetMessageOptions#options
     */
    public void setMQGMO_SYNCPOINT(boolean bSet) {
        if (bSet) {
            options |= com.ibm.mq.MQC.MQGMO_SYNCPOINT;
            options &= ~com.ibm.mq.MQC.MQGMO_NO_SYNCPOINT;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_GMO_SYNCPOINT_SET");
        } else {
            options &= ~com.ibm.mq.MQC.MQGMO_SYNCPOINT;
            options |= com.ibm.mq.MQC.MQGMO_NO_SYNCPOINT;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_GMO_SYNCPOINT_UNSET");
        }
    }

    

    /**
     * Set or unset the MGC.MQGMO_BROWSE_FIRST option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQGetMessageOptions#options
     */
    public void setMQGMO_BROWSE_FIRST(boolean bSet) {
        if (bSet) {
            options |= com.ibm.mq.MQC.MQGMO_BROWSE_FIRST;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_GMO_BROWSEFIRST_SET");
        } else {
            options &= ~com.ibm.mq.MQC.MQGMO_BROWSE_FIRST;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_GMO_BROWSEFIRST_UNSET");
        }
    }

    /**
     * Set or unset the MGC.MQGMO_BROWSE_NEXT option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQGetMessageOptions#options
     */
    public void setMQGMO_BROWSE_NEXT(boolean bSet) {
        if (bSet) {
            options |= com.ibm.mq.MQC.MQGMO_BROWSE_NEXT;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_GMO_BROWSENEXT_SET");
        } else {
            options &= ~com.ibm.mq.MQC.MQGMO_BROWSE_NEXT;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_GMO_BROWSENEXT_UNSET");
        }
    }

    /**
     * Set or unset the MGC.MQGMO_BROWSE_MSG_UNDER_CURSOR option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQGetMessageOptions#options
     */
    public void setMQGMO_BROWSE_MSG_UNDER_CURSOR(boolean bSet) {
        if (bSet) {
            options |= com.ibm.mq.MQC.MQGMO_BROWSE_MSG_UNDER_CURSOR;
            m_Logging.log(Level.FINE,
                          "GMO_DEBUG_MGC_GMO_BROWSEMSGUNDERCURSOR_SET");
        } else {
            options &= ~com.ibm.mq.MQC.MQGMO_BROWSE_MSG_UNDER_CURSOR;
            m_Logging.log(Level.FINE,
                          "GMO_DEBUG_MGC_GMO_BROWSEMSGUNDERCURSOR_UNSET");
        }
    }

    /**
     * Set or unset the MGC.MQGMO_LOCK option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQGetMessageOptions#options
     */
    public void setMQGMO_LOCK(boolean bSet) {
        if (bSet) {
            options |= com.ibm.mq.MQC.MQGMO_LOCK;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_GMO_LOCK_SET");
        } else {
            options &= ~com.ibm.mq.MQC.MQGMO_LOCK;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_GMO_LOCK_UNSET");
        }
    }

    /**
     * Set or unset the MGC.MQGMO_UNLOCK option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQGetMessageOptions#options
     */
    public void setMQGMO_UNLOCK(boolean bSet) {
        if (bSet) {
            options |= com.ibm.mq.MQC.MQGMO_UNLOCK;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_GMO_UNLOCK_SET");
        } else {
            options &= ~com.ibm.mq.MQC.MQGMO_UNLOCK;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_GMO_UNLOCK_UNSET");
        }
    }

    /**
     * Set or unset the MGC.MQGMO_ACCEPT_TRUNCATED_MSG option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQGetMessageOptions#options
     */
    public void setMQGMO_ACCEPT_TRUNCATED_MSG(boolean bSet) {
        if (bSet) {
            options |= com.ibm.mq.MQC.MQGMO_ACCEPT_TRUNCATED_MSG;
            m_Logging.log(Level.FINE,
                          "GMO_DEBUG_MGC_GMO_ACCEPTTRUNCATEDMSG_SET");
        } else {
            options &= ~com.ibm.mq.MQC.MQGMO_ACCEPT_TRUNCATED_MSG;
            m_Logging.log(Level.FINE,
                          "GMO_DEBUG_MGC_GMO_ACCEPTTRUNCATEDMSG_UNSET");
        }
    }

    /**
     * Set or unset the MGC.MQGMO_FAIL_IF_QUIESCING option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQGetMessageOptions#options
     */
    public void setMQGMO_FAIL_IF_QUIESCING(boolean bSet) {
        if (bSet) {
            options |= com.ibm.mq.MQC.MQGMO_FAIL_IF_QUIESCING;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_GMO_FAILIFQUIESCING_SET");
        } else {
            options &= ~com.ibm.mq.MQC.MQGMO_FAIL_IF_QUIESCING;
            m_Logging.log(Level.FINE,
                          "GMO_DEBUG_MGC_GMO_FAILIFQUIESCING_UNSET");
        }
    }

    /**
     * Set or unset the MGC.MQGMO_CONVERT option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQGetMessageOptions#options
     */
    public void setMQGMO_CONVERT(boolean bSet) {
        if (bSet) {
            options |= com.ibm.mq.MQC.MQGMO_CONVERT;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_GMO_CONVERT_SET");
        } else {
            options &= ~com.ibm.mq.MQC.MQGMO_CONVERT;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_GMO_CONVERT_UNSET");
        }
    }

    /**
     * Set or unset the MGC.MQGMO_SYNCPOINT_IF_PERSISTENT option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQGetMessageOptions#options
     */
    public void setMQGMO_SYNCPOINT_IF_PERSISTENT(boolean bSet) {
        if (bSet) {
            options |= com.ibm.mq.MQC.MQGMO_SYNCPOINT_IF_PERSISTENT;
            m_Logging.log(Level.FINE,
                          "GMO_DEBUG_MGC_GMO_SYNCPOINTIFPERSISTENT_SET");
        } else {
            options &= ~com.ibm.mq.MQC.MQGMO_SYNCPOINT_IF_PERSISTENT;
            m_Logging.log(Level.FINE,
                          "GMO_DEBUG_MGC_GMO_SYNCPOINTIFPERSISTENT_UNSET");
        }
    }

    /**
     * Set or unset the MGC.MQGMO_MARK_SKIP_BACKOUT option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQGetMessageOptions#options
     */
    public void setMQGMO_MARK_SKIP_BACKOUT(boolean bSet) {
        if (bSet) {
            options |= com.ibm.mq.MQC.MQGMO_MARK_SKIP_BACKOUT;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_GMO_MARKSKIPBACKOUT_SET");
        } else {
            options &= ~com.ibm.mq.MQC.MQGMO_MARK_SKIP_BACKOUT;
            m_Logging.log(Level.FINE,
                          "GMO_DEBUG_MGC_GMO_MARKSKIPBACKOUT_UNSET");
        }
    }

    /**
     * Set or unset the MGC.MQGMO_MSG_UNDER_CURSOR option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQGetMessageOptions#options
     */
    public void setMQGMO_MSG_UNDER_CURSOR(boolean bSet) {
        if (bSet) {
            options |= com.ibm.mq.MQC.MQGMO_MSG_UNDER_CURSOR;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_GMO_MSGUNDERCURSOR_SET");
        } else {
            options &= ~com.ibm.mq.MQC.MQGMO_MSG_UNDER_CURSOR;
            m_Logging.log(Level.FINE,
                          "GMO_DEBUG_MGC_GMO_MSGUNDERCURSOR_UNSET");
        }
    }
    
    /**
     * Set or unset the MGC.MQGMO_LOGICAL_ORDER option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQGetMessageOptions#options
     */
    public void setMQGMO_LOGICAL_ORDER(boolean bSet) {
        if (bSet) {
            options |= com.ibm.mq.MQC.MQGMO_LOGICAL_ORDER;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_GMO_LOGICALORDER_SET");
        } else {
            options &= ~com.ibm.mq.MQC.MQGMO_LOGICAL_ORDER;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_GMO_LOGICALORDER_UNSET");
        }
    }

    /**
     * Set or unset the MGC.MQGMO_COMPLETE_MSG option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQGetMessageOptions#options
     */
    public void setMQGMO_COMPLETE_MSG(boolean bSet) {
        if (bSet) {
            options |= com.ibm.mq.MQC.MQGMO_COMPLETE_MSG;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_GMO_COMPLETEMSG_SET");
        } else {
            options &= ~com.ibm.mq.MQC.MQGMO_COMPLETE_MSG;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_GMO_COMPLETEMSG_UNSET");
        }
    }

    /**
     * Set or unset the MGC.MQGMO_ALL_MSGS_AVAILABLE option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQGetMessageOptions#options
     */
    public void setMQGMO_ALL_MSGS_AVAILABLE(boolean bSet) {
        if (bSet) {
            options |= com.ibm.mq.MQC.MQGMO_ALL_MSGS_AVAILABLE;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_GMO_ALLMSGSAVAILABLE_SET");
        } else {
            options &= ~com.ibm.mq.MQC.MQGMO_ALL_MSGS_AVAILABLE;
            m_Logging.log(Level.FINE,
                          "GMO_DEBUG_MGC_GMO_ALLMSGSAVAILABLE_UNSET");
        }
    }

    /**
     * Set or unset the MGC.MQGMO_ALL_SEGMENTS_AVAILABLE option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQGetMessageOptions#options
     */
    public void setMQGMO_ALL_SEGMENTS_AVAILABLE(boolean bSet) {
        if (bSet) {
            options |= com.ibm.mq.MQC.MQGMO_ALL_SEGMENTS_AVAILABLE;
            m_Logging.log(Level.FINE,
                          "GMO_DEBUG_MGC_GMO_ALLSEGMENTSAVAILABLE_SET");
        } else {
            options &= ~com.ibm.mq.MQC.MQGMO_ALL_SEGMENTS_AVAILABLE;
            m_Logging.log(Level.FINE,
                          "GMO_DEBUG_MGC_GMO_ALLSEGMENTSAVAILABLE_UNSET");
        }
    }

    /**
     * Set or unset the MGC.MQMO_MATCH_MSG_ID option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQGetMessageOptions#matchOptions
     */
    public void setMQMO_MATCH_MSG_ID(boolean bSet) {
        if (bSet) {
            matchOptions |= com.ibm.mq.MQC.MQMO_MATCH_MSG_ID;
            m_Logging.log(Level.FINE,"GMO_DEBUG_MGC_MO_MATCHMSGID_SET");
        } else {
            matchOptions &= ~com.ibm.mq.MQC.MQMO_MATCH_MSG_ID;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_MO_MATCHMSGID_UNSET");
        }
    }

    /**
     * Set or unset the MGC.MQMO_MATCH_CORREL_ID option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQGetMessageOptions#matchOptions
     */
    public void setMQMO_MATCH_CORREL_ID(boolean bSet) {
        if (bSet) {
            matchOptions |= com.ibm.mq.MQC.MQMO_MATCH_CORREL_ID;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_MO_MATCHCORRELID_SET");
        } else {
            matchOptions &= ~com.ibm.mq.MQC.MQMO_MATCH_CORREL_ID;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_MO_MATCHCORRELID_UNSET");
        }
    }

    /**
     * Set or unset the MGC.MQMO_MATCH_GROUP_ID option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQGetMessageOptions#matchOptions
     */
    public void setMQMO_MATCH_GROUP_ID(boolean bSet) {
        if (bSet) {
            matchOptions |= com.ibm.mq.MQC.MQMO_MATCH_GROUP_ID;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_MO_MATCHGROUPID_SET");
        } else {
            matchOptions &= ~com.ibm.mq.MQC.MQMO_MATCH_GROUP_ID;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_MO_MATCHGROUPID_UNSET");
        }
    }

    /**
     * Set or unset the MGC.MQMO_MATCH_MSG_SEQ_NUMBER option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQGetMessageOptions#matchOptions
     */
    public void setMQMO_MATCH_MSG_SEQ_NUMBER(boolean bSet) {
        if (bSet) {
            matchOptions |= com.ibm.mq.MQC.MQMO_MATCH_MSG_SEQ_NUMBER;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_MO_MATCHMSGSEQNUMBER_SET");
        } else {
            matchOptions &= ~com.ibm.mq.MQC.MQMO_MATCH_MSG_SEQ_NUMBER;
            m_Logging.log(Level.FINE,
                          "GMO_DEBUG_MGC_MO_MATCHMSGSEQNUMBER_UNSET");
        }
    }

    /**
     * Set or unset the MGC.MQMO_NONE option.
     * 
     * @param bSet <code>true</code> to set, <code>false</code> to unset.
     * 
     * @see com.ibm.mq.MQGetMessageOptions#matchOptions
     */
    public void setMQMO_NONE(boolean bSet) {
        if (bSet) {
            matchOptions |= com.ibm.mq.MQC.MQMO_NONE;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_MO_NONE_SET");
        } else {
            matchOptions &= ~com.ibm.mq.MQC.MQMO_NONE;
            m_Logging.log(Level.FINE, "GMO_DEBUG_MGC_MO_NONE_UNSET");
        }
    }

    /**
     * Provide access to the content of the output field
     * {@link com.ibm.mq.MQGetMessageOptions#resolvedQueueName}.
     */ 
    public String getResolvedQueueName() {
        return resolvedQueueName;
    }
    
    /**
     * Provides access to the content of the output field
     * {@link com.ibm.mq.MQGetMessageOptions#segmentation}.
     */ 
    public char getSegmentation() {
        return segmentation;
    }
    
    /**
     * Provides access to the content of the output field
     * {@link com.ibm.mq.MQGetMessageOptions#segmentation}.
     */ 
    public char getSegmentationStatus() {
        return segmentStatus;
    }
    
    /**
     * Determine whether or not the output field {@link com.ibm.mq.MQGetMessageOptions#groupStatus}
     * specifies the retrieved message is in a group.
     * 
     * @see com.ibm.mq.MQC.MQGS_NOT_IN_GROUP
     */
    public boolean isMessageInGroup() {
        return groupStatus != com.ibm.mq.MQC.MQGS_NOT_IN_GROUP;
    }
    
    /**
     * Determine whether or not the output field {@link com.ibm.mq.MQGetMessageOptions#groupStatus)
     * indicates that the retrieved message is in a group <strong>and</strong>
     * it is the last message in the group.
     * 
     * @return <code>true</code> if the message is in the group, and it is the
     *         last message in the group; <code>false</code> indicates the
     *         message is in the group, but is not the last message in the
     *         group.
     */ 
    public boolean isMessageLastInGroup() {
        return isMessageInGroup()
                && groupStatus == com.ibm.mq.MQC.MQGS_LAST_MSG_IN_GROUP;
    }
    
   
}
