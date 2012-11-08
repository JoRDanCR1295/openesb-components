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
package com.sun.jbi.engine.bpel.core.bpel.persist;

import java.sql.Timestamp;
import java.util.Collection;
import java.util.Set;

import javax.xml.namespace.QName;

import com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext;
import com.sun.jbi.engine.bpel.core.bpel.persist.impl.StatePartnerLinkWrapper;
import com.sun.jbi.engine.bpel.core.bpel.persist.impl.StateVariableWrapper;

/**
 * @author Sun Inc
 * Aug 29, 2007
 */
public interface State {

    interface CRMPState {
    
        /**
         * @return Returns the mBPELME.
         */
        String getBPELME();
    
        /**
         * @return Returns the mCrmpInvokeId.
         */
        String getCrmpInvokeId();
    
        /**
         * @return Returns the mOper.
         */
        String getOper();
    
        /**
         * @return Returns the mPLink.
         */
        String getPLink();
    
        /**
         * @return Returns the mReplyVarId.
         */
        long getReplyVarId();
    
        /**
         * @return Returns the mRespObj.
         */
        char[] getRespObj();
        
        /**
         * @return flag to figure out if it is a reply based update to CRMP
         */
        boolean isUpdateForReply();
    }
    
    interface EventState extends State {
        String getAncestorScopeId();
        long getEventModelId();
        /** 
         * @return returns the string identified the instance state in the DB. 
         * (Id found in the state table)
         */
        String getBPStateId();
        /**
         * Not using the getTimer() API on the State, to avoid confusion when the EveHdlr's OnAlarm
         * is followed by a wait (or some other timer activity).
         * @return 
         */
        Timestamp getOnEventTimerValue();
        
        /** we want to support null values here, hence the return type is a Long object
         * @return
         */
        Long getRepeatEveryVal();
        
        /** check to see if the event handler has been marked as updated in the DB.
         * @return
         */
        boolean isUpdatedInDB();
    } 
    
    enum Mode {
    	ACTIVATE,
    	DELETE,
    	INSERT,
    	PASSIVATE,
       UPDATE,
       SUSPEND,
       RESUME
   };

    /**
     * get engine ID
     * @return String engine ID
     */
    String getEngineId();
    /**
     * get state ID
     * @return String state ID
     */
    String getId();
    /** Called by state manager right after the persistState is successfully executed.
     * marks the state as already inserted
     */
    void markInserted();
    /** determines the mode of the state (this) object.
     * @return int update mode
     */
    Mode getUpdateMode();
    
    /**
     * @return returns an array of two values. val[0] contains the
     * old PC and val[1] contains the new PC. It signifies that the val[0] has been
     * modified to val[1]. If val[0] is null, then val[1] is a new entry of the
     * persist state. It signifies that val[1] is not a modification to any of the
     * existing PCs. for instance, val[0] will and should be null, when the state's
     * getUpdateMode() API returns MutableState.INSERT_MODE
     * val[0] will also be null during the start of flow branches.
     *
     */
    Long[] getPC();
    /**
     * @return returns a collection of strings that ae no longer valid. This happens
     * when a flow activity is complete, all the PCs associated with that flow's
     * branches are no longer valid.
     */
    Long[] getInvalidPCs();
    /**
     * @return either a java.sql.Timestamp representing the duration
     * for which the state is expected to be idle.
     */
    Timestamp getTimerValue();
    
    /**
     * @return the value of the branch invoke counter that is used for custom 
     * reliable messaging for the InOut message exchange. 
     */
    Long getBranchInvokeCounter();

    /**
     * Fetches collection of foreach 
     * @return collection of foreach
     */
    Collection getForEach();
    /**
     * @return OnMessage or OnAlarm Id if present or null. It will return null
     * if the current getPC value is not an Id corresponding to a Pick activity.
     */
    Long getPickCompositeActId();
    /**
     * @return returns the Set of correlations. Set<CorrelationVal>  
     */
    Set getCorrelations();
    /**
     * get BPEL ID
     * @return QName BPEL ID
     */
    QName getBPELId();
    
    /**
     *  getvariables to persist
     * @param for_update flag that determines whether to return the
     *  variables that need to be inserted or the variables that need to be updated.
     *  true for update, false for insert.
     * @return Collection<StateVariableWrapper>
     */
    Collection<StateVariableWrapper> getVariables(boolean for_update);
    
    Collection<FaultHandlingContext> getScopeStates(boolean for_update);
    
    /**
     *  getvariables to persist
     * @param for_update flag that determines whether to return the
     *  partnerlinks that need to be inserted or the partnerlinks that need to be updated.
     *  true for update, false for insert.
     * @return
     */
    Collection<StatePartnerLinkWrapper> getPartnerLinks(boolean for_update);
    
    CRMPState getCRMPState();
    
    /**
     * called by the statemanger as soon as the state is persisted.
     */
    void clearPC();

}
