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
 * @(#)BPELProcessInstance.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine;

import java.util.Collection;
import java.util.List;

import com.sun.bpel.model.meta.RStartElement;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.CorrelationVal;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ScopeOrProcessUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.ClusterManager;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.IMAScopeBridge;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.InactivityReason;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.MonitorManager;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.PersistenceManager;
import com.sun.jbi.engine.bpel.core.bpel.util.PropagationContext;
import com.sun.jbi.engine.bpel.core.bpel.util.TxPropagationObject;

/**
 * BPEL process instance class
 *
 * @author Sun Microsystems
 */
public interface BPELProcessInstance extends ScopeOrProcessUnit {
	
	enum TerminationReason {ExitActivityEncountered, ForcedTermination};
	
    /**
     * gets instance ID
     *
     * @return String instance ID
     */
    String getId();

    /**
     * The BPELProcessManager instance.
     *
     * @return The static RBPELProcess model
     */    BPELProcessManager getBPELProcessManager();
    
    /**
     * gets the initiatedCorrelations. Correlations with initiate flag "No". If any of the
     * correlations that had to be initiated is not initiated, throws
     * CorrelationConsistencyConstraintFailed
     *
     * @param elem start element
     *
     * @return List list of initiated correlations
     *
     * @throws CorrelationConsistencyConstraintFailed CorrelationConsistencyConstraintFailed
     */
    List getInitiatedCorrelations(RStartElement elem);

    /**
     * gets the instance associated Correlations. Correlations with initiate  flag "join", in that
     * same sequence. Correlations from the sets that are common to all the  create_or_correlate
     * startElements
     *
     * @param elem start element
     *
     * @return List list of join initiated correlations
     */
    List getCommonJoinInitiatedCorrelations(RStartElement elem);
    
    /**
     * Used by scalability to keep track of active branches
     * 
     * @param count
     */
    void incrementActiveBranchCount(int count);
    
    /**
     * To decrement the active branch count. Used by scalability.
     * 
     * @param mCount
     */
    void decrementActiveBranchCount(int count);

    /**
     * checks and adds the correlation values to the instance
     *
     * @param correlationVals correlation values
	 * 
     * TODO(Note): This partice of setting the correlation values on the state
	 * object embedded in the process instance class through these calls have
	 * to be refactored out, as its not visible, intutive and potentially prone
	 * to errors.
     *
     * @throws CorrelationAlreadyExists CorrelationAlreadyExists
     */
    void checkAndAddCorrelationValues(List correlationVals);
    
    /**
     * Checks whether correlation are alreay associated with the instance. Currently to 
     * be used only by the one-way receive when using XA transactions 
     * @param correlationValues
     * @throws CorrelationAlreadyExists
     */
    void checkCorrelationValues(List correlationValues);
    
    /**
     * Associates the correlation with the instance. Currently to 
     * be used only by the one-way receive when using XA transactions 
     * @param correlationValues
     * @throws CorrelationAlreadyExists
     */
    void addCorrelationValues(List<CorrelationVal> correlationValues);
    
    /**
     * gets the list of correlations this instance is associated with.
     *
     * @return list of associated correlations
     */
    Collection<CorrelationVal> getInstanceAssociatedCorrVals();

    /**
     * The bp process runs to an end, call all its CallFrame.onExit()
     */
    void instanceComplete();
    
    /**
     * Invoked when the StartActivity that creates the instance has completed
     * 
     * @throws Exception  Any exception during the instantiating of EventHandlers
     *
     */
    void startActivityCompletes() throws Exception;
    
    /** This is called when the scope exists before a start activity that instantiates the 
     * instance. Scope registers itself if the start activity is not yet called.
     * Returns true if processinstance has already executed the start activity. 
     * If it returns false, then it also means that it has successfully registered the scopeUnit 
     * and that when the start activity is executed it will callback on the registered 
     * ScopeOrProcessUnit. If this method returns true the scope is respnsible for   
     * calling its own event handlers.
     * 
     * @param scopeOrProcess The Scope whose eventHandlers will be instantiated when the first start activity is done
     * @return TODO
     */
    boolean addTobeInstantiatedEventHandlers (ScopeOrProcessUnit scopeOrProcess);
    
    boolean isStartActivityDone();
    
    /**
     * adds callframe
     *
     * @param frame callframe
     */
    void addCallFrame(ICallFrame frame);
    
    /**
     * @return
     */
    PersistenceManager getPersistenctMgr();
    
    /**
     * @return
     */
    ClusterManager getClusterMgr();
    
    MonitorManager getMonitorMgr();
    
    /**
     * Mark the instance to be suspended,
     * If persistence is enabled,  update the state
     * This api will also send an alert to the alerting 
     * framework to provide and indication that the 
     * instance has been suspended.
     * @throws Exception
     */
    void suspend () throws Exception;
    
    /**
     * Return the suspended flag
     * @return true If suspended
     * @throws Exception
     */
    boolean isSuspended ();
    
    /**
     * Mark the suspended instance to resume
     * If persistence is enabled, update the state
     * @throws Exception
     */
    void resume () throws Exception;
    
    /**
     * Called at recovery to set the status to be suspended;
     *
     */
    void setSuspended ();
    
    /**
     * Returns the transaction propagation object that is associated with the business process instance.
     * 
     * @return
     */
    TxPropagationObject getTxPropagationObject();
    

    /**
     * Used by scalability to keep track of inactive callframes.
     * 
     * @param callframe
     * @param reason
     */
    public void addToInactiveList(ICallFrame callframe, InactivityReason reason);
    
    
    /**
     * Used by scalability to keep track of inactive callframes.
     * 
     * @param callframe
     * @param isOnAlarmForPick TODO
     */
    public void removeFromInactiveList(ICallFrame callframe, boolean isOnAlarmForPick);
    
    /**
     * When the scalability thread sweeps the instance and de-references all its
     * variables, the instance is marked as variables passivated as during subsequent
     * calls we don't want this instance to be picked up again.
     * During lazy loading of the variable, this instance is marked with vars not
     * passivated so that this instance can be picked by scalability thread as and
     * when scheduled.
     * 
     * @param flag
     */
    public void markVarsPassivated(boolean flag);
    
    /**
     * Returns if the variables of this instance has been passivated by scalability
     * thread or not.
     * 
     * @return
     */
    public boolean areVarsPassivated();
    
    /**
     * Orderly termination, send response to pending request on 
     * each open scope
     * @param reason
     *
     */
    public void doTerminate(TerminationReason reason);
    
    public boolean isExiting();
    
    /**
     * Extracts and sets MessageExchange artifacts from the <code>MessageContainer</code> on the 
     * <code>BPELProcessInstance</code>. This contains the parent message exchange context and also does additional 
     * housekeeping if the BP is atomic.
     * @param propContext
     * @param msgContainer
     */
    public void setParentMExArtifacts(MessageContainer msgContainer) throws Exception;
    
    /**
     * Returns the <code>PropagationContext</code> if one is associated with the <code>BPELProcessInstance</code>.
     * Returns null otherwise. 
     * @return
     */
    public PropagationContext getPropagationContext();

    /**
     * Adds the IMAScope bridge to the process instance level.
     * 
     * @param scope
     */
    void addIMAScope(IMAScopeBridge scope);

    
    boolean passesIdlenssCriterion(long idlenessCriterion);
    
    /**
     * Iterate through the list of IMAScopeBridges and returns true if any one of those have
     * active (outstanding) in-out message exchange (active Receive for receive-reply operation on wsdl)  
     * @param idlenessCriterion TODO
     * @return
     */
    Object[] qualifiesForInstancePassivation(long idlenessCriterion);

    
    /**
     * This is to guard against the persistence and scalability 
     * threads stepping into each other. The phase 1 scalability
     * would passivate the variable and change the flags maintained 
     * at runtime variable. If the instance is in the middle of persistence
     * these variables should not be touched by the scalability thread
     * and vice versa.
     * 
     * The boolean return is used by the scalability only and is ignored
     * by the regular persistence. The return of false indicates
     * that the scalability thread was not able to acquire the lock
     * since the instance is in the middle of persistence. Under such
     * circumstances, scalability would leave the variable passivation
     * for this tread and move on to next instance. This instance
     * can be visited later after the persistence for this instance
     * is complete.
     * 
     * @param isPassivation
     * @return
     */
    boolean acquirePersistenceLock(boolean isPassivation);
    
    /**
     * Release the Persistence lock.
     * 
     * @param isPassivation
     */
    void releasePersistenceLock(boolean isPassivation);
}
