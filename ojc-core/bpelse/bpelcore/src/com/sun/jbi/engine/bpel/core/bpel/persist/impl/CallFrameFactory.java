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
 * @(#)CallFrameFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.persist.impl;

import java.io.Reader;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.wsdl.Message;
import javax.xml.namespace.QName;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Text;

import com.sun.bpel.model.Catch;
import com.sun.bpel.model.Else;
import com.sun.bpel.model.ElseIf;
import com.sun.bpel.model.EventHandlers;
import com.sun.bpel.model.EventHandlersHolder;
import com.sun.bpel.model.EventHandlersOnAlarm;
import com.sun.bpel.model.EventHandlersOnEvent;
import com.sun.bpel.model.FaultHandlerScope;
import com.sun.bpel.model.Flow;
import com.sun.bpel.model.ForEach;
import com.sun.bpel.model.If;
import com.sun.bpel.model.Pick;
import com.sun.bpel.model.Scope;
import com.sun.bpel.model.SingleActivityHolder;
import com.sun.bpel.model.SystemFault;
import com.sun.bpel.model.Variables;
import com.sun.bpel.model.Wait;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RActivityHolder;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.ROnAlarm;
import com.sun.bpel.model.meta.RPartnerLink;
import com.sun.bpel.model.meta.RPersistable;
import com.sun.bpel.model.meta.RVariable;
import com.sun.bpel.model.meta.ScopingElement;
import com.sun.bpel.xml.NamespaceUtility;
import com.sun.jbi.engine.bpel.core.bpel.dbo.EventHandlerDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.ForEachDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.LastCheckPointDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.PartnerLinkDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.ScopeDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.VariableDBO;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.RecoveredCallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.CallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.JBIMessageImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.RecoveredCallFrameImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityContainerUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnitFactory;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.CompensateContinuation;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.EventHandlersChildAct;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.EventHandlersOnEventUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Fault;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.PartnerLinkScope;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeEventHandlers;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ScopeOrProcessUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.StateContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.BPELProcessInstanceImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.CatchUnitImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.CompensateUnitImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.FaultImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.ForEachUnitSerialImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.InvokeUnitImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.PickUnitImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.RuntimeEventHandlersImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.RuntimeVariableImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.ScopeUnitImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.VirtualCatchAllUnit;
import com.sun.jbi.engine.bpel.core.bpel.persist.MutableState;
import com.sun.jbi.engine.bpel.core.bpel.persist.RecoveredState;
import com.sun.jbi.engine.bpel.core.bpel.persist.State;
import com.sun.jbi.engine.bpel.core.bpel.persist.MutableState.MutableEventState;
import com.sun.jbi.engine.bpel.core.bpel.util.DOMHelper;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public class CallFrameFactory {
    private static Logger LOGGER = Logger.getLogger(CallFrameFactory.class.getName());
    
    //The key is the parent scope and the value is the scope which is executing CH.
    private HashMap <FaultHandlingContext, FaultHandlingContext> compensatingScopes;
    
    /**
     * get instance of CallFrameFactory
     *
     * @return CallFrameFactory CallFrameFactory
     */
    public static CallFrameFactory getInstance() {
        return new CallFrameFactory();
    }

    /**
     * if there are child call frames that need to be executed, the parent doesn't need to be
     * executed, since the join of the children will trigger the parent's execution.
     * @param info
     * @return returns an array of three objects. the first is the root ICallFrame. second is the
     *         list of callframes that includes the root callframe. third value in the array is the processInstance
     */
    public Object[] getCallFrames(RecoveredState info) {
        // exit if there are no checkpoints
        if (!info.hasCheckpoints()) return null;
        
        //The key is the parent scope and the value is the scope which is executing CH.
        compensatingScopes = new HashMap<FaultHandlingContext, FaultHandlingContext>();
        
        // TODO: This is a temporary fix. Similar code exists in BPELProcessInstanceImpl.getStaticModelActivity()
        // needs to be fixed there also.
        RActivity rootAct = (RActivity) info.getProcessDef().getActivity();
        BPELProcessInstance instance = info.getProcessInstance();
		List <ScopeDBO> scopeDBOs = info.getScopeDBOs(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);
		
		if (scopeDBOs != null) {
	    	recreateScopeState(scopeDBOs.get(0), (BPELProcessInstanceImpl)instance, 
	    			(info.getProcessDef()).getVariables().getVariables(), info);
		} else {
			//State of the process unit was not persisted yet. This happens for cases like the following
			//<process>
			// <flow>
			//   <receive>
			//	</flow>
			//	...
			//</process>
			//Here's the flow and the process state is not persisted. Only the receive is persisted.
			//Since the flow was not persisted, the scope state for the process instance was also
			//not persisted (we persist scope states only for the branch on which the persistence 
			//point is hit). So we need to create the scope state manually. Note that in this case
			//the state will always be running.
	        List<RuntimeVariable> runtimeVars = recreateVariables((info.getProcessDef()).getVariables().getVariables(), 
	        		instance, RBPELProcess.DEFAULT_PROCESS_SCOPE_ID.toString(), info);
            List<RuntimePartnerLink> runtimePLinks = recreatePartnerLinks((PartnerLinkScope) instance, info);
	        ((Context) instance).getStateContext().getState().updateScope(
	        		RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(), (FaultHandlingContext)instance);
	        ((Context) instance).initUponRecovery(runtimeVars, runtimePLinks);			
		}
		
        ((State) ((Context) instance).getStateContext().getState()).markInserted();
        
        UnSynchronizedStack stack = new UnSynchronizedStack();
        
        createChildUnit(rootAct, RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(), 
        		(BPELProcessInstanceImpl)instance, stack, info, true);
        
        traverseDepthForScope((EventHandlersHolder) info.getProcessDef(), 
                stack, info, (ActivityUnit) instance);

        traverseDepthForScope((FaultHandlerScope) info.getProcessDef(), 
                stack, info, (BPELProcessInstanceImpl)instance);
        
        ICallFrame rootCF = info.getRootCallFrame();
        List<ICallFrame> cfs = new ArrayList<ICallFrame>(info.getCallFrames());

        //create CallFrames for the brances which were not persisted
        //these are NON RecoveredCallFrame
        List<ICallFrame> uninitiatedFlowCFs = uninitiatedFlowBranchCallFrames(info);
        cfs.addAll(uninitiatedFlowCFs);
        
        //we do not schedule CFs pointing at Flow, 
        //when all the branches are complete, they schedule these parent CFs
        cfs.removeAll(info.getParentCFsWithCheckPointedChildCFs());

        // add callframes corresponding to the uninitiated event handlers
        cfs.addAll(info.getUninitializedEHCallFrames());
        
        return new Object[] {rootCF, cfs, instance};
    }

    /**
     * traverse the tree to find the activity with the matching Id and construct a callframe. it is
     * a recursive call. Traverse depth first till you hit leaf nodes and then compare the Ids of
     * the nodes. creation of activity units CAN BE deferred. the current implementation is chosen
     * to avoid another looping of the tree to construct the unit nodes.
     * 
     * @param root
     * @param stack
     * @param holder
     * @param rootUnit
     */
    private void traverseDepth(RActivity root, 
                               UnSynchronizedStack stack,
                               RecoveredState info,
                               ActivityUnit rootUnit) {
        if (Utility.checkClassType(root, Flow.class)) {
            DS ds = new DS((Flow) root, rootUnit);
            stack.push(ds);
        } 
        
        if (root instanceof ForEach) {
            ForEachDBO dbo = info.findForEach(root.getUniqueId());
            if (dbo != null) {
                ((ForEachUnitSerialImpl)rootUnit).initUponRecovery(dbo.getCounter(), dbo.getSuccesses(), 
                		dbo.getStartCount(), dbo.getFinalCount(), dbo.getCompletionCount());
                /* initialize the recovery for the for each entry in the dbo during this travesal.
                 * IMP: Bug 591: The assumption is made that we will traverse the entire depth of the bpel tree
                 * and hence traverse through all the ForEach activities in the bpel tree. One fact that validates
                 * this assumption is that since we need to reconstruct all the completed scopes for compensation 
                 * handling we would be travesing the whole tree. If for some reason this assumption is not true
                 * then we have to make sure that the traversal is complete, because adding any hack to recover
                 * all the ForEach DBO entries without the branch information leads to complexity in maintaining 
                 * the recovered states in the StateImpl. 
                 */ 
                long branchId = rootUnit.getBranchId();
                MutableState state = ((StateContext) info.getProcessInstance()).getState();
                state.recover(info, branchId);
            }
        }

        if (root instanceof RActivityHolder) {
        	RActivity child = ((RActivityHolder) root).getChildActivity();
        	while (child != null) {
        		
            	//Determine child branch id
            	long childBranchID = 0;
            	if(root instanceof Flow){
            		//If the parent is a flow then we create create new branches for the child activities
            		//We need to update the state with this new branch id
            		childBranchID = child.getUniqueId(); 
            		// update state with branch information for all activities
            		MutableState state = rootUnit.getContext().getStateContext().getState();
            	} else {
            		//The branch id does not change
            		childBranchID = rootUnit.getBranchId();            		
            	}
            	
        		createChildUnit(child, childBranchID, rootUnit, stack, info, true);
        		child = child.getNextActivity();
        	}
        	
        } else if (root instanceof Pick) {
            long actId = root.getUniqueId();
            if (!info.containsCheckpoint(actId)) {
                //If the last check point was this pick, there is no value in traversing down into the  
            	//pick. This is so because if the persistence point was inside pick, the LCP would have 
            	//been updated with the activity related to that persistence point.
                traverseDepthForPick(root, stack, info, rootUnit);
            }
        }
        
        if (root instanceof If) {
            // to traverse the else and elseIf blocks under IF element.
            traverseDepthForIf(root, stack, info, rootUnit);
        } 
        
        if (root instanceof EventHandlersHolder) {
            // BPELProcess, Scope satisfy this criteria
            traverseDepthForScope((EventHandlersHolder) root, 
                    stack, info, rootUnit);
        }
        
        if (root instanceof Scope) {
            traverseDepthForScope((FaultHandlerScope) root, stack, 
                                  info, (FaultHandlingContext)rootUnit);
        }

        DS ds = null;
        if (Utility.checkClassType(root, Flow.class)) {
        	ds = stack.pop();
        }

        if (root instanceof RPersistable && info.continueTraversing() 
        		&& !rootUnit.getContext().getFaultHandlingContext().getScopeState().equals(FaultHandlingContext.COMPLETED) ) {
        	long actId = root.getUniqueId();
        	if ((ds != null) && (ds.mFrame != null)
					&& (((RActivity) ds.mAct).getUniqueId() == actId)) {
        		//CF created for this FLOW while traversing children
        	} else {
        	LastCheckPointDBO lcpdbo = info.findCheckpoint(actId);
        	if (lcpdbo != null) {
        		createCallFrame(info, root, stack, lcpdbo, rootUnit);

				//We persist a fault only when an invoke returns it. So check if the LCP is
        		//associated with an invoke and whether a fault was persisted.
        		if(rootUnit instanceof InvokeUnitImpl) {
            		long faultedActId = rootUnit.getContext().getFaultHandlingContext().getFaultActivityId();
            		if(root.getUniqueId() == faultedActId) {
            			((InvokeUnitImpl)rootUnit).setFault(rootUnit.getContext().getFaultHandlingContext().getFault());
            		}
        		}
        	}
        }
        }
    }
    
    private void createChildUnit(RActivity child, long childBranchID, Unit parentUnit, 
    		UnSynchronizedStack stack, RecoveredState info, boolean traverseDepth) {

    	Context context = null;
    	if(parentUnit instanceof Context) {
    		context = (Context)parentUnit;
    	} else {
    		context = parentUnit.getContext();
    	}
    	
    	if (child instanceof Scope) {
    		List<ScopeDBO> scopeDBOs = info.getScopeDBOs(child.getUniqueId() + RecoveredState.IDS_SEPARATOR 
    				+ context.getFaultHandlingContext().getScopeGuid());
			if (scopeDBOs != null) {
				
				for (ScopeDBO scopeDBO : scopeDBOs) {
					ActivityUnit scopeUnit = new ScopeUnitImpl(context, parentUnit, child, childBranchID, scopeDBO.getScopeGuid());
					
					ArrayList <RVariable> scopeVariableDefs = new ArrayList <RVariable>();
					Variables variables = ((Scope)child).getVariables();
					if(variables != null){
						scopeVariableDefs.addAll(variables.getVariables());
					}

					if (parentUnit instanceof ForEachUnitSerialImpl) {
						// Get the variable corresponding to the counter and add it to the scope variables
						RVariable counterVariable = (RVariable) ((ForEach) ((ActivityUnit) parentUnit)
								.getStaticModelActivity()).getCounterVariable();
						scopeVariableDefs.add(counterVariable);
					} else if (parentUnit instanceof EventHandlersOnEventUnit) {
		    			((EventHandlersOnEventUnit)parentUnit).setChildScope((ScopeUnitImpl)scopeUnit);
		    		}
					
					if (parentUnit instanceof EventHandlersChildAct) {
			    		((MutableEventState)context.getStateContext().getState()).setAssociatedScopeGuid(
			    				scopeDBO.getScopeGuid());
					}

					recreateScopeState(scopeDBO, (ScopeUnitImpl) scopeUnit, scopeVariableDefs, info);
					if(traverseDepth) {
						traverseDepth(child, stack, info, scopeUnit);
					}
				}
			}
    	} else {
    		ActivityUnit childUnit = ActivityUnitFactory.getInstance().createActivityUnit(
    				context, parentUnit, child, childBranchID);
    		if(childUnit instanceof CompensateContinuation){
    			FaultHandlingContext childScopeExecutingCH = compensatingScopes.get(childUnit.getContext().getFaultHandlingContext());
    			//Check whether this CompensateContinuation is the one that called compensate on the child scope
    			//that was executing CH
        		if (childScopeExecutingCH != null && childScopeExecutingCH.getCompensateId() == child.getUniqueId()) {
        			childScopeExecutingCH.setCompensateContinuation(childUnit);
        		}
    		}
			if(traverseDepth) {
				traverseDepth(child, stack, info, childUnit);    		
			}
    	}
    }
    
    private void traverseDepthForPick(RActivity root, 
                                      UnSynchronizedStack stack,
                                      RecoveredState info,
                                      ActivityUnit rootUnit) {
        Pick pick = (Pick) root;

        for (int i = 0, onAlrmSize = pick.getOnAlarmSize(); i < onAlrmSize; i++) {
            ROnAlarm onAlrm = (ROnAlarm) pick.getOnAlarm(i);
            RActivity child = onAlrm.getChildActivity();
            createChildUnit(child, rootUnit.getBranchId(), rootUnit, stack, info, true);
        }

        for (int j = 0, onMesgSize = pick.getOnMessageSize(); j < onMesgSize; j++) {
            RActivityHolder onMesg = (RActivityHolder) pick.getOnMessage(j);
            RActivity child = onMesg.getChildActivity();
            createChildUnit(child, rootUnit.getBranchId(), rootUnit, stack, info, true);
        }
    }

    private void traverseDepthForIf(RActivity root, 
                                    UnSynchronizedStack stack,
                                    RecoveredState info,
                                    ActivityUnit rootUnit) {
        // Do the ElseIf and Else block navigation (If block traversal is already taken care of)
        If ifBlock = (If) root;
        Collection elseIfs = ifBlock.getElseIfs();
        long childBranchID = Long.MIN_VALUE;
        childBranchID = rootUnit.getBranchId();
        
        if (elseIfs != null) {
            Iterator iter = elseIfs.iterator();
            while (iter.hasNext()) {
                ElseIf elseIfClause = (ElseIf) iter.next();
                RActivity child = ((RActivityHolder) elseIfClause).getChildActivity();
                createChildUnit(child, rootUnit.getBranchId(), rootUnit, stack, info, true);
            }
        }
        
        Else elsee = (Else) ifBlock.getElse();
        if(elsee != null) {
            RActivity child = ((RActivityHolder) elsee).getChildActivity();
            createChildUnit(child, rootUnit.getBranchId(), rootUnit, stack, info, true);
        }
    }
    
    private void traverseDepthForScope(FaultHandlerScope processOrScope, 
                                           UnSynchronizedStack stack,
                                           RecoveredState info, 
                                           FaultHandlingContext rootUnit) {
    	String scopeState = rootUnit.getScopeState();
    	long branchId = ((ActivityUnit)rootUnit).getBranchId();
    	boolean traverseCT =  false;
    	RActivity tcActivity = null;
    	
    	if(scopeState.equals(FaultHandlingContext.COMPLETED)
    			|| scopeState.equals(FaultHandlingContext.RUNNING)
    			|| scopeState.equals(FaultHandlingContext.FAULTED) 
    			|| scopeState.equals(FaultHandlingContext.TERMINATED)) {
    		return;
    	}

    	if(scopeState.equals(FaultHandlingContext.EXE_FAULT_HANDLER)
    			|| scopeState.equals(FaultHandlingContext.FAULTED_IN_FH)) {
    		
    		Fault fault = rootUnit.getFault();
    		Unit fhUnit = ActivityUnitFactory.getInstance().createFaultHandlingUnit((Context)rootUnit, fault, 
    				processOrScope.getFaultHandlers());
    		
    		if(fhUnit != null) {
    			SingleActivityHolder catchOrCatchAll = (SingleActivityHolder)((ActivityContainerUnit) fhUnit).getStaticModel();
    			RActivity child = (RActivity) catchOrCatchAll.getActivity();

    			if(fhUnit instanceof CatchUnitImpl) {
    				Catch catchModel = (Catch) catchOrCatchAll;
    				RVariable var = (RVariable)catchModel.getBPELFaultVariable();

    				if(var != null) {
        				String catchId = String.valueOf(((ScopingElement)catchModel).getScopeId());
        				RuntimeVariable runtimeVar = new RuntimeVariableImpl(var, ((Context)fhUnit).getProcessInstance(), 
        						catchId);
        				VariableDBO varDBO = info.getVariables().get(var.getUniqueId() + RecoveredState.IDS_SEPARATOR 
        						+ catchId);
        				if (varDBO != null) {
        					runtimeVar.setSerializedValue(varDBO.getValue());
        					runtimeVar.markInserted();
        				}
        				
            			List<RuntimeVariable> runtimeVars = new ArrayList<RuntimeVariable>();
        				runtimeVars.add(runtimeVar);
        				
            			((Context) fhUnit).initUponRecovery(runtimeVars, new ArrayList<RuntimePartnerLink>());
    				}
    			}

    			createChildUnit(child, branchId, fhUnit, stack, info, true);

    		} else {
    			VirtualCatchAllUnit virtualCatchAllUnit = new VirtualCatchAllUnit((Context)rootUnit, (RActivity)processOrScope, fault);
    			//Check if a child scope needs to be set with its compensate unit
    			FaultHandlingContext childScopeExecutingCH = compensatingScopes.get(rootUnit);
        		if (childScopeExecutingCH != null) {
        			childScopeExecutingCH.setCompensateContinuation(virtualCatchAllUnit.getVirtualCompensateUnit());
        		}
    		}
    	} else if ((scopeState.equals(FaultHandlingContext.EXE_TERMINATION_HANDLER) 
    			|| scopeState.equals(FaultHandlingContext.FAULTED_IN_TH))
    			&& rootUnit instanceof ScopeUnitImpl) {
    		
    		traverseCT = true;
    		if(((Scope)processOrScope).getTerminationHandler() != null) {
        		tcActivity = (RActivity)((Scope)processOrScope).getTerminationHandler().getActivity();    			
    		}
    		
    	} else if((scopeState.equals(FaultHandlingContext.EXE_COMPENSATION_HANDLER)
    			|| scopeState.equals(FaultHandlingContext.FAULTED_IN_CH)) 
    			&& rootUnit instanceof ScopeUnitImpl) {

    		traverseCT = true;
    		if(((Scope)processOrScope).getCompensationHandler() != null) {
        		tcActivity = (RActivity)((Scope)processOrScope).getCompensationHandler().getActivity();    			
    		}
    		
    		//Since this scope is executing CH, we need to find and set the CompensateUnit on it. Here we simply
    		//put this scope in a map so that when a parent scope can set the compensate activity later.
    		compensatingScopes.put(((Context)rootUnit).getParentContext().getFaultHandlingContext(), rootUnit);            	
    	} 
    	
    	if(traverseCT) {
        	//We need to traverse CH or TH
        	if(tcActivity != null) {
    			createChildUnit(tcActivity, branchId, (Unit)rootUnit, stack, info, true);
    			
        	} else {
        		ActivityUnit virtualCompensate = new CompensateUnitImpl((Context)rootUnit, (Unit)rootUnit, null, branchId, true);
    			//Check if a child scope needs to be set with its compensate unit
        		FaultHandlingContext childScopeExecutingCH = compensatingScopes.get(rootUnit);
        		if (childScopeExecutingCH != null) {
        			childScopeExecutingCH.setCompensateContinuation(virtualCompensate);
        		}
        	}
    	}
    }

    private void recreateScopeState(ScopeDBO scopeDBO, FaultHandlingContext processOrScopeUnit, 
    		Collection<RVariable> vars, RecoveredState info) {
    	
    	processOrScopeUnit.markInserted();
    	
    	String scopeState = scopeDBO.getScopeState();
    	processOrScopeUnit.setScopeState(scopeDBO.getScopeState());
    	if (processOrScopeUnit instanceof ScopeUnitImpl) {
    		processOrScopeUnit.setCompletionOrder(scopeDBO.getCompletionOrder());
    		processOrScopeUnit.setCompensateId(scopeDBO.getCompensateId());
    	}
    	
    	if (scopeDBO.getFaultName() != null) {
        	processOrScopeUnit.setFault(getFault(scopeDBO, info), scopeDBO.getFaultActivityId());
    	}

        List<RuntimeVariable> runtimeVars = recreateVariables(vars, ((Context)processOrScopeUnit).getProcessInstance(), 
        		scopeDBO.getScopeGuid(), info);
        List<RuntimePartnerLink> runtimePLinks = recreatePartnerLinks((PartnerLinkScope) processOrScopeUnit, info);
            
        ((Context) processOrScopeUnit).initUponRecovery(runtimeVars, runtimePLinks);
            
        if(processOrScopeUnit instanceof ScopeUnitImpl) {
            ((Context)processOrScopeUnit).getParentContext().getFaultHandlingContext().registerEnclosedScope(processOrScopeUnit);        	
        }
    }
    
    private List <RuntimeVariable> recreateVariables(Collection<RVariable> vars, BPELProcessInstance instance, 
    		 String scopeGuid, RecoveredState info){
        List<RuntimeVariable> runtimeVars = new ArrayList<RuntimeVariable>();

    	if (vars != null){
        	for (RVariable var : vars) {
        		RuntimeVariable runtimeVar = new RuntimeVariableImpl(var, instance, scopeGuid);
        		VariableDBO varDBO = info.getVariables().get(var.getUniqueId() 
        				+ RecoveredState.IDS_SEPARATOR + scopeGuid);
                if (varDBO != null) {
                    runtimeVar.setSerializedValue(varDBO.getValue());
                }
                runtimeVars.add(runtimeVar);    		
        	}
    	}
    	
    	return runtimeVars;
    }
    
    private List <RuntimePartnerLink> recreatePartnerLinks(PartnerLinkScope pLinkScope, 
             RecoveredState info){
        Collection<RPartnerLink> pLinks = info.getProcessDef().getPartnerLinks().getPartnerLinks();
        List<RuntimePartnerLink> runtimePLinks = new ArrayList<RuntimePartnerLink>();

        // TODO As of today we only support partnerlinks defined at the process level
        if (!(pLinkScope instanceof BPELProcessInstance)) {
            return runtimePLinks;
        }
        
        if (pLinks != null){
            for (RPartnerLink pLink : pLinks) {
                // TODO This call of constructor will initialise the internal EPR.
                // How do we avoid this (It is not costly but if it can be avoided it is better)
                RuntimePartnerLink runtimePLink = pLinkScope.createRuntimePartnerLink(pLink);
                PartnerLinkDBO pLinkDBO = info.getPartnerLinks().get(pLink.getUniqueId() 
                        + RecoveredState.IDS_SEPARATOR + runtimePLink.getScopeGuid());
                if (pLinkDBO != null) {
                    runtimePLink.setSerializedValue(pLinkDBO.getValue());
                }
                runtimePLinks.add(runtimePLink);            
            }
        }
        
        return runtimePLinks;
    }
    
    private Fault getFault(ScopeDBO scopeDBO, RecoveredState info) {
    	
    	QName faultName = scopeDBO.getFaultName();
    	Reader reader = scopeDBO.getFaultData();
    	
    	if (reader == null) {
    		return new FaultImpl(faultName, null);
    	}
    	Document doc = DOMHelper.readDocument(reader);
        Element docElement = doc.getDocumentElement();
        String wsdlMsgTypeAttr = docElement.getAttribute("type");
        String prefix = NamespaceUtility.getPrefix(wsdlMsgTypeAttr);
        String localName = NamespaceUtility.getLocalName(wsdlMsgTypeAttr);
        String namespace = docElement.lookupNamespaceURI(prefix);
        QName wsdlMsgTypeAttrQName = new QName(namespace, localName);
        Message msg = info.getProcessDef().getWSDLMessage(wsdlMsgTypeAttrQName);
        
        WSMessage wsMsg = new JBIMessageImpl(doc, msg);
        return new FaultImpl(faultName, wsMsg);
    }

    /* For event handlers which are no longer active (either because the scope has
     * completed or is executing FCT handler) we need to traverse the event handler
     * hierarchy to create the completed scopes inside the EH. This is needed for
     * compensation.
     */
    private void traverseDepthForInactiveEventHandlers(EventHandlersHolder processOrScope, 
            UnSynchronizedStack stack,
            RecoveredState info, 
            ActivityUnit rootUnit) {
    	
        EventHandlersOnEvent[] onEvents = processOrScope.getEventHandlers().getOnEvents();
        RActivity onEvent = null;
        for (int j = 0; j < onEvents.length; j++) {
            onEvent = (RActivity)onEvents[j];
            RActivity associatedScope = ((RActivityHolder)onEvent).getChildActivity();
            createChildUnit(associatedScope, rootUnit.getBranchId(), rootUnit, stack, info, true);
        }
        
        EventHandlersOnAlarm[] onAlarms = processOrScope.getEventHandlers().getOnAlarms();
        RActivity onAlarm = null;
        for (int j = 0; j < onAlarms.length; j++) {
            onAlarm = (RActivity)onAlarms[j];
            RActivity associatedScope = ((RActivityHolder)onAlarm).getChildActivity();
            createChildUnit(associatedScope, rootUnit.getBranchId(), rootUnit, stack, info, true);
        }
    }
    
    private void traverseDepthForScope(EventHandlersHolder processOrScope, 
                                       UnSynchronizedStack stack,
                                       RecoveredState info, 
                                       ActivityUnit rootUnit) {
        
        EventHandlers eh = processOrScope.getEventHandlers();
        if (eh == null) {
            return;
        }
        
        if(!((FaultHandlingContext)rootUnit).getScopeState().equals(FaultHandlingContext.RUNNING)) {
        	traverseDepthForInactiveEventHandlers(processOrScope, stack, info,  rootUnit);
        	return;
        }
    	
        String scopeGuid = ((FaultHandlingContext)rootUnit).getScopeGuid();
    	
        List<EventHandlerDBO> eventDBOs = info.findEventhHandlers(scopeGuid);
        boolean isScopeMarkedAsLCP = info.containsCheckpoint(rootUnit.getStaticModelActivity().getUniqueId());        
        
        RuntimeEventHandlers rEH = new RuntimeEventHandlersImpl(
                (Context) rootUnit, info.getProcessInstance(), (ScopeOrProcessUnit) rootUnit, rootUnit,
                eh, rootUnit.getBranchId(), info.getEngine(), info.getInterpreter(), 
                info.getProcessInstance().getBPELProcessManager(), scopeGuid);

        ((Context) rootUnit).initEHUponRecovery(rEH);
        
        /************************************************************
         * Event recovery matrix looks like this. I=Inserted, U=Updated, P=Persisted, NP=Non-persisted, PIS=persisted inside the scope.
         * ------------------------------------------------
         * OnEvent Ancestor-Scope   Recover-Reconstruct Events 
         * ------------------------------------------------
         *   NP         P               schedule a new one
         *   NP         PIS             schedule a new one
         *   I          P               Recover persisted, schedule a new one    
         *   I          PIS             Recover persisted, schedule a new one
         *   U          P               Recover persisted, schedule a new one
         *   U          PIS             Recover persisted, schedule a new one
         * 
         * ------------------------------------------------
         * Repeat-Every  Ancestor-Scope  Recover-Reconstruct Events
         * ------------------------------------------------
         *   NP         P               schedule a new one
         *   NP         PIS             schedule a new one
         *   I          P               Recover persisted, schedule a new one
         *   I          PIS             Recover persisted, schedule a new one
         *   U          P               Recover persisted, schedule a new one
         *   U          PIS             Recover persisted, schedule a new one
         *   
         * ------------------------------------------------
         * OnAlarm  Ancestor-Scope Recover-Reconstruct Events 
         * ------------------------------------------------
         *   NP         P               schedule a new one
         *   NP         PIS             schedule a new one
         *   I          P               Recover persisted
         *   I          PIS             Recover persisted
         *   U          P               Recover persisted
         *   U          PIS             Recover persisted
         * 
         *   This needs to be considered with the situation when multiple onEvents, 
         *   onAlarms, onAlarm-repeatEverys could be defined. 
         */
        
        
        if (eventDBOs == null || eventDBOs.size() == 0) {
            // whether the scope is the persistence point or the something within the scope is updated,
            // since there were no Events persisted we create all the events.
            constructOnEventFrames(rEH, eh, info);
            constructEHOnAlarmFrames(rEH, eh, info, false);
            return;
        }
        
        // Recovery for onEvents and OnAlarm-repeatEvery
        constructOnEventFrames(rEH, eh, info);
        constructEHOnAlarmFrames(rEH, eh, info, true);
        
        // If this EH is recorded in DB, then we need to recurse further down the path
        // of this EH to find nested persisted points 
               
        EventHandlersOnEvent[] onEvents = eh.getOnEvents();
        EventHandlersOnAlarm[] onAlarms = eh.getOnAlarms();
        
        List<EventHandlersOnAlarm> activeOnAlarms = new ArrayList<EventHandlersOnAlarm>();

        for (int i = 0, size = eventDBOs.size(); i < size; i++) {
            EventHandlerDBO eventDBO = eventDBOs.get(i);
            long eventOrAlarmId = eventDBO.getEventModelId();
            Timestamp ts = eventDBO.getTimerValue();
            // find the onEvent 
            if (ts == null) {
                EventHandlersOnEvent tempOnEvent;
                for (int j = 0; j < onEvents.length; j++) {
                    tempOnEvent = onEvents[j];
                    if (eventOrAlarmId == ((RActivity) tempOnEvent).getUniqueId()) {
                        
                        recoverOnEventFrames(rEH, eventDBO, tempOnEvent, info, rootUnit);
                        break;
                    }
                }
            } else {
                EventHandlersOnAlarm tempOnAlarm;
                // find the onAlarm (if eventOrAlarmId is onEventId, then continue outer for loop)
                for (int j = 0; j < onAlarms.length; j++) {
                    tempOnAlarm = onAlarms[j];
                    if (eventOrAlarmId == ((RActivity) tempOnAlarm).getUniqueId()) {
                        // for simplicity of code, using this object list.
                        activeOnAlarms.add(tempOnAlarm);
                        
                        recoverOnAlarmFrames(rEH, eventDBO, tempOnAlarm, info, rootUnit);
                        break;
                    }
                }            
            }
        }
                
        // construct frames for all the non repeating onAlarms that haven't started yet (that weren't 
        // persisted before the crash). We have already launched all the OnAlarm-RepeatEvery, 
        // we just need to launch the non repeating onAlarms.
        EventHandlersOnAlarm tempOnAlarm;
        for (int j = 0; j < onAlarms.length; j++) {
            tempOnAlarm = onAlarms[j];
            if (!activeOnAlarms.contains(tempOnAlarm)) {
                if (com.sun.jbi.engine.bpel.core.bpel.util.Utility.isRepeatEveryDefined(tempOnAlarm)) {
                    continue;
                }
                ICallFrame onAlrmFrame = rEH.createFrameForRecovery(tempOnAlarm);
                info.addUninitializedEHCallFrames(onAlrmFrame);
            }
        }
    }
        
    private void updateState(ActivityUnit actUnit, EventHandlerDBO eveDBO) {
        long eventId = actUnit.getStaticModelActivity().getUniqueId();
        State ehState = (State) actUnit.getContext().getStateContext().getState();
        ehState.markInserted();
        ((MutableState) ehState).updatePC(actUnit.getBranchId(), eventId);
        ehState.clearPC();
        ((EventStateImpl) ehState).setIsUpdatedInDB(EventHandlerDBO.UPDATE_SATUS.equals(eveDBO.isUpdated()));
    }
    
    /**
     * @param rEH
     * @param eh
     * @param info
     * @param createAlarmsWithOnlyRepeatEvery if true it means create only for RepeatEvery onAlarms. if false, 
     * it means create for both Repeat-Every as well as onAlarm with out repeat-every. 
     */
    private void constructEHOnAlarmFrames(RuntimeEventHandlers rEH, 
            EventHandlers eh, RecoveredState info, boolean createAlarmsWithOnlyRepeatEvery) {
        
        EventHandlersOnAlarm[] onAlarms = eh.getOnAlarms();
        
        for (int j = 0; j < onAlarms.length; j++) {
        	
        	if(createAlarmsWithOnlyRepeatEvery) {
        		if(com.sun.jbi.engine.bpel.core.bpel.util.Utility.isRepeatEveryDefined(onAlarms[j])) {
                    ICallFrame onAlrmFrame = rEH.createFrameForRecovery(onAlarms[j]);
                    info.addUninitializedEHCallFrames(onAlrmFrame);
        		}
        	} else {
                ICallFrame onAlrmFrame = rEH.createFrameForRecovery(onAlarms[j]);
                info.addUninitializedEHCallFrames(onAlrmFrame);        		
        	}
        }
    }       
    
    private void constructOnEventFrames(RuntimeEventHandlers rEh, 
            EventHandlers eh, RecoveredState info) {
        
        EventHandlersOnEvent[] onEvents = eh.getOnEvents();
        ICallFrame onEventFrame;
        for (int j = 0; j < onEvents.length; j++) {
            onEventFrame = rEh.createFrameForRecovery(onEvents[j], null);
            info.addUninitializedEHCallFrames(onEventFrame);
        }
    }

    private void recoverOnEventFrames(RuntimeEventHandlers rEH, EventHandlerDBO onEventDBO, 
            EventHandlersOnEvent onEvent, RecoveredState info, ActivityUnit rootUnit) {

        if (onEventDBO.isUpdated()) {
            // If the DBO is updated, it means the frame was persisted somewhere else within the onEvent.
            
            //Check if the associated scope is complete. If yes, then the event handlers are inactive.
            //We still need to traverse the event handler to recreate the scopes in the event handlers
            //for compensation
            if(info.isEHAssociatedScopeComplete(onEventDBO.getId())){
                RActivity childScope = ((RActivityHolder)onEvent).getChildActivity();
                createChildUnit(childScope, rootUnit.getBranchId(), rootUnit, new UnSynchronizedStack(), info, true);
                return;
            }
            
            // Else, the recovery of that last check point will schedule the frame. So traverse the onEvent.
            // ** There is no need to register this recoveredFrame for recovery, because we 
            // would register the frame corresponding to the persistence point within the EH
            ActivityUnit actUnit = (ActivityUnit) rEH.createOnEventUnitForRecovery(onEvent, 
                    onEventDBO.getId());
            updateState(actUnit, onEventDBO);
            
            RActivity childScope = ((RActivityHolder)actUnit.getStaticModelActivity()).getChildActivity();
            //create and traverse the child scope unit since the LCP is somewhere inside the scope
            createChildUnit(childScope, actUnit.getBranchId(), actUnit, new UnSynchronizedStack(), info, true);
            
        } else {
            //createOnEvent Frame
            ICallFrame onEveFrame = rEH.createFrameForRecovery(onEvent, onEventDBO.getId());
            updateState(onEveFrame.getProgramCounter(), onEventDBO);
            info.addUninitializedEHCallFrames(onEveFrame);
            ActivityUnit onEveUnit = onEveFrame.getProgramCounter();
            
            RActivity childScope = ((RActivityHolder)onEveUnit.getStaticModelActivity()).getChildActivity();
            //Just create the child scope, no need to traverse it
            createChildUnit(childScope, onEveUnit.getBranchId(), onEveUnit, new UnSynchronizedStack(), info, false);
        }        
    }
    
    private void recoverOnAlarmFrames(RuntimeEventHandlers rEH, EventHandlerDBO onAlarmDBO, 
            EventHandlersOnAlarm onAlarm, RecoveredState info, ActivityUnit rootUnit) {

        if (onAlarmDBO.isUpdated()) {
            // If the DBO is updated, it means the frame was persisted somewhere else within the onAlarm.
            
            //Check if the associated scope is complete. If yes, then the event handlers are inactive.
            //We still need to traverse the event handler to recreate the scopes in the event handlers
            //for compensation
            if(info.isEHAssociatedScopeComplete(onAlarmDBO.getId())){
                RActivity childScope = ((RActivityHolder)onAlarm).getChildActivity();
                createChildUnit(childScope, rootUnit.getBranchId(), rootUnit, new UnSynchronizedStack(), info, true);
                return;
            }

            // Else, the recovery of that last check point will schedule the frame. So traverse the onAlarm.
            // ** There is no need to register this recoveredFrame for recovery, because we 
            // would register the frame corresponding to the persistence point within the EH
            ActivityUnit actUnit = (ActivityUnit) rEH.createOnAlarmUnitForRecovery(onAlarm, onAlarmDBO.getId());
            updateState(actUnit, onAlarmDBO);
            
            RActivity childScope = ((RActivityHolder)actUnit.getStaticModelActivity()).getChildActivity();
            //create and traverse the child scope unit since the LCP is somewhere inside the scope
            createChildUnit(childScope, actUnit.getBranchId(), actUnit, new UnSynchronizedStack(), info, true);
            
        } else {
            //createOnAlarm
            Timestamp ts = onAlarmDBO.getTimerValue();
            long waitTime = ts.getTime();
            long repeatEveryValue = onAlarmDBO.getRepeatEveryVal();
            ICallFrame onAlrmFrame = rEH.createFrameForRecovery(onAlarm, 
                    onAlarmDBO.getId(), 
                    waitTime, repeatEveryValue);
            updateState(onAlrmFrame.getProgramCounter(), onAlarmDBO);
            info.addUninitializedEHCallFrames(onAlrmFrame);
        }
    }
    
    private void createCallFrame(RecoveredState info, 
                                 RActivity act, 
                                 UnSynchronizedStack stack, 
                                 LastCheckPointDBO lcpdbo,
                                 ActivityUnit actUnit) {
        // activity should be one of these 
        // Receive, Reply, Invoke, Wait, Pick (OnAlarm, OnMessage)
        // for the cases of pick (OnAlarm) and wait.
        // TODO should this Timer value be exposed by the callframe interface?
        long timerVal = Long.MIN_VALUE;
        // recover the value of the branch invoke counter for the persisted branch
        // needed for the custom reliable messaging InOut message exchange 
        long branchInvokeCounter = lcpdbo.getBranchInvokeCounter();        

        if (Utility.checkClassType(act, Wait.class)) {
            Timestamp ts = lcpdbo.getTimerValue();

            if (ts != null) {
                timerVal = ts.getTime();
            }
        } 
        else if (Utility.checkClassType(act, Pick.class)) {
            Timestamp ts = lcpdbo.getTimerValue();
            long pickCompositeActId = lcpdbo.getPickCompositeActId(); 
            if (ts != null) {
                if (pickCompositeActId == LastCheckPointDBO.DEFAULT_PICK_COMPOSITE_ACT_ID) {
                    LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6066: invalid state for - failed to recover pick " + 
                    		"of bpId {0} and activity Id {1}", lcpdbo.getBPId(), new Long(lcpdbo.getActivityId())));
                }

                timerVal = ts.getTime();
            }

            actUnit = ActivityUnitFactory.getInstance()
                        .createPickRecoveredUnit(actUnit.getContext(),
                                                 (PickUnitImpl) actUnit, 
                                                 pickCompositeActId, 
                                                 timerVal);
        }

        createCF(info, stack, timerVal, actUnit, branchInvokeCounter);
    }

    /**
     * DOCUMENT ME!
     *
     * @param holder convenient data structure to pass around the variables and holds lists of
     *        created callframes.
     * @param act activity to be asociated with the callframe
     * @param stack useful for the creation of the Flow activity associated callframes.
     * @param timerVal this is the value applicable for timer based activities, Wait, Pick
     *        (OnAlarm)
     * @param actUnit DOCUMENT ME!
     *
     * @return
     */
    private ICallFrame createCF(RecoveredState info, 
                                UnSynchronizedStack stack, 
                                long timerVal,
                                ActivityUnit actUnit,
                                long branchInvokeCounter) {
        ICallFrame parentCF = null;
        DS ds = stack.peek();

        if (ds != null) {
            //when it is flow create parent call frames
            //parentCF = createCFRecursive(info, stack, branchInvokeCounter);
        	/** FIX IN TEST*/
    		parentCF = createCFRecursive(info, stack);
        }

    	ICallFrame retCF = createCallFrameOnRecovery(parentCF, timerVal, actUnit, info, branchInvokeCounter, false, true);
        info.addCallFrame(retCF);
        return retCF;
    }

    /**
     * All the call frames that are created in this method will be only "Flow" based callframes.
     *
     * @param holder
     * @param stack
     *
     * @return
     */
    private ICallFrame createCFRecursive(RecoveredState info, UnSynchronizedStack stack) {
        DS ds = (DS) stack.pop();
        if (ds == null) return null;

        if (ds.mFrame == null) {
        	ICallFrame parentCF = createCFRecursive(info, stack);
            
        	long actId = ds.mActUnit.getStaticModelActivity().getUniqueId();
			LastCheckPointDBO lcpdbo = info.findCheckpoint(actId);

			//if CF existed in the database
			boolean updatePC = (lcpdbo != null) ? true : false;
			long branchInvokeCounter = (lcpdbo != null) ? lcpdbo.getBranchInvokeCounter() : 0L;   

            ds.mFrame = createCallFrameOnRecovery(parentCF, Long.MIN_VALUE,
					ds.mActUnit, info, branchInvokeCounter, true, updatePC);
            info.addCallFrame(ds.mFrame);
        }
        stack.push(ds);

        return ds.mFrame;
    }

    private List<ICallFrame> uninitiatedFlowBranchCallFrames(RecoveredState info) {
        List<ICallFrame> flowCFs = info.getParentCFsWithCheckPointedChildCFs();
        ICallFrame flowCF = null;
        List<ICallFrame> initiatedChildBranchs = null;
        List<ICallFrame> unInitiatedChildBranches = null;
        List<ICallFrame> retVal = new ArrayList<ICallFrame>();

        for (int i = 0, size = flowCFs.size(); i < size; i++) {
            flowCF = (ICallFrame) flowCFs.get(i);
            initiatedChildBranchs = info.getInitiatedChildBranches(flowCF); 
            unInitiatedChildBranches = recoverFlow(flowCF, initiatedChildBranchs, info);
            retVal.addAll(unInitiatedChildBranches);
        }

        return retVal;
    }

    private List<ICallFrame> createUninitiatedCFs(ICallFrame parentCF, 
                                      List<ICallFrame> initiatedChildBranchs, 
                                      RecoveredState info) {
        Flow flow = (Flow) parentCF.getPC();
        return createUninitiatedCFs(flow, parentCF, initiatedChildBranchs, info);
    }
    
    private List<ICallFrame> createUninitiatedCFs(Flow flow,
                                      ICallFrame parentCF, 
                                      List<ICallFrame> initiatedChildBranchs, 
                                      RecoveredState info) {
        ActivityUnit flowUnit = parentCF.getProgramCounter();
        Collection allChildPaths = flow.getActivities();
        List<ICallFrame> needToExecuteChildPaths = new ArrayList<ICallFrame>();
        String activityPath;
        ICallFrame unInitiatedChildCF;
        ICallFrame initiatedChildCF;
        String pcVal;
        RActivity activity = null;

        for (Iterator allChildPathsItr = allChildPaths.iterator(); allChildPathsItr.hasNext();) {
            activity = (RActivity) allChildPathsItr.next();
            activityPath = activity.getXPath();

            boolean found = false;
            for (int j = 0, size = initiatedChildBranchs.size(); j < size; j++) {
                initiatedChildCF = initiatedChildBranchs.get(j);
                pcVal = initiatedChildCF.getPC().getXPath();

                if (pcVal.startsWith(activityPath)) {
                    found = true;
                    break;
                }
            }

            if (found) continue;

            ActivityUnit actUnit = ActivityUnitFactory.getInstance().createActivityUnit(
                    flowUnit.getContext(), null, activity, activity.getUniqueId());
            unInitiatedChildCF = info.getInterpreter().createCallFrame(parentCF, actUnit, flowUnit.getContext());
            actUnit.getContext().getFaultHandlingContext().registerCallFrame(unInitiatedChildCF);
            needToExecuteChildPaths.add(unInitiatedChildCF);
        }

        return needToExecuteChildPaths;
    }

    private List<ICallFrame> recoverFlow(ICallFrame frame, List<ICallFrame> initiatedChildBranchs, RecoveredState info) {
        Flow flow = (Flow) frame.getPC();
        int totalChildBranches = flow.getActivities().size();
        List<ICallFrame> retVal = new ArrayList<ICallFrame>();

        if (totalChildBranches == initiatedChildBranchs.size()) {
            return retVal;
        }

        if (totalChildBranches < initiatedChildBranchs.size()) {
            LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6067: something is seriously wrong, total branches cannot " + 
            		"be less than persisted number of branches for a given flow")); //$NON-NLS-1$
        }

        // finding out if there are any children paths that didn't ever get to 
        // persist their presence
        return createUninitiatedCFs(frame, initiatedChildBranchs, info);
    }

    private static class UnSynchronizedStack extends ArrayList {
        /**
         * Creates a new UnSynchronizedStack object.
         */
        public UnSynchronizedStack() {
        }

        /**
         * DOCUMENT ME!
         *
         * @param item DOCUMENT ME!
         *
         * @return DOCUMENT ME!
         */
        public DS push(DS item) {
            add(item);

            return item;
        }

        /**
         * DOCUMENT ME!
         *
         * @return DOCUMENT ME!
         */
        public DS pop() {
            int len = size();

            if (len == 0) {
                return null;
            }

            return (DS) remove(len - 1);
        }

        /**
         * DOCUMENT ME!
         *
         * @return DOCUMENT ME!
         */
        public DS peek() {
            int len = size();

            if (len == 0) {
                return null;
            }

            return (DS) get(len - 1);
        }

        /**
         * DOCUMENT ME!
         *
         * @return DOCUMENT ME!
         */
        public boolean empty() {
            return size() == 0;
        }
    }

    /**
     * DS is a structure to keep track of the flows and a callframe that has Flow as it's current
     * execution activity.
     */
    private static class DS {
        /** DOCUMENT ME! */
        Flow mAct;

        /** DOCUMENT ME! */
        ICallFrame mFrame;

        /** DOCUMENT ME! */
        ActivityUnit mActUnit;

        /**
         * Creates a new DS object.
         *
         * @param act DOCUMENT ME!
         * @param actUnit DOCUMENT ME!
         */
        DS(Flow act, ActivityUnit actUnit) {
            mAct = act;
            mActUnit = actUnit;
        }
    }

    /**
     * @param parent
     * @param timerVal
     * @param actUnit
     * @param holder
     * @return
     */
    private ICallFrame createCallFrameOnRecovery(ICallFrame parent,
                                                 long timerVal,
                                                 ActivityUnit actUnit,
                                                 RecoveredState info,
                                                 long branchInvokeCounter, 
                                                 boolean hasChildern, 
                                                 boolean updatePC) {
        BPELProcessInstance instance = info.getProcessInstance();
        CallFrame frame = (parent != null) 
                ? new RecoveredCallFrameImpl(parent, info.getInterpreter(), 
                                             timerVal, actUnit, branchInvokeCounter)
                : new RecoveredCallFrameImpl(info.getProcessDef(), 
                                             info.getInterpreter(), 
                                             timerVal, instance, actUnit, branchInvokeCounter);
                
        instance.addCallFrame(frame);
        actUnit.getContext().getFaultHandlingContext().registerCallFrame(frame);
        MutableState state = actUnit.getContext().getStateContext().getState();

        if(updatePC) {
        	state.updatePC(actUnit.getBranchId(), actUnit.getStaticModelActivity().getUniqueId());
        	((State) state).clearPC();
        }
        
        if(hasChildern){
            ((RecoveredCallFrame) frame).setHasPersistedChildCF(true);
            registerFlowBranchesWithState(state, frame);
        }
        
        return frame;
    }

    private void registerFlowBranchesWithState(MutableState state, CallFrame parentCF) {
        List branchIds = new ArrayList();
        RActivity childAct = ((RActivityHolder) parentCF.getPC()).getChildActivity();
        while (childAct != null) {
            long branchId = childAct.getUniqueId();
            branchIds.add(new Long(branchId));
            childAct = childAct.getNextActivity();
        }
        state.enterFlow((parentCF.getPC()).getUniqueId(), branchIds);
    }
}
