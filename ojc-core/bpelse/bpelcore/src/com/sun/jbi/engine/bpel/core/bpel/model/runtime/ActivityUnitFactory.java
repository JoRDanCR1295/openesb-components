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
 * @(#)ActivityUnitFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime;

import java.util.Collection;
import java.util.Iterator;

import javax.xml.namespace.QName;

import com.sun.bpel.model.Activity;
import com.sun.bpel.model.Assign;
import com.sun.bpel.model.Catch;
import com.sun.bpel.model.CatchAll;
import com.sun.bpel.model.Compensate;
import com.sun.bpel.model.CompensateScope;
import com.sun.bpel.model.Empty;
import com.sun.bpel.model.FaultHandlers;
import com.sun.bpel.model.Flow;
import com.sun.bpel.model.ForEach;
import com.sun.bpel.model.If;
import com.sun.bpel.model.Invoke;
import com.sun.bpel.model.Pick;
import com.sun.bpel.model.Receive;
import com.sun.bpel.model.RepeatUntil;
import com.sun.bpel.model.Reply;
import com.sun.bpel.model.Rethrow;
import com.sun.bpel.model.Scope;
import com.sun.bpel.model.Sequence;
import com.sun.bpel.model.Terminate;
import com.sun.bpel.model.Throw;
import com.sun.bpel.model.Validate;
import com.sun.bpel.model.Wait;
import com.sun.bpel.model.While;
import com.sun.bpel.model.meta.RActivity;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.JBIMessageImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.AssignUnitImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.CatchAllUnitImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.CatchUnitImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.CompensateScopeUnitImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.CompensateUnitImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.EmptyUnitImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.ExitUnitImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.FlowUnitImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.ForEachUnitSerialImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.IfUnitImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.InvokeUnitImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.PickUnitImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.ReceiveUnitImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.RepeatUntilUnitImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.ReplyUnitImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.RethrowUnitImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.ScopeUnitImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.SequenceUnitImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.VirtualThrowUnitImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.ThrowUnitImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.ValidateUnitImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.WaitUnitImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.WhileUnitImpl;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;

/**
 * Activity unit factory
 * 
 * @author Sun Microsystems
 */
public class ActivityUnitFactory {

    private static final ActivityUnitFactory SINGLETON = new ActivityUnitFactory();

    private ActivityUnitFactory() {
    }

    /**
     * gets singleton instance
     * 
     * @return ActivityUnitFactory activity unit factory
     */
    public static ActivityUnitFactory getInstance() {
        return SINGLETON;
    }

    /**
     * creatse activity unit
     * 
     * @param parentActUnit parent activity unit
     * @param act activity
     * @param branchId branch ID
     * @return ActivityUnit activity unit
     * @throws RuntimeException DOCUMENT ME!
     */
    public ActivityUnit createActivityUnit(Context context,
                                           Unit parentActUnit,
                                           RActivity act,
                                           long branchId) {
        if (act instanceof While) {
            return new WhileUnitImpl(context, parentActUnit, act, branchId);
        } else if (act instanceof RepeatUntil) {
            return new RepeatUntilUnitImpl(context, parentActUnit, act, branchId);
        } else if (act instanceof Receive) {
            return new ReceiveUnitImpl(context, parentActUnit, act, branchId);
        } else if (act instanceof Invoke) {
            return new InvokeUnitImpl(context, parentActUnit, act, branchId);
        } else if (act instanceof Reply) {
            return new ReplyUnitImpl(context, parentActUnit, act, branchId);
        } else if (act instanceof Sequence) {
            return new SequenceUnitImpl(context, parentActUnit, act, branchId);
        } else if (act instanceof Assign) {
            return new AssignUnitImpl(context, parentActUnit, act, branchId);
        } else if (act instanceof Throw) {
            return new ThrowUnitImpl(context, parentActUnit, act, branchId);
        } else if (act instanceof Wait) {
            return new WaitUnitImpl(context, parentActUnit, act, branchId);
        } else if (act instanceof Empty) {
            return new EmptyUnitImpl(context, parentActUnit, act, branchId);
        } else if (act instanceof Scope) {
            return new ScopeUnitImpl(context, parentActUnit, act, branchId);
        } else if (act instanceof Flow) {
            return new FlowUnitImpl(context, parentActUnit, act, branchId);
        } else if (act instanceof Terminate) {
            return new ExitUnitImpl(context, parentActUnit, act, branchId);
        } else if (act instanceof If) {
            return new IfUnitImpl(context, parentActUnit, act, branchId);
        } else if (act instanceof Pick) {
            return new PickUnitImpl(context, parentActUnit, act, branchId);
        } else if (act instanceof ForEach) {
            ForEach lForEach = (ForEach) act;
            if (lForEach.getParallel().equals("yes")) {
                // @toDo: create a ForEachUnitParallelImpl instead
                return new ForEachUnitSerialImpl(context, parentActUnit, act, branchId);
            } else {
                return new ForEachUnitSerialImpl(context, parentActUnit, act, branchId);
            }
        } else if (act instanceof Rethrow) {
            return new RethrowUnitImpl(context, parentActUnit, act, branchId);
        } else if (act instanceof Compensate) {
        	return new CompensateUnitImpl(context, parentActUnit, act, branchId, false);
        } else if (act instanceof CompensateScope) {
        	return new CompensateScopeUnitImpl(context, parentActUnit, act, branchId);
        } else if (act instanceof Validate) {
            return new ValidateUnitImpl(context, parentActUnit, act, branchId);
        }

        throw new RuntimeException(I18n.loc("BPCOR-6045: unknown activity type: {0}", ((Activity) act).getName())); //$NON-NLS-1$

    }

    /**
     * creats Pick recovered unit
     * 
     * @param unit pick unit
     * @param pickCompositeActId pick composite ID
     * @param timerVal timer value
     * @return ActivityUnit Pick recovered unit
     */
    public ActivityUnit createPickRecoveredUnit(Context context,
                                                PickUnitImpl unit,
                                                long pickCompositeActId,
                                                long timerVal) {
        return new PickUnitImpl(context, (PickUnitImpl) unit, pickCompositeActId, timerVal);
    }

    /**
     * creasts synth. throw activity unit
     * @param parentActUnit parent activity unit
     * @param act activity
     * @param branchId branch ID
     * @param actualUnit actual Unit that caused the fault.
     * @param fault TODO
     * @return ActivityUnit synth. throw activity unit
     */
    public ActivityUnit createVirtualThrowUnit(Context context,
                                             Unit parentActUnit,
                                             RActivity act,
                                             long branchId,
                                             ActivityUnit actualUnit,
                                             boolean updateState,
                                             Fault fault) {
        return new VirtualThrowUnitImpl(context, parentActUnit, act, branchId, actualUnit,
                updateState, fault);
    }

    /**
     * Tries to find a suitable catch or catchAll in a scope or a process and if one is found
     * returns a corresponding CatchUnit or CatchAllUnit
     * @param fault TODO
     * @param faultHandlers scope in which the current fault handler is enclosed in not the
     *            one it is associated with.
     * 
     * @return Matching CatchUnit or CatchAllUnit
     */
    public Unit createFaultHandlingUnit(Context context, Fault fault, FaultHandlers faultHandlers) {
        
        Unit faultHandlingUnit = null;
        
        if (faultHandlers == null) {
            return null;
        }

        /*
         * Because of the flexibility allowed in expressing the faults that a catch activity can
         * handle, it is possible for a fault to match more than one fault handler. In the case of
         * faults thrown without associated data the fault will be caught as follows: 1. If there is
         * a catch activity with a matching faultName value that does not specify a faultVariable
         * attribute then the fault is passed to the identified catch activity. 2. Otherwise if
         * there is a catchAll handler then the fault is passed to the catchAll handler. 3.
         * Otherwise, the fault will be handled by the default fault handler. In the case of faults
         * thrown with associated data the fault will be caught as follows: 1. If there is a catch
         * activity with a matching faultName value that has a faultVariable whose type matches the
         * type of the fault data then the fault is passed to the identified catch activity. 2.
         * Otherwise if the fault data is a WSDL message type where the message contains a single
         * part defined by an element and there exists a catch activity with a matching faultName
         * value that has a faultVariable whose type matches the type of the element used to define
         * the part then the fault is passed to the identified catch activity with the faultVariable
         * initialized to the value in the single part's element. 3. Otherwise if there is a catch
         * activity with a matching faultName value that does not specify a faultVariable or
         * faultMessageType value then the fault is passed to the identified catch activity. Note
         * that in this case the fault value will not be available from within the fault handler but
         * will be available to the "rethrow" activity. 4. Otherwise if there is a catch activity
         * without a faultName attribute that has a faultVariable whose type matches the type of the
         * fault data then the fault is passed to the identified catch activity. 5. Otherwise if the
         * fault data is a WSDL message type where the message contains a single part defined by an
         * element and there exists a catch activity without a faultName attribute that has a
         * faultVariable whose type matches the type of the element used to define the part then the
         * fault is passed to the identified catch activity with the faultVariable initialized to
         * the value in the single part's element. 6. Otherwise if there is a catchAll handler then
         * the fault is passed to the catchAll handler. 7. Otherwise, the fault will be handled by
         * the default fault handler. If the fault occurs in (or is rethrown to) the global process
         * scope, and there is no matching fault handler for the fault at the global level, the
         * process terminates abnormally, as though an exit activity had been performed. See
         * Implicit Fault and Compensation Handlers for a more complete description of the default
         * fault and compensation handling behavior.
         */

        // TODO: Throw exception if the fault name is null, we can put this
        // in the throw activity itself.
        Catch matchingCatchAct = null;

        Collection catches = faultHandlers.getCatches();

        if ((catches != null) && !catches.isEmpty()) {
            QName faultName = fault.getName();
            WSMessage faultData = fault.getData();
            if (faultData == null) {
                // Case 1: Only fault name is specified
                matchingCatchAct = findMatchingCatch(catches, faultName);
            } else {
                // Case 2: Fault name as well as fault data is specified
                matchingCatchAct = findMatchingCatch(catches, faultName, faultData);
            }
        }

        if (matchingCatchAct != null) {
            faultHandlingUnit = new CatchUnitImpl(context, matchingCatchAct, fault);
        } else {
            CatchAll catchAll = faultHandlers.getCatchAll();

            if (catchAll != null) {
                faultHandlingUnit = new CatchAllUnitImpl(context, catchAll, fault);
            }
        }

        return faultHandlingUnit;
    }

    /**
     * Finds a catch matching the faultName. 1. If there is a catch activity with a matching
     * faultName value that does not specify a faultVariable attribute then the fault is passed to
     * the identified catch activity. Here we are implemeting 1
     * 
     * @param catches Collection of catch activities
     * @param faultName The faultName to match
     * @return The matching catch activity
     */
    private Catch findMatchingCatch(Collection catches, QName faultName) {
        Iterator catchIter = catches.iterator();

        for (; catchIter.hasNext();) {
            Catch catchAct = (Catch) catchIter.next();
            QName catchActFaultCustomQName = catchAct.getFaultName();
            QName catchActFaultMessageTypeCustomQName = catchAct.getFaultMessageType();

            // Check if the catch has a faultName defined on it
            if (catchActFaultCustomQName == null) {
                continue;
            }

            // Continue if a faultVariable is also specified
            if (catchActFaultMessageTypeCustomQName != null) {
                continue;
            }

            if (catchActFaultCustomQName.equals(faultName)) {
                return catchAct;
            }
        }

        return null;
    }

    /**
     * Finds a catch matching the faultName and the faultData 1. If there is a catch activity with a
     * matching faultName value that has a faultVariable whose type matches the type of the fault
     * data then the fault is passed to the identified catch activity. 2. Otherwise if the fault
     * data is a WSDL message type where the message contains a single part defined by an element
     * and there exists a catch activity with a matching faultName value that has a faultVariable
     * whose type matches the type of the element used to define the part then the fault is passed
     * to the identified catch activity with the faultVariable initialized to the value in the
     * single part's element. 3. Otherwise if there is a catch activity with a matching faultName
     * value that does not specify a faultVariable or faultMessageType value then the fault is
     * passed to the identified catch activity. Note that in this case the fault value will not be
     * available from within the fault handler but will be available to the "rethrow" activity. 4.
     * Otherwise if there is a catch activity without a faultName attribute that has a faultVariable
     * whose type matches the type of the fault data then the fault is passed to the identified
     * catch activity. 5. Otherwise if the fault data is a WSDL message type where the message
     * contains a single part defined by an element and there exists a catch activity without a
     * faultName attribute that has a faultVariable whose type matches the type of the element used
     * to define the part then the fault is passed to the identified catch activity with the
     * faultVariable initialized to the value in the single part's element.
     * 
     * @param catches Collection of catch activities
     * @param faultName The faultName to match
     * @param faultData The faultData to match
     * @return The matching catch activity
     * @throws RuntimeException Thrown if an error occurrs while querying the faultData for its
     *             messageType
     */
    private Catch findMatchingCatch(Collection catches, QName faultName, WSMessage faultData) {
        Catch catchNameAndData = null;
        Catch catchName = null;
        Catch catchData = null;

        QName faultDataMessageType = null;

        faultDataMessageType = ((JBIMessageImpl) faultData).getMessageType();
        Iterator catchIter = catches.iterator();

        for (; catchIter.hasNext();) {
            Catch catchAct = (Catch) catchIter.next();
            QName catchActFaultName = catchAct.getFaultName();
            QName catchActFaultMessageType = catchAct.getFaultMessageType();

            if (faultName.equals(catchActFaultName)
                    && faultDataMessageType.equals(catchActFaultMessageType)) {
                catchNameAndData = catchAct;

                break;
            }

            if ((catchActFaultMessageType == null) && faultName.equals(catchActFaultName)) {
                if ((catchName == null)) {
                    catchName = catchAct;
                }
            }

            if ((catchActFaultName == null)
                    && faultDataMessageType.equals(catchActFaultMessageType)) {
                if (catchData == null) {
                    catchData = catchAct;
                }
            }
        }

        if (catchNameAndData != null) {
            return catchNameAndData;
        } else if (catchName != null) {
            return catchName;
        } else if (catchData != null) {
            return catchData;
        } else {
            return null;
        }
    }
}
