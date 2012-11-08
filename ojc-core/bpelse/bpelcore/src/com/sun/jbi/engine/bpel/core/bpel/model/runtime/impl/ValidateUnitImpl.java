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
package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import java.util.Collection;
import java.util.Iterator;
import java.util.logging.Logger;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RVariable;
import com.sun.bpel.model.meta.impl.RValidateImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.exception.StandardException;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit;
import com.sun.jbi.engine.bpel.core.bpel.trace.BPELTraceManager;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;
import com.sun.jbi.engine.bpel.core.bpel.util.ValidationResult;
import com.sun.jbi.engine.bpel.core.bpel.util.DOMHelper;
import com.sun.jbi.engine.bpel.core.bpel.util.Validator;
import com.sun.jbi.engine.bpel.core.bpel.util.XmlBeansValidator;
import java.util.Set;
import java.util.HashSet;
import org.w3c.dom.Element;

/**
 * Validate Unit implementation
 *
 * @author Vitaly Bychkov
 */
public class ValidateUnitImpl  extends ActivityUnitImpl {
    private static final Logger LOGGER = Logger.getLogger(ValidateUnitImpl.class.getName());
    private static final String CLASSIDENTIFIER_FORMEASUREMENT = "BPEL-SE.ValidateUnitImpl";

    /**
     * Creates a new ValidateUnitImpl object.
     *
     * @param parentActUnit parent activity unit
     * @param act activity
     * @param branchId branch ID
     */
    public ValidateUnitImpl(Context context, Unit parentActUnit, RActivity act, long branchId) {
        super(context, parentActUnit, act, branchId);
    }

    /**
     * @see ValidateUnitImpl#doAction(com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame,
     *      com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread,
     *      com.sun.jbi.engine.bpel.core.bpel.model.RequiredObjects)
     */
    @Override
    public boolean doAction(ICallFrame frame, BusinessProcessInstanceThread bpit, 
            RequiredObjects rObjs) throws Exception
    {
        frame.setProgramCounter(this);
        frame.onLineChange(this);
        BPELTraceManager.getInstance().doTraceOnStart(mAct, mContext, frame.getProcessInstance());
        frame.getProcessInstance().getMonitorMgr().postActivityStartEvent(this);

        RValidateImpl validate = (RValidateImpl) mAct;

        //
        if (rObjs.getEngine().isValidationEnabled()) {
            validate(validate.getRVariables());
        }
        // update the bpel instance with the variable got updated


        BPELTraceManager.getInstance().doTraceOnComplete(mAct, mContext, frame.getProcessInstance());
        frame.onActivityComplete(this);
        frame.getProcessInstance().getMonitorMgr().postActivityCompleteEvent(this);

        return true;
    }

    private Validator getValidator() {
        return XmlBeansValidator.getInstance();
    }

    private void validate(Set<RVariable> rVariables) {
        if (rVariables == null) {
            return;
        }

        Set<RuntimeVariable> runtimeVariables = new HashSet<RuntimeVariable>();
        Iterator<RVariable> iter = rVariables.iterator();
        while (iter.hasNext()) {
            RuntimeVariable tmpVar = mContext.getRuntimeVariable(iter.next());
            if (tmpVar != null) {
                runtimeVariables.add(tmpVar);
            }
        }


        ValidationResult<RuntimeVariable> valResult = getValidator().validate(runtimeVariables);
        if (valResult != null) {
                StringBuilder error = new StringBuilder();
                Collection<Object> errors = valResult.getErrors();
                for (Iterator it = errors.iterator(); it.hasNext();) {
                    String localError = "" + it.next();
                    error.append(localError).append(System.getProperty("line.separator"));
                }

                Object resultObject = valResult.getValue();

                // TODO set correct BPCOR-number
                String error2throw = I18n.loc(
                        "BPCOR-7135: Invalid variable value \n{0}\nis in {1} in BP instance ({2}) at line {3} BP {4}. Error Summary: {5}",
                        resultObject instanceof Element ? DOMHelper.createXmlString((Element) resultObject) : resultObject, valResult.getSource().getVariableDef().getName(),
                        mContext.getProcessInstance().getId(), mAct.getLocator().getLineNumber(),
                        mContext.getProcessInstance().getBPELProcessManager().getBPELProcess().getBPELId(), error.toString());
                throw new StandardException(StandardException.Fault.InvalidVariables, error2throw);
        }
    }

}
