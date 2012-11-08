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
 * @(#)$Id: ChangeVariableHandler.java,v 1.3 2010/02/15 19:24:08 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.process.notification;

import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

import org.w3c.dom.Element;

import com.sun.jbi.engine.workflow.WorkflowException;
import com.sun.jbi.engine.workflow.common.model.TaskModelFactory;
import com.sun.jbi.engine.workflow.common.model.TaskOutput;
import com.sun.jbi.engine.workflow.process.Handler;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTask;
import com.sun.jbi.engine.workflow.runtime.model.TaskException;
import com.sun.jbi.engine.workflow.runtime.model.TaskManager;
import com.sun.jbi.engine.workflow.runtime.model.TaskStateEvent;
import com.sun.jbi.engine.workflow.runtime.model.TaskStateListener;
import com.sun.jbi.engine.workflow.runtime.model.VariableCopy;
import com.sun.jbi.engine.workflow.runtime.model.impl.CopyUnitImpl;
import com.sun.jbi.engine.workflow.util.I18n;
import com.sun.jbi.workflow.model.Action;
import com.sun.jbi.workflow.model.ChangeVariables;
import com.sun.jbi.workflow.model.Copy;
import com.sun.jbi.workflow.model.RuntimeVariables;
import com.sun.jbi.workflow.model.Task;
import com.sun.jbi.workflow.model.xmlbeans.TActionType;

public class ChangeVariableHandler implements Handler, TaskStateListener {

    private static final Logger LOGGER = Logger
            .getLogger(ChangeVariableHandler.class.getName());

    private RuntimeTask.TaskState mNewState;

    private TaskManager mTaskManager;
    
    private List<ChangeVariables> mChangeVariables;    

    private QName mTaskQName;

    public ChangeVariableHandler(List<ChangeVariables> changeVariables, RuntimeTask.TaskState newState,
            TaskManager taskManager) {
        this.mNewState = newState;
        this.mTaskManager = taskManager;
        this.mChangeVariables = changeVariables;
        this.mTaskQName = changeVariables.get(0).getTask().getQName();
    }

    public void execute() throws TaskException {
        // TODO Auto-generated method stub
        this.mTaskManager.addTaskStateListener(this);
    }

    public QName getTaskDefName() {
        // TODO Auto-generated method stub
        return mTaskQName;
    }

    public void onStateChange(TaskStateEvent evt) {
        // TODO Auto-generated method stub
        RuntimeTask.TaskState nState = evt.getNewState();
        if (nState == this.mNewState) {
            LOGGER.log(Level.INFO, I18n.loc(
                    "WLM-5030: Change Variable on status: {0}", nState));
            // this.mTaskManager.removeTaskStateListener(this);
            RuntimeTask task = evt.getTask();
            Task taskMeta = task.getTaskMeta();    
            if (mChangeVariables != null && mChangeVariables.size() > 0) {
                for (int i = 0; i < mChangeVariables.size(); i++) {
                    ChangeVariables varInit = mChangeVariables.get(i);
                    List<Copy> copies = varInit.getCopies();
                    if (copies != null && copies.size() > 0) {
                        for (int j = 0; j < copies.size(); j++) {
                            VariableCopy varCopy = new CopyUnitImpl(copies
                                    .get(j), task.getJXpathContext());
                            try {
                                varCopy.doCopy();
                            } catch (Exception e) {
                                // TODO Auto-generated catch block
                                throw new TaskException(e);
                            }
                        }
                    }

                }
            }
            TaskOutput taskOutput = task.getOutput();
            Object outputObj = task.getRuntimeVariables().getVariable(
                    RuntimeVariables.TASK_OUTPUT_VAR);
            if (outputObj != null) {
                try {
                    taskOutput = TaskModelFactory.getInstance()
                            .createTaskOutput((Element) outputObj);
                } catch (WorkflowException e) {
                    // TODO Auto-generated catch block
                    throw new TaskException(e);
                }
                task.setOutput(taskOutput);
                mTaskManager.setTaskOutput(task.getId(),
                        taskOutput.getOutput(), TaskModelFactory.getInstance()
                                .createDefaultPrincipal());
            }
        }
    }

    private TActionType.Enum convertStatus(RuntimeTask.TaskState state) {
        String stateString = state.getDescription();
        return TActionType.Enum.forString(stateString);
    }
}
