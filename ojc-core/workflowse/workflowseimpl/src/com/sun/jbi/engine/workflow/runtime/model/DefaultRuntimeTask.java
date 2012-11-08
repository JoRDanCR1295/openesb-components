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
 * @(#)DefaultRuntimeTask.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.runtime.model;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.wsdl.Message;

import org.apache.commons.jxpath.JXPathContext;
import org.w3c.dom.Element;

import com.sun.jbi.engine.workflow.EngineContext;
import com.sun.jbi.engine.workflow.common.model.TaskInput;
import com.sun.jbi.engine.workflow.common.model.TaskModelFactory;
import com.sun.jbi.engine.workflow.common.model.TaskOutput;
import com.sun.jbi.engine.workflow.common.model.TaskPrincipal;
import com.sun.jbi.engine.workflow.process.LDAPConfig;
import com.sun.jbi.engine.workflow.runtime.model.impl.CopyUnitImpl;
import com.sun.jbi.engine.workflow.runtime.model.impl.RuntimeVariablesImpl;
import com.sun.jbi.engine.workflow.util.I18n;
import com.sun.jbi.engine.workflow.util.Util;
import com.sun.jbi.engine.workflow.util.XPathUtil;
import com.sun.jbi.engine.workflow.xpath.ExpressionInfo;
import com.sun.jbi.workflow.model.ChangeVariables;
import com.sun.jbi.workflow.model.Copy;
import com.sun.jbi.workflow.model.Init;
import com.sun.jbi.workflow.model.Keyword;
import com.sun.jbi.workflow.model.RuntimeVariables;
import com.sun.jbi.workflow.model.Task;

/**
 * 
 * 
 */
public class DefaultRuntimeTask implements RuntimeTask {

	private Logger mLogger = Logger.getLogger(DefaultRuntimeTask.class
			.getName());
    
     private static final String TASK_OUTPUT_VAR = "TaskOutput";

	private Long mId;

	private TaskInput mInput;

	private TaskOutput mOutput;

	private Task mTaskMetaData;

	private TaskState mState;

	private int mPriority;

	private String mDescription;

	private Calendar mCreationDate;
    
    private Calendar mEndDate;
    
    private Calendar mDeadline;

	private TaskPrincipal mClaimedByPrincipal;

	private TaskPrincipal mCompletedByPrincipal;

	private Collection<TaskPrincipal> mAssignedToPrincipals = new ArrayList<TaskPrincipal>();

	private TaskManager mManager;

	private String mExchangeId;

	private boolean mNew;

	private Set<RuntimeTaskTimer> mTimers = new HashSet();

	private RuntimeVariables mRuntimeVariables;
    
    private JXPathContext mJxpathContext;
    
    private LDAPConfig mLDAPConfig;
    
    private String mKeywords;
    
    private boolean mCompleted = false;
    
    private static  final String KEYWORD_S ="[";
    private static  final String KEYWORD_E ="]";  


	/** ReCreates an instance of DefaultRuntimeTask from DB */
	public DefaultRuntimeTask(Long id, String exchangeId, TaskInput input,
			Task taskMetaData) {
		this.mId = id;
		this.mInput = input;
		this.mTaskMetaData = taskMetaData;
		// String title = taskMetaData.getName();

		// this.mDescription = "This is a task for " + title;
		this.mCreationDate = Calendar.getInstance();
		this.mState = TaskState.UNASSIGNED;
		// this.mPriority = TaskPriority.MEDIUM;
		this.mExchangeId = exchangeId;
		this.mNew = true;

		try {
			this.mManager = TaskManagerFactory.getInstance().getTaskManager();
		} catch (Exception ex) {
			mLogger.log(Level.SEVERE, "Failed to get TaskManager ", ex);
		}
	}

	// /** Creates a new instance of DefaultRuntimeTask */
	// public DefaultRuntimeTask(Long id, String exchangeId, Element input,
	// Task taskMetaData) {
	// this.mId = id;
	// this.mWrapperEl = input;
	// this.mTaskMetaData = taskMetaData;
	// String title = taskMetaData.getName();
	//
	// this.mDescription = "This is a task for " + title;
	// this.mCreationDate = Calendar.getInstance();
	// this.mState = TaskState.UNASSIGNED;
	// this.mPriority = TaskPriority.MEDIUM;
	// this.mExchangeId = exchangeId;
	// this.mNew = true;
	//
	// try {
	// this.mManager = TaskManagerFactory.getInstance().getTaskManager();
	// } catch (Exception ex) {
	// mLogger.log(Level.SEVERE, "Failed to get TaskManager ", ex);
	// }
	// }

	public void execute() throws TaskException {
		try {
			Collection<TaskPrincipal> newPrincipals = TaskManagerHelper
					.getTaskPrincipalsFromWorkflowDefinition(this);
            Collection<TaskPrincipal> excludedPrincipals = TaskManagerHelper
            .getExcludedTaskPrincipalsFromWorkflowDefinition(this);
            mManager.assignTask(this, newPrincipals, excludedPrincipals);
		} catch (Exception ex) {
			throw new TaskException(ex);
		}
	}

	public Long getId() {
		return this.mId;
	}

	public TaskInput getInput() {
		return this.mInput;
	}

	public TaskOutput getOutput() {
		return this.mOutput;
	}

	public synchronized void setOutput(TaskOutput output) {
		this.mOutput = output;
	}

	public TaskState getState() {
		return mState;
	}

	public synchronized void setState(TaskState state) {
		this.mState = state;
	}

	public Collection<TaskPrincipal> getAssignedTo() {
		return this.mAssignedToPrincipals;
	}

	public synchronized void setAssignedTo(Collection<TaskPrincipal> users) {
		mAssignedToPrincipals = users;
		this.mState = TaskState.ASSIGNED;
	}

	public synchronized void addAssignedTo(TaskPrincipal user) {
		if (this.mAssignedToPrincipals == null) {
			this.mAssignedToPrincipals = new ArrayList<TaskPrincipal>();
		}

		this.mAssignedToPrincipals.add(user);

		this.mState = TaskState.ASSIGNED;
	}

	public TaskPrincipal getClaimedBy() {
		return this.mClaimedByPrincipal;
	}

	public synchronized void setClaimedBy(TaskPrincipal user) {
		if (user != null) {
			this.mClaimedByPrincipal = user;
			this.mState = TaskState.CLAIMED;
		}
	}

	public synchronized void setCompletedBy(TaskPrincipal user) {
		if (user != null) {
			this.mCompletedByPrincipal = user;
			this.mState = TaskState.COMPLETED;
		}
	}

	public TaskPrincipal getCompletedBy() {
		return this.mCompletedByPrincipal;
	}

	public Calendar getCreateDate() {
		return mCreationDate;
	}

	public void setCreateDate(Calendar date) {
		// TODO Auto-generated method stub
		mCreationDate = date;
	}

    
	public Calendar getDeadline() {
        return mDeadline;
    }

    public void setDeadline(Calendar deadline) {
        mDeadline = deadline;
    }

    public String getDescription() {
		return this.mDescription;
	}

	public void setDescription(String desc) {
		// TODO Auto-generated method stub
		mDescription = desc;
	}

	public int getPriority() {
		return this.mPriority;
	}

	public void setPriority(int priority) {
		this.mPriority = priority;
	}

	public Task getTaskMeta() {
		return this.mTaskMetaData;
	}

	public String getExchangeId() {
		// TODO Auto-generated method stub
		return mExchangeId;
	}

	public void setExchangeId(String exchangeId) {
		// TODO Auto-generated method stub
		mExchangeId = exchangeId;
	}

	public void setId(Long id) {
		// TODO Auto-generated method stub
		mId = id;

	}

	public boolean isNew() {
		// TODO Auto-generated method stub
		return mNew;
	}

	public void setNew(boolean isNew) {
		mNew = isNew;
	}

	@Override
	public int hashCode() {
		final int PRIME = 31;
		int result = 1;
		result = PRIME * result + ((mId == null) ? 0 : mId.hashCode());
		return result;
	}

	public Set<RuntimeTaskTimer> getTaskTimers() {
		return mTimers;
	}

	public RuntimeTaskTimer getTaskTimer(Long timerId) {
		RuntimeTaskTimer taskTimer = null;
		Iterator<RuntimeTaskTimer> it = mTimers.iterator();
		while (it.hasNext()) {
			RuntimeTaskTimer rtt = it.next();
			if (timerId.equals(rtt.getId())) {
				taskTimer = rtt;
				break;
			}
		}
		return taskTimer;
	}

	public void addTaskTimer(RuntimeTaskTimer timer) {
		if (timer != null) {
			mTimers.add(timer);
		}
	}

	public void removeTaskTimer(RuntimeTaskTimer timer) {
		if (timer != null) {
			mTimers.remove(timer);
		}

	}

	public RuntimeVariables getRuntimeVariables() {
		// TODO Auto-generated method stub
		return mRuntimeVariables;
	}

	public void setRuntimeVariables(RuntimeVariables runtimeVars) {
		// TODO Auto-generated method stub
		mRuntimeVariables = runtimeVars;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		final DefaultRuntimeTask other = (DefaultRuntimeTask) obj;
		if (mId == null) {
			if (other.mId != null)
				return false;
		} else if (!mId.equals(other.mId))
			return false;
		return true;
	}

	public void init(EngineContext context) throws TaskException {
		// TODO Auto-generated method stub

		if (mTaskMetaData == null) {
			throw new TaskException(I18n
					.loc("WLM-6056: The Task model is NULL"));
		}

		try {
			mRuntimeVariables = new RuntimeVariablesImpl();
			mRuntimeVariables.declareVariable(RuntimeVariables.TASK_INPUT_VAR,
					mInput.getInput());
            Message inputMsg = mTaskMetaData.getTask().getWSDLOperation().getInput().getMessage();
            //XPathUtil.setPartsAsVariables(RuntimeVariables.TASK_INPUT_VAR, mInput.getInput(), inputMsg, mRuntimeVariables);
            String partName = (String)  inputMsg.getParts().keySet().iterator().next();
            mRuntimeVariables.declareVariable(RuntimeVariables.TASK_INPUT_VAR + "." +partName ,
                    mInput.getInput());
            mRuntimeVariables.declareVariable(RuntimeVariables.TASK_ID,
                    mId.toString());            
		} catch (Exception e) {
			// TODO Auto-generated catch block
			throw new TaskException(e);
		}
        //Set owner variable if claimed
        if (mClaimedByPrincipal != null) {
            mRuntimeVariables.declareVariable(RuntimeVariables.TASK_INSTANCE_OWNER,
                    mClaimedByPrincipal.getName());
        }
        boolean newOutput = true;
		if (mOutput == null) {
			// set the output variable to the empty instance
			Element outputEl = Util.getOutputInstance(mTaskMetaData, context);  
            Element outputCloneEl = null;
            //Clone the node for concurrent access
            if (outputEl != null) {
                synchronized (outputEl) {
                    outputCloneEl = (Element) outputEl.cloneNode(true);
                }
                mRuntimeVariables.declareVariable(RuntimeVariables.TASK_OUTPUT_VAR,
                        outputCloneEl);
                TaskOutput taskOutput = TaskModelFactory.getInstance().createTaskOutput(
                        outputCloneEl);
                mOutput = taskOutput;                
            }

		} else {
			mRuntimeVariables.declareVariable(RuntimeVariables.TASK_OUTPUT_VAR,
					mOutput.getOutput());
            newOutput = false;
		}
		//Set Ldap context
        setLDAPConfig(context.getLDAPConfig());
		// initialize variables in the init section
		JXPathContext jxpathContext = XPathUtil.newJXPathContextFromTask(this,
				new XPathUtil.DOMFactory());

		setJXpathContext(jxpathContext);
        
		Init init = mTaskMetaData.getInit();
		if (init != null) {
            List<ChangeVariables> variableInits = init.getChangeVariables();
            try {
                if (variableInits != null && variableInits.size() > 0) {
                    for (int i = 0; i < variableInits.size(); i++) {
                        ChangeVariables varInit = variableInits.get(i);
                        List<Copy> copies = varInit.getCopies();
                        if (copies != null && copies.size() > 0) {
                            for (int j = 0; j < copies.size(); j++) {
                                Copy copy = copies.get(j);
                                VariableCopy varCopy = new CopyUnitImpl(copy,
                                        jxpathContext);
                                if (!newOutput) {
                                    // If the output is not new, don't do any
                                    // copy that will overwrite it
                                    ExpressionInfo expression = ((CopyUnitImpl) varCopy)
                                            .getToExpression(copy.getToExpr());
                                    if (expression.getVariableName().equals(
                                            TASK_OUTPUT_VAR)) {
                                        continue;
                                    }
                                }
                               varCopy.doCopy();
                            }
                        }

                    }
                }
            } catch (Exception e) {
                // TODO Auto-generated catch block
                throw new TaskException(e);
            }
        }
        
        //Do search keywords, similar to Init
        if (mKeywords == null) {
        List<Keyword>  keywordsModel = mTaskMetaData.getKeywords();
        if (keywordsModel != null && keywordsModel.size() > 0) {
            //Build keywords into a string
            StringBuffer buffer = new StringBuffer ();
            for (Keyword key : keywordsModel) {
                String keyContent = key.getContent(mJxpathContext);
                if (keyContent != null && keyContent.trim().length() >0) {
                    buffer.append(KEYWORD_S);
                    buffer.append(keyContent.trim());
                    buffer.append(KEYWORD_E);
                }
            }
            mKeywords = buffer.toString();
        } else {
            mKeywords = null;
        }
        }

		// Set priority and title
		if (mPriority == 0) {
            int parsedPriority = mTaskMetaData.getPriority(mJxpathContext);
			if (parsedPriority != 0) {
					mPriority = parsedPriority;
			} else {
				mPriority = DEFAULT_PRIORITY;
			}
		}
        
		if (mDescription == null) {
            String parsedTitle = mTaskMetaData.getTitle(mJxpathContext);
            if (parsedTitle.length() > 0) {
					mDescription = parsedTitle;			
			} else {
				String title = mTaskMetaData.getName();
				mDescription = "This is a task for " + title;
			}
		}
	}

    public JXPathContext getJXpathContext() {
        // TODO Auto-generated method stub
      if (mJxpathContext == null) {
          mJxpathContext =XPathUtil.newJXPathContextFromTask(this,
                  new XPathUtil.DOMFactory());
        }
      return mJxpathContext;
    }

    public void setJXpathContext(JXPathContext jxpathContext) {
        // TODO Auto-generated method stub
        mJxpathContext = jxpathContext;
    }
    
    public void setLDAPConfig (LDAPConfig ldapConfig) {
        mLDAPConfig = ldapConfig;
    }

    public LDAPConfig getLDAPConfig() {
        // TODO Auto-generated method stub
        return mLDAPConfig;
    }

    public Calendar getEndDate() {
        // TODO Auto-generated method stub
        return mEndDate;
    }

    public void setEndDate(Calendar date) {
        // TODO Auto-generated method stub
        mEndDate = date;
    }

    public String getKeywords() {
        // TODO Auto-generated method stub
        return this.mKeywords;
    }

    public void setKeywords(String keywords) {
        // TODO Auto-generated method stub
        this.mKeywords = keywords;
    }

}
